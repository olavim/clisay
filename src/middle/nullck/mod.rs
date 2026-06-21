//! Static nullability checking.

mod native;

use std::collections::HashMap;

use anyhow::anyhow;

use crate::middle::bind::Bindings;
use crate::middle::hir::{Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol};

/// A function's declared nullability signature: per-parameter nullability and return shape.
pub struct FnSig {
    /// Per-parameter declared nullability. Argument conformance reads this in a later step.
    #[allow(dead_code)]
    pub params: Vec<bool>,
    pub ret: ReturnShape,
}

impl FnSig {
    fn of(decl: &HirFnDecl) -> FnSig {
        FnSig { params: decl.params.iter().map(|p| p.nullable).collect(), ret: decl.ret }
    }
}

#[derive(Default)]
pub struct Signatures {
    pub fns: HashMap<HirId<HirStmt>, FnSig>,
}

pub fn check(hir: &Hir, _bindings: &Bindings) -> Result<(), anyhow::Error> {
    let sigs = Collector::collect(hir);
    let mut checker = Checker { hir, sigs: &sigs, locals: Vec::new(), frame_start: 0 };
    checker.stmt(&hir.get_root())
}

struct Collector<'a> {
    hir: &'a Hir,
    sigs: Signatures,
}

impl<'a> Collector<'a> {
    fn collect(hir: &Hir) -> Signatures {
        let mut collector = Collector { hir, sigs: Signatures::default() };
        collector.stmt(&hir.get_root());
        collector.sigs
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                self.sigs.fns.insert(*stmt, FnSig::of(decl));
                self.expr(&decl.body);
            },
            // A type's methods and initializer are themselves `Fn` statements.
            HirStmt::Type(decl) | HirStmt::Trait(decl) => {
                self.stmt(&decl.init);
                for method in &decl.methods {
                    self.stmt(method);
                }
            },
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => self.expr(e),
            HirStmt::Return(opt) => if let Some(e) = opt { self.expr(e); },
            HirStmt::While(cond, body) => { self.expr(cond); self.expr(body); },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond);
                self.expr(then);
                if let Some(otherwise) = otherwise { self.stmt(otherwise); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.expr(body);
                if let Some(catch) = catch { self.expr(&catch.body); }
                if let Some(finally) = finally { self.expr(finally); }
            },
            HirStmt::Say(field) => if let Some(value) = field.value { self.expr(&value); },
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => for s in stmts { self.stmt(s); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) => self.expr(x),
            HirExpr::Binary(_, l, r) | HirExpr::Assign(l, r) | HirExpr::Coalesce(l, r)
            | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => { self.expr(l); self.expr(r); },
            HirExpr::Call(callee, args) => {
                self.expr(callee);
                for a in args { self.expr(a); }
            },
            HirExpr::Construct(callee, args, brace) => {
                self.expr(callee);
                for a in args { self.expr(a); }
                for (_, v) in brace { self.expr(v); }
            },
            HirExpr::Literal(lit) => self.literal(lit),
            HirExpr::Identifier(_) | HirExpr::This => {},
        }
    }

    fn literal(&mut self, lit: &HirLiteral) {
        match lit {
            HirLiteral::Array(elems) => for e in elems { self.expr(e); },
            HirLiteral::Dict(pairs) => for (k, v) in pairs { self.expr(k); self.expr(v); },
            HirLiteral::Lambda(decl) => self.expr(&decl.body),
            _ => {},
        }
    }
}

/// The static nullability of an expression in the three-point lattice, plus the
/// compiler-internal `Unknown` for dynamic-boundary values.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Nullness {
    NonNull,
    Nullable,
    Null,
    Unknown,
}

/// A tracked binding in the current function frame. Functions are tracked too, so a direct
/// call can resolve its callee's return shape.
struct Local {
    name: Symbol,
    declared_nullable: bool,
    mutable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    /// The declaration node when this name binds a function, else `None`.
    func: Option<HirId<HirStmt>>,
}

/// Phase two: a forward dataflow over each function body. It checks lattice conformance into
/// non-null slots, definite assignment before use, and immutable single-assignment.
struct Checker<'a> {
    hir: &'a Hir,
    sigs: &'a Signatures,
    locals: Vec<Local>,
    /// The start index in `locals` of the current function frame. Value reads only see
    /// bindings at or above this, so a closure does not read an enclosing local's flow state.
    frame_start: usize,
}

impl<'a> Checker<'a> {
    fn error<T>(&self, msg: String, node: &HirId<T>) -> anyhow::Error {
        anyhow!("{}\n\tat {}", msg, self.hir.pos(node))
    }

    fn ident_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.hir.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("parameter is an identifier"),
        }
    }

    /// The index in `locals` of a value/function binding visible for a value read: the nearest
    /// match within the current frame.
    fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.locals[self.frame_start..].iter().rposition(|l| l.name == name)
            .map(|i| self.frame_start + i)
    }

    /// The nearest binding of `name` across all frames. Functions resolve across frames so a
    /// nested body can call an enclosing function.
    fn func_of(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        self.locals.iter().rev().find(|l| l.name == name).and_then(|l| l.func)
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                // Register the name first so the body may call itself.
                self.locals.push(Local { name: decl.name, declared_nullable: false, mutable: false, assigned: true, func: Some(*stmt) });
                self.function(decl)?;
            },
            HirStmt::Type(decl) | HirStmt::Trait(decl) => self.type_decl(decl)?,
            HirStmt::Say(field) => self.say(field.name, field.nullable, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => if let Some(e) = opt { self.expr(e)?; },
            HirStmt::Throw(e) => { self.expr(e)?; },
            HirStmt::While(cond, body) => {
                self.expr(cond)?;
                // The body may run zero times, so its assignments are not definite afterward.
                let pre = self.snapshot_locals();
                self.expr(body)?;
                self.restore_locals(&pre);
            },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond)?;
                let pre = self.snapshot_locals();
                self.expr(then)?;
                let then_assigned = self.snapshot_locals();
                self.restore_locals(&pre);
                if let Some(otherwise) = otherwise {
                    self.stmt(otherwise)?;
                }
                let else_assigned = self.snapshot_locals();
                // A binding is definitely assigned after the `if` only if both paths assign it.
                for i in 0..self.locals.len() {
                    self.locals[i].assigned = then_assigned[i] && else_assigned[i];
                }
            },
            HirStmt::Try(body, catch, finally) => {
                self.expr(body)?;
                if let Some(catch) = catch {
                    let mark = self.locals.len();
                    if let Some(param) = catch.param {
                        // The thrown value is arbitrary, so the catch parameter is nullable.
                        let name = self.ident_sym(&param);
                        self.locals.push(Local { name, declared_nullable: true, mutable: false, assigned: true, func: None });
                    }
                    self.expr(&catch.body)?;
                    self.locals.truncate(mark);
                }
                if let Some(finally) = finally { self.expr(finally)?; }
            },
        }
        Ok(())
    }

    fn say(&mut self, name: Symbol, declared_nullable: bool, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let assigned = if let Some(value) = value {
            let nullness = self.expr(value)?;
            self.check_into_slot(nullness, declared_nullable, name, value)?;
            true
        } else {
            false
        };
        self.locals.push(Local { name, declared_nullable, mutable, assigned, func: None });
        Ok(())
    }

    /// Checks a function/method/lambda body in a fresh frame holding its parameters.
    fn function(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in &decl.params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local { name, declared_nullable: param.nullable, mutable: param.mutable, assigned: true, func: None });
        }
        self.expr(&decl.body)?;
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        Ok(())
    }

    fn type_decl(&mut self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        self.function_stmt(&decl.init)?;
        for method in &decl.methods {
            self.function_stmt(method)?;
        }
        Ok(())
    }

    fn function_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            self.function(decl)?;
        }
        Ok(())
    }

    /// Evaluates an expression, emitting diagnostics, and returns its static nullability.
    fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        Ok(match self.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => Nullness::Null,
            HirExpr::Literal(lit) => { self.literal_children(lit)?; Nullness::NonNull },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => Nullness::Unknown,
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => self.call(callee, args)?,
            HirExpr::Construct(callee, args, brace) => {
                self.expr(callee)?;
                for a in args { self.expr(a)?; }
                for (_, v) in brace { self.expr(v)?; }
                Nullness::Unknown
            },
            HirExpr::Index(target, member, _) => { self.expr(target)?; self.expr(member)?; Nullness::Unknown },
            HirExpr::Binary(_, l, r) => { self.expr(l)?; self.expr(r)?; Nullness::NonNull },
            HirExpr::Unary(_, x) => { self.expr(x)?; Nullness::NonNull },
            HirExpr::Is(x, _) => { self.expr(x)?; Nullness::NonNull },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                for s in stmts { self.stmt(s)?; }
                self.locals.truncate(mark);
                Nullness::Unknown
            },
            HirExpr::Coalesce(l, r) => { self.expr(l)?; self.expr(r)?; Nullness::Unknown },
            HirExpr::SafeAccess(target, member, _) => { self.expr(target)?; self.expr(member)?; Nullness::Unknown },
            HirExpr::Assert(x) => { self.expr(x)?; Nullness::NonNull },
        })
    }

    fn literal_children(&mut self, lit: &HirLiteral) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems { self.expr(e)?; },
            HirLiteral::Dict(pairs) => for (k, v) in pairs { self.expr(k)?; self.expr(v)?; },
            HirLiteral::Lambda(decl) => self.function(decl)?,
            _ => {},
        }
        Ok(())
    }

    fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // Captures, globals, types, and fields are not tracked as locals here.
            return Ok(Nullness::Unknown);
        };
        let local = &self.locals[i];
        if local.func.is_some() {
            return Ok(Nullness::Unknown);
        }
        if !local.assigned && !local.declared_nullable {
            return Err(self.error(format!("'{}' is used before it is assigned", self.hir.text(name)), expr));
        }
        Ok(if local.declared_nullable { Nullness::Nullable } else { Nullness::NonNull })
    }

    fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        let nullness = self.expr(rhs)?;
        if let HirExpr::Identifier(name) = self.hir.get(lhs) {
            let name = *name;
            if let Some(i) = self.frame_index_of(name) {
                if self.locals[i].func.is_none() {
                    let (mutable, assigned, declared_nullable) =
                        (self.locals[i].mutable, self.locals[i].assigned, self.locals[i].declared_nullable);
                    if !mutable && assigned {
                        return Err(self.error(format!("Cannot reassign immutable binding '{}'; declare it 'mut'", self.hir.text(name)), lhs));
                    }
                    self.check_into_slot(nullness, declared_nullable, name, lhs)?;
                    self.locals[i].assigned = true;
                }
            }
        }
        Ok(nullness)
    }

    fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<Nullness, anyhow::Error> {
        for a in args { self.expr(a)?; }
        if let HirExpr::Identifier(name) = self.hir.get(callee) {
            if let Some(id) = self.func_of(*name) {
                let ret = self.sigs.fns.get(&id).map(|s| s.ret);
                return Ok(match ret {
                    Some(ReturnShape::NonNull) => Nullness::NonNull,
                    Some(ReturnShape::Nullable) => Nullness::Nullable,
                    _ => Nullness::Unknown,
                });
            }
        }
        self.expr(callee)?;
        Ok(Nullness::Unknown)
    }

    /// Checks moving a value of nullability `nullness` into a slot. A nullable or null value into a
    /// non-null slot is an error. `Unknown` is allowed here and gets a runtime barrier later.
    fn check_into_slot(&self, nullness: Nullness, slot_nullable: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if slot_nullable {
            return Ok(());
        }
        match nullness {
            Nullness::Null => Err(self.error(format!("Cannot assign null to non-null binding '{}'", self.hir.text(name)), node)),
            Nullness::Nullable => Err(self.error(format!("Cannot assign a nullable value to non-null binding '{}'", self.hir.text(name)), node)),
            Nullness::NonNull | Nullness::Unknown => Ok(()),
        }
    }

    fn snapshot_locals(&self) -> Vec<bool> {
        self.locals.iter().map(|l| l.assigned).collect()
    }

    fn restore_locals(&mut self, snapshot: &[bool]) {
        for (local, &assigned) in self.locals.iter_mut().zip(snapshot) {
            local.assigned = assigned;
        }
    }
}
