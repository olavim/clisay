//! Static nullability checking.

mod native;

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::middle::bind::Bindings;
use crate::middle::hir::{BinOp, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};

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

pub fn check(hir: &Hir, bindings: &Bindings) -> Result<(), anyhow::Error> {
    let sigs = Collector::collect(hir);
    let mut checker = Checker {
        hir,
        bindings,
        sigs: &sigs,
        locals: Vec::new(),
        frame_start: 0,
        current_type: None,
        narrowed_fields: HashSet::new(),
    };
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

/// The static nullability of an expression.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Nullness {
    NonNull,
    Nullable,
    Null,
    Unknown,
}

/// A tracked binding in the current function frame.
struct Local {
    name: Symbol,
    declared_nullable: bool,
    mutable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    /// Whether a check has narrowed this binding to non-null on the current path.
    narrowed: bool,
    func: Option<HirId<HirStmt>>,
}

/// A place that can be narrowed to non-null.
enum Narrow {
    Local(usize),
    Field(Symbol),
}

/// A snapshot of flow facts.
struct Flow {
    assigned: Vec<bool>,
    narrowed: Vec<bool>,
    fields: HashSet<Symbol>,
}

struct Checker<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    locals: Vec<Local>,
    /// The start index in `locals` of the current function frame. Value reads only see
    /// bindings at or above this, so a closure does not read an enclosing local's flow state.
    frame_start: usize,
    /// The enclosing type while checking its methods, used to resolve `this` field nullability.
    current_type: Option<HirId<HirStmt>>,
    /// Immutable `this` fields narrowed to non-null on the current path.
    narrowed_fields: HashSet<Symbol>,
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
                self.locals.push(Local { name: decl.name, declared_nullable: false, mutable: false, assigned: true, narrowed: false, func: Some(*stmt) });
                self.function(decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(Some(*stmt), decl)?,
            // A trait has no runtime layout, so its `this` fields are not resolved here.
            HirStmt::Trait(decl) => self.type_decl(None, decl)?,
            HirStmt::Say(field) => self.say(field.name, field.nullable, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => if let Some(e) = opt { self.expr(e)?; },
            HirStmt::Throw(e) => { self.expr(e)?; },
            HirStmt::While(cond, body) => {
                self.expr(cond)?;
                let body_narrow = self.narrowings(cond, true);
                // Loop bodies may run zero times, so their assignments are not definite afterward.
                let pre = self.snapshot();
                self.apply_narrowings(&body_narrow);
                self.expr(body)?;
                self.restore(&pre);
            },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond)?;
                let then_narrow = self.narrowings(cond, true);
                let else_narrow = self.narrowings(cond, false);
                let pre = self.snapshot();
                self.apply_narrowings(&then_narrow);
                self.expr(then)?;
                let then_assigned = self.assigned_vec();
                self.restore(&pre);
                self.apply_narrowings(&else_narrow);
                if let Some(otherwise) = otherwise {
                    self.stmt(otherwise)?;
                }
                let else_assigned = self.assigned_vec();
                // Widen narrowing back at the join, then keep only assignments made on both paths.
                self.restore(&pre);
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
                        self.locals.push(Local { name, declared_nullable: true, mutable: false, assigned: true, narrowed: false, func: None });
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
        self.locals.push(Local { name, declared_nullable, mutable, assigned, narrowed: false, func: None });
        Ok(())
    }

    /// Checks a function/method/lambda body in a fresh frame holding its parameters.
    fn function(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in &decl.params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local { name, declared_nullable: param.nullable, mutable: param.mutable, assigned: true, narrowed: false, func: None });
        }
        self.expr(&decl.body)?;
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        Ok(())
    }

    fn type_decl(&mut self, type_id: Option<HirId<HirStmt>>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved = self.current_type;
        self.current_type = type_id;
        self.function_stmt(&decl.init)?;
        for method in &decl.methods {
            self.function_stmt(method)?;
        }
        self.current_type = saved;
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
            HirExpr::Index(target, member, _) => self.index(target, member)?,
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
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
        Ok(if local.declared_nullable && !local.narrowed { Nullness::Nullable } else { Nullness::NonNull })
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
                    // A reassignment resets narrowing: the slot is non-null only if the new value is.
                    self.locals[i].narrowed = matches!(nullness, Nullness::NonNull);
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
        let callee_nullness = self.expr(callee)?;
        self.require_value(callee_nullness, callee)?;
        Ok(Nullness::Unknown)
    }

    /// Checks if moving a value into a slot is allowed per its nullability. A nullable or null value into a
    /// non-null slot is an error. `Unknown` is always allowed and gets a runtime barrier.
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

    /// Member or data access `target.member` / `target[member]`.
    fn index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        // `this.field` resolves to the field's nullability
        if let Some(nullness) = self.this_field(target, member) {
            return Ok(nullness);
        }
        let target_nullness = self.expr(target)?;
        self.require_value(target_nullness, target)?;
        self.expr(member)?;
        Ok(Nullness::Unknown)
    }

    fn binary(&mut self, op: BinOp, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        match op {
            // Short-circuit operators narrow their left operand into the right operand.
            BinOp::And => {
                self.expr(l)?;
                let narrow = self.narrowings(l, true);
                let pre = self.snapshot();
                self.apply_narrowings(&narrow);
                self.expr(r)?;
                self.restore(&pre);
                Ok(Nullness::NonNull)
            },
            BinOp::Or => {
                self.expr(l)?;
                let narrow = self.narrowings(l, false);
                let pre = self.snapshot();
                self.apply_narrowings(&narrow);
                self.expr(r)?;
                self.restore(&pre);
                Ok(Nullness::NonNull)
            },
            // Equality is a boolean context; a possibly-null operand is fine.
            BinOp::Equal | BinOp::NotEqual => {
                self.expr(l)?;
                self.expr(r)?;
                Ok(Nullness::NonNull)
            },
            _ => {
                let ln = self.expr(l)?;
                self.require_value(ln, l)?;
                let rn = self.expr(r)?;
                self.require_value(rn, r)?;
                Ok(Nullness::NonNull)
            },
        }
    }

    fn unary(&mut self, op: UnOp, x: &HirId<HirExpr>) -> Result<Nullness, anyhow::Error> {
        let nullness = self.expr(x)?;
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            self.require_value(nullness, x)?;
        }
        Ok(Nullness::NonNull)
    }

    /// The nullability of `target.member` when it means `this.field`. Otherwise None.
    fn this_field(&self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Option<Nullness> {
        if !matches!(self.hir.get(target), HirExpr::This) {
            return None;
        }
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return None };
        let type_id = self.current_type?;
        let sym = self.hir.symbol_of(name)?;
        if self.narrowed_fields.contains(&sym) {
            return Some(Nullness::NonNull);
        }
        let layout = self.bindings.type_layout(&type_id);
        Some(if layout.is_nullable(sym) { Nullness::Nullable } else { Nullness::NonNull })
    }

    /// Rejects an operation that requires a value when the operand may be null.
    fn require_value(&self, nullness: Nullness, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !matches!(nullness, Nullness::Nullable | Nullness::Null) {
            return Ok(());
        }
        let msg = match self.hir.get(operand) {
            HirExpr::Identifier(name) => format!("'{}' may be null; narrow it before use", self.hir.text(*name)),
            _ => "Value may be null; narrow it before use".to_string(),
        };
        Err(self.error(msg, operand))
    }

    /// The places a condition narrows to non-null. `positive` selects the branch where the
    /// condition holds versus the branch where it fails.
    fn narrowings(&self, cond: &HirId<HirExpr>, positive: bool) -> Vec<Narrow> {
        match self.hir.get(cond) {
            // A bare truthiness test narrows in the truthy branch.
            HirExpr::Identifier(name) if positive => self.narrow_ident(*name),
            HirExpr::Index(target, member, _) if positive => self.narrow_field(target, member),
            // `x != null` narrows when true; `x == null` narrows when false.
            HirExpr::Binary(BinOp::NotEqual, l, r) if positive => self.narrow_null_compare(l, r),
            HirExpr::Binary(BinOp::Equal, l, r) if !positive => self.narrow_null_compare(l, r),
            // Conjunction narrows both sides when true. By De Morgan, disjunction narrows both when false.
            HirExpr::Binary(BinOp::And, l, r) if positive => {
                let mut narrow = self.narrowings(l, true);
                narrow.extend(self.narrowings(r, true));
                narrow
            },
            HirExpr::Binary(BinOp::Or, l, r) if !positive => {
                let mut narrow = self.narrowings(l, false);
                narrow.extend(self.narrowings(r, false));
                narrow
            },
            HirExpr::Unary(UnOp::Not, x) => self.narrowings(x, !positive),
            _ => Vec::new(),
        }
    }

    fn narrow_null_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<Narrow> {
        let place = if self.is_null(l) { r } else if self.is_null(r) { l } else { return Vec::new() };
        match self.hir.get(place) {
            HirExpr::Identifier(name) => self.narrow_ident(*name),
            HirExpr::Index(target, member, _) => self.narrow_field(target, member),
            _ => Vec::new(),
        }
    }

    fn narrow_ident(&self, name: Symbol) -> Vec<Narrow> {
        match self.frame_index_of(name) {
            Some(i) if self.locals[i].func.is_none() => vec![Narrow::Local(i)],
            _ => Vec::new(),
        }
    }

    /// An immutable `this` field narrows; a `mut` field could change between check and use.
    fn narrow_field(&self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Vec<Narrow> {
        if !matches!(self.hir.get(target), HirExpr::This) {
            return Vec::new();
        }
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return Vec::new() };
        let Some(type_id) = self.current_type else { return Vec::new() };
        let Some(sym) = self.hir.symbol_of(name) else { return Vec::new() };
        if self.bindings.type_layout(&type_id).is_mutable(sym) {
            return Vec::new();
        }
        vec![Narrow::Field(sym)]
    }

    fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
    }

    fn apply_narrowings(&mut self, narrowings: &[Narrow]) {
        for narrow in narrowings {
            match narrow {
                Narrow::Local(i) => self.locals[*i].narrowed = true,
                Narrow::Field(sym) => { self.narrowed_fields.insert(*sym); },
            }
        }
    }

    fn assigned_vec(&self) -> Vec<bool> {
        self.locals.iter().map(|l| l.assigned).collect()
    }

    fn snapshot(&self) -> Flow {
        Flow {
            assigned: self.locals.iter().map(|l| l.assigned).collect(),
            narrowed: self.locals.iter().map(|l| l.narrowed).collect(),
            fields: self.narrowed_fields.clone(),
        }
    }

    fn restore(&mut self, flow: &Flow) {
        for (local, (&assigned, &narrowed)) in self.locals.iter_mut().zip(flow.assigned.iter().zip(&flow.narrowed)) {
            local.assigned = assigned;
            local.narrowed = narrowed;
        }
        self.narrowed_fields = flow.fields.clone();
    }
}
