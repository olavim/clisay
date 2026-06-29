use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::hir::{BinOp, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatcher, HirMatchElem, Symbol, UnOp};
use crate::middle::ir::Inst;
use crate::middle::bind::{FnKind, Member, Place};

use super::Compiler;

/// How an index / property expression (`a.b`, `a[b]`) is being accessed.
#[derive(Clone, Copy)]
enum IndexOp {
    /// Read the value.
    Load,
    /// Assign `rhs`. `discarded` is true in statement position (the value should be dropped).
    Store { rhs: HirId<HirExpr>, discarded: bool },
}

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr)?,
            HirExpr::Unary(op, operand) => self.unary_expression(*op, operand)?,
            HirExpr::Binary(op, left, right) => self.binary_expression(*op, left, right)?,
            HirExpr::Assign(left, right) => self.compile_assign(left, right, false)?,
            HirExpr::Call(callee, args) => self.call_expression(callee, args)?,
            HirExpr::Index(target, member, is_dot) => self.index(target, member, *is_dot, IndexOp::Load)?,
            HirExpr::Literal(lit) => self.literal(expr, lit)?,
            HirExpr::Identifier(_) => {
                let place = self.bindings.place(expr);
                self.emit_load(place, expr)?;
            },
            HirExpr::Is(target, name) => {
                self.expression(target)?;
                let name_ref = self.gc.intern(self.hir.text(*name));
                let idx = self.ir.add_constant(Value::from(name_ref))?;
                self.emit(Inst::Is(idx), expr);
            },
            HirExpr::Construct(callee, args, brace) => self.construct_expression(expr, callee, args, brace)?,
            HirExpr::Has(left, matcher) => self.compile_has(left, matcher, expr)?,
            HirExpr::This => self.emit(Inst::LoadLocal(0), expr),
            HirExpr::Coalesce(left, right) => self.coalesce(left, right)?,
            HirExpr::SafeAccess(target, member, is_dot) => self.safe_access(target, member, *is_dot)?,
            // `!` leaves its operand's value; the barrier below guards it when the check pass flagged it.
            HirExpr::Assert(operand) => self.expression(operand)?,
            HirExpr::MatchBind(..) => compiler_error!(self, expr, "`<-` match-bind is not yet supported"),
        };

        // A value that the check pass marked as an `unknown` crossing into a non-null slot is guarded.
        if self.barriers.has(expr) {
            self.emit(Inst::AssertNonNull, expr);
        }
        Ok(())
    }

    /// Compiles `a ?? b`: yield `a` when it is non-null, else `b`. The fallback is evaluated only
    /// when `a` is null.
    fn coalesce(&mut self, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        self.emit(Inst::JumpIfNotNullOrPop(end), left);
        self.expression(right)?;
        self.ir.bind(end);
        Ok(())
    }

    /// Compiles `a?.b` / `a?[i]`: yield null when `a` is null, else the member access. The access
    /// runs only when `a` is non-null.
    fn safe_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(target)?;
        self.emit(Inst::JumpIfNull(end), target);
        self.expression(member)?;
        self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
        self.ir.bind(end);
        Ok(())
    }

    /// Compiles an expression in statement position, where its value is discarded.
    pub (super) fn expression_stmt(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr),
            // An assignment statement stores in discard context, so the store op itself drops the value.
            HirExpr::Assign(left, right) => self.compile_assign(left, right, true),
            // Any other expression leaves a value that the statement discards.
            _ => {
                self.expression(expr)?;
                self.emit(Inst::Pop, expr);
                Ok(())
            }
        }
    }

    fn unary_expression(&mut self, op: UnOp, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(unop_inst(op), expr);
        Ok(())
    }

    /// Compiles a brace construction. Source order: push the type, the `init` args, then the
    /// brace values; the `Construct` op allocates, sets the brace fields, and runs `init`.
    fn construct_expression(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], brace: &[(Symbol, HirId<HirExpr>)]) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
        for arg in args {
            self.expression(arg)?;
        }
        for (_, value) in brace {
            self.expression(value)?;
        }
        let field_ids = self.bindings.construct_fields(expr).to_vec();
        let fields_idx = self.ir.add_construct_fields(field_ids)?;
        self.emit(Inst::Construct(fields_idx, args.len() as u8), expr);
        Ok(())
    }

    /// Emits a read of `place`, pushing its value.
    fn emit_load(&mut self, place: Place, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => self.emit(Inst::LoadLocal(slot), node),
            Place::Upvalue(idx) => self.emit(Inst::LoadUpvalue(idx), node),
            Place::Field(id) => {
                self.emit(Inst::LoadLocal(0), node);
                self.emit(Inst::GetField(id), node);
            },
            Place::Global(symbol) => {
                let name = self.gc.intern(self.hir.text(symbol));
                let idx = self.ir.add_constant(Value::from(name))?;
                self.emit(Inst::LoadGlobal(idx), node);
            },
        }
        Ok(())
    }

    /// Emits a store into `place`. The value to store is already on top of the
    /// stack. When `discarded` (statement position) the store also pops the value.
    fn emit_store(&mut self, place: Place, discarded: bool, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => {
                self.emit(if discarded { Inst::StoreLocalPop(slot) } else { Inst::StoreLocal(slot) }, node);
            },
            Place::Upvalue(idx) => {
                self.emit(if discarded { Inst::StoreUpvaluePop(idx) } else { Inst::StoreUpvalue(idx) }, node);
            },
            Place::Field(id) => {
                self.emit(Inst::LoadLocal(0), node); // push `this` (the target)
                self.emit(if discarded { Inst::SetFieldPop(id) } else { Inst::SetField(id) }, node);
            },
            Place::Global(_) => unreachable!("assignment to a global is rejected during resolution"),
        }
        Ok(())
    }

    /// Compiles `lhs = rhs`. `discarded` is true in statement position, which lets
    /// the store pop its own value.
    fn compile_assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>, discarded: bool) -> Result<(), anyhow::Error> {
        match self.hir.get(lhs) {
            HirExpr::Identifier(_) => {
                let place = self.bindings.place(lhs);
                self.expression(rhs)?;
                self.emit_store(place, discarded, lhs)?;
                Ok(())
            },
            HirExpr::Index(obj, member, is_dot) => {
                let (obj, member, is_dot) = (*obj, *member, *is_dot);
                self.index(&obj, &member, is_dot, IndexOp::Store { rhs: *rhs, discarded })
            },
            _ => compiler_error!(self, lhs, "Invalid assignment")
        }
    }

    fn binary_expression(&mut self, op: BinOp, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // `&&`/`||` short-circuit and yield an operand, so they compile to a
        // conditional jump rather than a binary op.
        if let BinOp::And | BinOp::Or = op {
            return self.logical_expression(op, left, right);
        }

        // Canonical lowering; `optimize` fuses `local <op> const` forms.
        self.expression(left)?;
        self.expression(right)?;
        self.emit(binop_inst(op), right);
        Ok(())
    }

    /// Compiles `a && b` / `a || b` with short-circuit.
    fn logical_expression(&mut self, op: BinOp, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        let short_circuit = match op {
            BinOp::And => Inst::JumpIfFalseOrPop(end),
            BinOp::Or => Inst::JumpIfTrueOrPop(end),
            _ => unreachable!("logical_expression called with a non-logical operator"),
        };
        self.emit(short_circuit, left);
        self.expression(right)?;
        self.ir.bind(end);
        Ok(())
    }

    fn compile_has(&mut self, left: &HirId<HirExpr>, matcher: &HirMatcher, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(left)?;
        self.compile_matcher_test(matcher, node)
    }

    fn compile_matcher_test(&mut self, m: &HirMatcher, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match m {
            HirMatcher::Wildcard => {
                self.emit(Inst::Pop, node);
                self.emit(Inst::PushTrue, node);
                Ok(())
            },
            HirMatcher::Literal(lit) => {
                self.literal(node, lit)?;
                self.emit(Inst::Equal, node);
                Ok(())
            },
            HirMatcher::Type { nominal, name, shape } => self.compile_type_test(*nominal, *name, shape, node),
            HirMatcher::Shape(fields) => self.compile_test_and(fields.len(), node, &|c, i, n| {
                c.compile_shape_field(&fields[i].key, &fields[i].value, n)
            }),
            HirMatcher::Array(elements) => self.compile_array_test(elements, node),
            HirMatcher::And(parts) => self.compile_test_and(parts.len(), node, &|c, i, n| c.compile_matcher_test(&parts[i], n)),
            HirMatcher::Or(parts) => self.compile_test_or(parts.len(), node, &|c, i, n| c.compile_matcher_test(&parts[i], n)),
            HirMatcher::Binder(_) | HirMatcher::As(..) => compiler_error!(self, node, "a `has` matcher cannot bind"),
        }
    }

    fn compile_type_test(&mut self, nominal: bool, name: Symbol, shape: &Option<Box<HirMatcher>>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match shape {
            None => self.compile_type_atom(nominal, name, node),
            Some(shape) => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => c.compile_type_atom(nominal, name, n),
                _ => c.compile_matcher_test(shape, n),
            }),
        }
    }

    fn compile_type_atom(&mut self, nominal: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if nominal {
            let name_ref = self.gc.intern(self.hir.text(name));
            let idx = self.ir.add_constant(Value::from(name_ref))?;
            self.emit(Inst::Is(idx), node);
            return Ok(());
        }
        let members = match self.bindings.surface(name) {
            Some(members) => members.to_vec(),
            None => compiler_error!(self, node, "'{}' is not a type or trait", self.hir.text(name)),
        };
        self.compile_test_and(members.len(), node, &|c, i, n| {
            let idx = c.has_member_constant(members[i])?;
            c.emit(Inst::HasMember(idx), n);
            Ok(())
        })
    }

    fn compile_array_test(&mut self, elements: &[HirMatchElem], node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let len = elements.len();
        self.compile_test_and(len + 1, node, &|c, i, n| {
            if i == 0 {
                c.emit(Inst::ArrayLen, n);
                let idx = c.ir.add_constant(Value::from(len as f64))?;
                c.emit(Inst::PushConstant(idx), n);
                c.emit(Inst::Equal, n);
                return Ok(());
            }
            let HirMatchElem::Elem(elem) = &elements[i - 1] else {
                compiler_error!(c, n, "a `has` array shape cannot use a rest `..`");
            };
            let idx = c.ir.add_constant(Value::from((i - 1) as f64))?;
            c.emit(Inst::PushConstant(idx), n);
            c.emit(Inst::GetIndex, n);
            c.compile_matcher_test(elem, n)
        })
    }

    fn compile_test_and(&mut self, count: usize, node: &HirId<HirExpr>, compile_test: &dyn Fn(&mut Self, usize, &HirId<HirExpr>) -> Result<(), anyhow::Error>) -> Result<(), anyhow::Error> {
        // An empty AND holds for any receiver, so drop it and yield true.
        if count == 0 {
            self.emit(Inst::Pop, node);
            self.emit(Inst::PushTrue, node);
            return Ok(());
        }
        if count == 1 {
            return compile_test(self, 0, node);
        }
        let fail = self.ir.new_label();
        let end = self.ir.new_label();
        // Each test but the last runs on a duplicate so the receiver survives for the next.
        for i in 0..count - 1 {
            self.emit(Inst::Dup, node);
            compile_test(self, i, node)?;
            self.emit(Inst::JumpIfFalse(fail), node);
        }
        compile_test(self, count - 1, node)?;
        self.emit(Inst::Jump(end), node);
        self.ir.bind(fail);
        self.emit(Inst::Pop, node); // a test failed: drop the surviving receiver, yield false
        self.emit(Inst::PushFalse, node);
        self.ir.bind(end);
        Ok(())
    }

    fn compile_test_or(&mut self, count: usize, node: &HirId<HirExpr>, compile_test: &dyn Fn(&mut Self, usize, &HirId<HirExpr>) -> Result<(), anyhow::Error>) -> Result<(), anyhow::Error> {
        if count == 1 {
            return compile_test(self, 0, node);
        }
        let end = self.ir.new_label();
        for i in 0..count {
            let last = i == count - 1;
            let next = self.ir.new_label();
            // Every alternative but the last keeps a copy so a later one can retry the receiver.
            if !last {
                self.emit(Inst::Dup, node);
            }
            compile_test(self, i, node)?;
            self.emit(Inst::JumpIfFalse(next), node);
            if !last {
                self.emit(Inst::Pop, node); // matched: drop the surviving receiver
            }
            self.emit(Inst::PushTrue, node);
            self.emit(Inst::Jump(end), node);
            self.ir.bind(next);
        }
        // Reached only when the last alternative failed. Its receiver was already consumed.
        self.emit(Inst::PushFalse, node);
        self.ir.bind(end);
        Ok(())
    }

    fn compile_shape_field(&mut self, key: &HirLiteral, value: &HirMatcher, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let key_idx = self.has_spec_key_constant(key)?;
        match value {
            HirMatcher::Wildcard => {
                self.emit(Inst::HasMember(key_idx), node);
                Ok(())
            },
            HirMatcher::Literal(HirLiteral::Null) => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => { c.emit(Inst::HasMember(key_idx), n); Ok(()) },
                _ => {
                    c.emit(Inst::GetIndexOrNull(key_idx), n);
                    c.emit(Inst::PushNull, n);
                    c.emit(Inst::Equal, n);
                    Ok(())
                },
            }),
            HirMatcher::Literal(lit) => {
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.literal(node, lit)?;
                self.emit(Inst::Equal, node);
                Ok(())
            },
            nested => {
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.compile_matcher_test(nested, node)
            },
        }
    }

    /// Interns a member name (a string key) into the constant pool, returning its index.
    fn has_member_constant(&mut self, name: Symbol) -> Result<u8, anyhow::Error> {
        let name_ref = self.gc.intern(self.hir.text(name));
        self.ir.add_constant(Value::from(name_ref))
    }

    /// Pools a scalar-literal `has` key as its runtime value, returning the constant index.
    fn has_spec_key_constant(&mut self, key: &HirLiteral) -> Result<u8, anyhow::Error> {
        let value = match key {
            HirLiteral::String(s) => Value::from(self.gc.intern(s)),
            HirLiteral::Number(n) => Value::from(*n),
            HirLiteral::Boolean(b) => Value::from(*b),
            HirLiteral::Null => Value::NULL,
            _ => unreachable!("a `has` key is a scalar literal"),
        };
        self.ir.add_constant(value)
    }

    fn index(&mut self, target: &HirId<HirExpr>, member_expr_id: &HirId<HirExpr>, is_dot: bool, op: IndexOp) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(target), HirExpr::This) {
            let Member::ById(member_id) = self.bindings.member(target);
            return self.index_member_by_id(target, member_id, op);
        }

        // `.name` (member, `is_dot`) and `[expr]` (data) use the same stack protocol
        // but distinct opcodes, so the VM can route the dynamic-boundary `dict` to its
        // method surface (`.`) vs its keyed data (`[]`).
        match op {
            IndexOp::Load => {
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(if is_dot { Inst::SetProperty } else { Inst::SetIndex }, target);
                if discarded {
                    self.emit(Inst::Pop, target);
                }
            }
        }
        Ok(())
    }

    fn index_member_by_id(&mut self, target_expr: &HirId<HirExpr>, member_id: u8, op: IndexOp) -> Result<(), anyhow::Error> {
        match op {
            IndexOp::Load => {
                self.expression(target_expr)?;
                self.emit(Inst::GetField(member_id), target_expr);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(target_expr)?;
                self.emit(if discarded { Inst::SetFieldPop(member_id) } else { Inst::SetField(member_id) }, target_expr);
            }
        }
        Ok(())
    }

    fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &Vec<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        // Fuse `recv.name(args)` into a single INVOKE when the receiver is an
        // arbitrary expression (not `this`/`super`, which have their own member
        // resolution) and the member is a literal name.
        if let Some((target, name)) = self.as_method_invoke(callee) {
            self.expression(&target)?;
            for arg in args {
                self.expression(arg)?;
            }
            let name_ref = self.gc.intern(name);
            let idx = self.ir.add_constant(Value::from(name_ref))?;
            self.emit(Inst::Invoke(idx, args.len() as u8), callee);
            return Ok(());
        }

        self.expression(callee)?;

        for arg in args {
            self.expression(arg)?;
        }
        self.emit(Inst::Call(args.len() as u8), callee);

        Ok(())
    }

    /// If `callee` is `recv.name` where `recv` is not `this` and `name` is
    /// a literal, returns the receiver expression and the member name.
    fn as_method_invoke(&self, callee: &HirId<HirExpr>) -> Option<(HirId<HirExpr>, String)> {
        let HirExpr::Index(target, member, is_dot) = self.hir.get(callee) else { return None };
        // Only `recv.name(args)` is a method invoke.
        if !is_dot {
            return None;
        }
        if matches!(self.hir.get(target), HirExpr::This) {
            return None;
        }
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return None };
        Some((*target, name.clone()))
    }

    fn lambda(&mut self, expr: &HirId<HirExpr>, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, decl, kind)?;
        self.emit(Inst::PushClosure(const_idx), expr);
        return Ok(());
    }

    fn literal(&mut self, expr: &HirId<HirExpr>, literal: &HirLiteral) -> Result<(), anyhow::Error> {
        match literal {
            HirLiteral::Number(num) => {
                let idx = self.ir.add_constant(Value::from(*num))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            HirLiteral::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.ir.add_constant(Value::from(str))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            HirLiteral::Null => { self.emit(Inst::PushNull, expr); },
            HirLiteral::Boolean(true) => { self.emit(Inst::PushTrue, expr); },
            HirLiteral::Boolean(false) => { self.emit(Inst::PushFalse, expr); },
            HirLiteral::Array(elements) => {
                for element in elements {
                    self.expression(element)?;
                }
                self.emit(Inst::Array(elements.len() as u8), expr);
            },
            HirLiteral::Dict(pairs) => {
                for (key, value) in pairs {
                    self.expression(key)?;
                    self.expression(value)?;
                }
                self.emit(Inst::Dict(pairs.len() as u8), expr);
            },
            HirLiteral::Lambda(decl) => self.lambda(expr, decl, FnKind::Function)?
        };

        return Ok(());
    }
}

fn binop_inst(op: BinOp) -> Inst {
    match op {
        BinOp::Add => Inst::Add,
        BinOp::Subtract => Inst::Subtract,
        BinOp::Multiply => Inst::Multiply,
        BinOp::Divide => Inst::Divide,
        BinOp::LeftShift => Inst::LeftShift,
        BinOp::RightShift => Inst::RightShift,
        BinOp::LessThan => Inst::LessThan,
        BinOp::LessThanEqual => Inst::LessThanEqual,
        BinOp::GreaterThan => Inst::GreaterThan,
        BinOp::GreaterThanEqual => Inst::GreaterThanEqual,
        BinOp::Equal => Inst::Equal,
        BinOp::NotEqual => Inst::NotEqual,
        BinOp::And | BinOp::Or => unreachable!("logical ops compile to short-circuit branches"),
        BinOp::BitAnd => Inst::BitAnd,
        BinOp::BitOr => Inst::BitOr,
        BinOp::BitXor => Inst::BitXor,
    }
}

fn unop_inst(op: UnOp) -> Inst {
    match op {
        UnOp::Negate => Inst::Negate,
        UnOp::BitNot => Inst::BitNot,
        UnOp::Not => Inst::Not,
    }
}
