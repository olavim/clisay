use crate::compiler_error;
use crate::ast::{AstId, Expr, FnDecl, Literal, Operator};
use crate::core::value::Value;
use crate::middle::ir::Inst;
use crate::middle::resolve::{FnKind, Member, Place};

use super::Compiler;

/// How an index / property expression (`a.b`, `a[b]`) is being accessed.
#[derive(Clone, Copy)]
enum IndexOp {
    /// Read the value.
    Load,
    /// Assign `rhs`. `discarded` is true in statement position (the value should be dropped).
    Store { rhs: AstId<Expr>, discarded: bool },
}

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Block(stmts) => self.scoped_body(stmts, expr)?,
            Expr::Unary(op, operand) => self.unary_expression(op, operand)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Call(callee, args) => self.call_expression(callee, args)?,
            Expr::Index(target, member) => self.index(target, member, IndexOp::Load)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(_) => {
                let place = self.bindings.place(expr);
                self.emit_load(place, expr)?;
            },
            Expr::This | Expr::Super => self.emit(Inst::GetLocal(0), expr),
        };

        Ok(())
    }

    /// Compiles an expression in statement position, where its value is discarded.
    pub (super) fn expression_stmt(&mut self, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Block(stmts) => self.scoped_body(stmts, expr),
            // An assignment statement stores in discard context, so the store op itself drops the value.
            Expr::Binary(Operator::Assign(_), left, right) => self.compile_assign(left, right, true),
            // Any other expression leaves a value that the statement discards.
            _ => {
                self.expression(expr)?;
                self.emit(Inst::Pop, expr);
                Ok(())
            }
        }
    }

    fn unary_expression(&mut self, op: &Operator, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(Inst::from_operator(op), expr);
        Ok(())
    }

    /// Emits a read of `place`, pushing its value.
    fn emit_load(&mut self, place: Place, node: &AstId<Expr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => self.emit(Inst::GetLocal(slot), node),
            Place::Upvalue(idx) => self.emit(Inst::GetUpvalue(idx), node),
            Place::Field(id) => {
                self.emit(Inst::GetLocal(0), node);
                self.emit(Inst::GetPropertyId(id), node);
            },
            Place::Global(symbol) => {
                let name = self.gc.intern(self.ast.text(symbol));
                let idx = self.ir.add_constant(Value::from(name))?;
                self.emit(Inst::GetGlobal(idx), node);
            },
        }
        Ok(())
    }

    /// Emits a store into `place`. The value to store is already on top of the
    /// stack. When `discarded` (statement position) the store also pops the value.
    fn emit_store(&mut self, place: Place, discarded: bool, node: &AstId<Expr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => {
                self.emit(if discarded { Inst::SetLocalPop(slot) } else { Inst::SetLocal(slot) }, node);
            },
            Place::Upvalue(idx) => {
                self.emit(if discarded { Inst::SetUpvaluePop(idx) } else { Inst::SetUpvalue(idx) }, node);
            },
            Place::Field(id) => {
                self.emit(Inst::GetLocal(0), node); // push `this` (the target)
                self.emit(if discarded { Inst::SetPropertyIdPop(id) } else { Inst::SetPropertyId(id) }, node);
            },
            Place::Global(symbol) => {
                let name = self.gc.intern(self.ast.text(symbol));
                let idx = self.ir.add_constant(Value::from(name))?;
                self.emit(Inst::SetGlobal(idx), node);
                if discarded { self.emit(Inst::Pop, node); }
            }
        }
        Ok(())
    }

    /// Compiles `lhs = rhs`. `discarded` is true in statement position, which lets
    /// the store pop its own value.
    fn compile_assign(&mut self, lhs: &AstId<Expr>, rhs: &AstId<Expr>, discarded: bool) -> Result<(), anyhow::Error> {
        match self.ast.get(lhs) {
            Expr::Identifier(_) => {
                let place = self.bindings.place(lhs);
                self.expression(rhs)?;
                self.emit_store(place, discarded, lhs)?;
                Ok(())
            },
            Expr::Index(obj, member) => {
                let (obj, member) = (*obj, *member);
                self.index(&obj, &member, IndexOp::Store { rhs: *rhs, discarded })
            },
            _ => compiler_error!(self, lhs, "Invalid assignment")
        }
    }

    fn binary_expression(&mut self, op: &Operator, left: &AstId<Expr>, right: &AstId<Expr>) -> Result<(), anyhow::Error> {
        // A value-context assignment: its result is used, so it is never discarded.
        if let Operator::Assign(_) = op {
            return self.compile_assign(left, right, false);
        }

        if let Operator::MemberAccess = op {
            return self.index(left, right, IndexOp::Load);
        }

        // Canonical lowering; `optimize` fuses `local <op> const` forms.
        self.expression(left)?;
        self.expression(right)?;
        self.emit(Inst::from_operator(op), right);
        Ok(())
    }

    fn index(&mut self, target: &AstId<Expr>, member_expr_id: &AstId<Expr>, op: IndexOp) -> Result<(), anyhow::Error> {
        if matches!(self.ast.get(target), Expr::This | Expr::Super) {
            match self.bindings.member(target) {
                Member::ById(member_id) => return self.index_member_by_id(target, member_id, op),
                Member::ByAccessor(accessor_id) => return self.index_member_by_accessor(target, accessor_id, member_expr_id, op),
                Member::SuperInit(_) => unreachable!("super-call resolution on a member access"),
            }
        }

        match op {
            IndexOp::Load => {
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(Inst::GetIndex, target);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(Inst::SetIndex, target);
                if discarded {
                    self.emit(Inst::Pop, target);
                }
            }
        }
        Ok(())
    }

    fn index_member_by_id(&mut self, target_expr: &AstId<Expr>, member_id: u8, op: IndexOp) -> Result<(), anyhow::Error> {
        match op {
            IndexOp::Load => {
                self.expression(target_expr)?;
                self.emit(Inst::GetPropertyId(member_id), target_expr);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(target_expr)?;
                self.emit(if discarded { Inst::SetPropertyIdPop(member_id) } else { Inst::SetPropertyId(member_id) }, target_expr);
            }
        }
        Ok(())
    }

    fn index_member_by_accessor(&mut self, target_expr: &AstId<Expr>, accessor_id: u8, member_expr_id: &AstId<Expr>, op: IndexOp) -> Result<(), anyhow::Error> {
        self.expression(target_expr)?;
        self.emit(Inst::GetPropertyId(accessor_id), target_expr);

        self.expression(member_expr_id)?;
        let arg_count = match op {
            IndexOp::Load => 1,
            IndexOp::Store { rhs, .. } => {
                self.expression(&rhs)?;
                2
            }
        };
        self.emit(Inst::Call(arg_count), target_expr);

        // A setter call returns a value; drop it in statement position.
        if let IndexOp::Store { discarded: true, .. } = op {
            self.emit(Inst::Pop, target_expr);
        }
        Ok(())
    }

    fn call_expression(&mut self, callee: &AstId<Expr>, args: &Vec<AstId<Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(callee) {
            Expr::Super => {
                let Member::SuperInit(member_id) = self.bindings.member(callee) else {
                    unreachable!("super call resolved to a non-init member");
                };
                self.emit(Inst::GetLocal(0), callee);
                self.emit(Inst::GetPropertyId(member_id), callee);
            },
            _ => { self.expression(callee)? }
        };

        for arg in args {
            self.expression(arg)?;
        }
        self.emit(Inst::Call(args.len() as u8), callee);

        Ok(())
    }

    fn lambda(&mut self, expr: &AstId<Expr>, decl: &FnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, decl, kind)?;
        self.emit(Inst::PushClosure(const_idx), expr);
        return Ok(());
    }

    fn literal(&mut self, expr: &AstId<Expr>, literal: &Literal) -> Result<(), anyhow::Error> {
        match literal {
            Literal::Number(num) => {
                let idx = self.ir.add_constant(Value::from(*num))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            Literal::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.ir.add_constant(Value::from(str))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            Literal::Null => { self.emit(Inst::PushNull, expr); },
            Literal::Boolean(true) => { self.emit(Inst::PushTrue, expr); },
            Literal::Boolean(false) => { self.emit(Inst::PushFalse, expr); },
            Literal::Array(elements) => {
                for element in elements {
                    self.expression(element)?;
                }
                self.emit(Inst::Array(elements.len() as u8), expr);
            },
            Literal::Lambda(decl) => self.lambda(expr, decl, FnKind::Function)?
        };

        return Ok(());
    }
}
