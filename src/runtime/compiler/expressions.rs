use crate::compiler_error;
use crate::parser::{AstId, Expr, FnDecl, Literal, Operator};
use crate::runtime::objects::ObjString;
use crate::runtime::opcode;
use crate::runtime::value::Value;

use super::{Compiler, FnKind};

/// A resolved assignable binding that a bare identifier denotes.
#[derive(Clone, Copy)]
enum Place {
    Local(u8),
    Upvalue(u8),
    /// An implicit-`this` class field, by member id.
    Field(u8),
    /// A global, by name-constant index.
    Global(u8),
}

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
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Index(expr, id) => self.index(expr, id, IndexOp::Load)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(name) => {
                let name = self.gc.intern(name);
                let place = self.resolve_place(name)?;
                self.emit_load(place, expr);
            },
            Expr::This => self.this(expr)?,
            Expr::Super => self.super_(expr)?
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
                self.emit(opcode::POP, expr);
                Ok(())
            }
        }
    }

    fn this(&mut self, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            compiler_error!(self, expr, "Cannot use 'this' outside of a class method");
        }

        self.emit_operand(opcode::GET_LOCAL, 0, expr);
        Ok(())
    }

    fn super_(&mut self, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        let Some(frame) = self.class_frames.last() else {
            compiler_error!(self, expr, "Cannot use 'super' outside of a class method");
        };
        if frame.superclass.is_none() {
            compiler_error!(self, expr, "Cannot use 'super' outside of a child class method");
        }

        self.emit_operand(opcode::GET_LOCAL, 0, expr);
        Ok(())
    }

    fn unary_expression(&mut self, op: &Operator, expr: &AstId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(opcode::from_operator(op), expr);
        Ok(())
    }

    /// Resolves a bare identifier to the binding it denotes.
    fn resolve_place(&mut self, name: *mut ObjString) -> Result<Place, anyhow::Error> {
        let place = if let Some(slot) = self.resolve_local(name) {
            Place::Local(slot)
        } else if let Some(idx) = self.resolve_upvalue(name)? {
            Place::Upvalue(idx)
        } else if let Some(id) = self.class_frames.last().and_then(|f| f.class.resolve_id(name)) {
            Place::Field(id)
        } else {
            Place::Global(self.chunk.add_constant(Value::from(name))?)
        };
        Ok(place)
    }

    /// Emits a read of `place`, pushing its value.
    fn emit_load(&mut self, place: Place, node: &AstId<Expr>) {
        match place {
            Place::Local(slot) => self.emit_operand(opcode::GET_LOCAL, slot, node),
            Place::Upvalue(idx) => self.emit_operand(opcode::GET_UPVALUE, idx, node),
            Place::Field(id) => {
                self.emit_operand(opcode::GET_LOCAL, 0, node);
                self.emit_operand(opcode::GET_PROPERTY_ID, id, node);
            },
            Place::Global(idx) => self.emit_operand(opcode::GET_GLOBAL, idx, node),
        }
    }

    /// Emits a store into `place`; the value to store is already on top of the
    /// stack. When `discarded` (statement position) the store also drops the value.
    fn emit_store(&mut self, place: Place, discarded: bool, node: &AstId<Expr>) {
        match place {
            Place::Local(slot) => {
                let op = if discarded { opcode::SET_LOCAL_POP } else { opcode::SET_LOCAL };
                self.emit_operand(op, slot, node);
            },
            Place::Upvalue(idx) => {
                let op = if discarded { opcode::SET_UPVALUE_POP } else { opcode::SET_UPVALUE };
                self.emit_operand(op, idx, node);
            },
            Place::Field(id) => {
                self.emit_operand(opcode::GET_LOCAL, 0, node); // push `this` (the target)
                let op = if discarded { opcode::SET_PROPERTY_ID_POP } else { opcode::SET_PROPERTY_ID };
                self.emit_operand(op, id, node);
            },
            Place::Global(idx) => {
                self.emit_operand(opcode::SET_GLOBAL, idx, node);
                if discarded { self.emit(opcode::POP, node); }
            }
        }
    }

    /// Resolves `expr` to a local slot if it is a bare identifier bound to one.
    fn local_operand(&mut self, expr: &AstId<Expr>) -> Option<u8> {
        let Expr::Identifier(name) = self.ast.get(expr) else { return None };
        let name = self.gc.intern(name);
        self.resolve_local(name)
    }

    /// `(local_slot, const_idx)` when `local_side` is a local and `const_side` is a
    /// numeric literal. The constant is interned only on a full match, so callers
    /// can probe operand orders cheaply.
    pub (super) fn try_local_const(&mut self, local_side: &AstId<Expr>, const_side: &AstId<Expr>) -> Result<Option<(u8, u8)>, anyhow::Error> {
        let Some(local) = self.local_operand(local_side) else { return Ok(None) };
        let Expr::Literal(Literal::Number(num)) = self.ast.get(const_side) else { return Ok(None) };
        let const_idx = self.chunk.add_constant(Value::from(*num))?;
        Ok(Some((local, const_idx)))
    }

    /// Like `try_local_const`, but also accepts the constant on the left.
    fn try_local_const_commutative(&mut self, left: &AstId<Expr>, right: &AstId<Expr>) -> Result<Option<(u8, u8)>, anyhow::Error> {
        if let Some(pair) = self.try_local_const(left, right)? {
            return Ok(Some(pair));
        }
        return self.try_local_const(right, left);
    }

    /// If `rhs` is `a + b` with both operands locals, returns their slots.
    fn local_add_operands(&mut self, rhs: &AstId<Expr>) -> Option<(u8, u8)> {
        let Expr::Binary(Operator::Add, a, b) = self.ast.get(rhs) else { return None };
        let (a, b) = (*a, *b);
        Some((self.local_operand(&a)?, self.local_operand(&b)?))
    }

    /// Compiles `lhs = rhs`. `discarded` is true in statement position, which lets
    /// the store drop its own value (and enables the `dst = a + b` store fusion).
    fn compile_assign(&mut self, lhs: &AstId<Expr>, rhs: &AstId<Expr>, discarded: bool) -> Result<(), anyhow::Error> {
        match self.ast.get(lhs) {
            Expr::Identifier(name) => {
                let interned = self.gc.intern(name);
                if let Some(local) = self.locals.iter().rev().find(|local| local.name == interned) {
                    if !local.is_mutable {
                        compiler_error!(self, lhs, "Invalid assignment: '{name}' is immutable");
                    }
                }

                let place = self.resolve_place(interned)?;

                // Store fusion: `dst = a + b` (all locals, value discarded) → one op.
                if discarded {
                    if let Place::Local(dst) = place {
                        if let Some((a, b)) = self.local_add_operands(rhs) {
                            self.emit(opcode::SET_LOCAL_ADD_LOCAL_LOCAL, lhs);
                            self.emit(dst, lhs);
                            self.emit(a, lhs);
                            self.emit(b, lhs);
                            return Ok(());
                        }
                    }
                }

                self.expression(rhs)?;
                self.emit_store(place, discarded, lhs);
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

        match op {
            Operator::Add => {
                if let Some((local, const_idx)) = self.try_local_const_commutative(left, right)? {
                    self.emit(opcode::ADD_LOCAL_CONST, right);
                    self.emit(local, right);
                    self.emit(const_idx, right);
                    return Ok(());
                }
            },
            Operator::Subtract => {
                let block = if let Some((local, const_idx)) = self.try_local_const(left, right)? {
                    Some((opcode::SUB_LOCAL_CONST, local, const_idx))
                } else if let Some((local, const_idx)) = self.try_local_const(right, left)? {
                    Some((opcode::SUB_CONST_LOCAL, const_idx, local))
                } else { None };

                if let Some((opcode, lhs, rhs)) = block {
                    self.emit(opcode, right);
                    self.emit(lhs, right);
                    self.emit(rhs, right);
                    return Ok(());
                }
            },
            _ => {}
        }

        self.expression(left)?;
        self.expression(right)?;
        if !matches!(op, Operator::Comma) {
            self.emit(opcode::from_operator(op), right);
        }
        Ok(())
    }

    fn index(&mut self, expr: &AstId<Expr>, member_expr_id: &AstId<Expr>, op: IndexOp) -> Result<(), anyhow::Error> {
        let expr_type = self.ast.get(expr);
        if matches!(expr_type, Expr::This | Expr::Super) {
            let member_name = match self.ast.get(member_expr_id) {
                Expr::Literal(Literal::String(name)) => Some(self.gc.intern(name)),
                _ => None,
            };

            if self.class_frames.is_empty() {
                if matches!(expr_type, Expr::Super) {
                    compiler_error!(self, expr, "Cannot use 'super' outside of a class method");
                }
                compiler_error!(self, expr, "Cannot use 'this' outside of a class method");
            }

            let class = if matches!(expr_type, Expr::Super) {
                let frame = self.current_class_frame();
                if frame.superclass.is_none() {
                    compiler_error!(self, expr, "Cannot use 'super' outside of a child class method");
                }
                unsafe { &*frame.superclass.unwrap().class }
            } else {
                &self.current_class_frame().class
            };

            if let Some(member_id) = member_name.and_then(|name| class.resolve_id(name)) {
                return self.index_class_member_by_id(expr, member_id, op);
            }

            let is_store = matches!(op, IndexOp::Store { .. });
            let accessor_id = if is_store {
                class.resolve_id(self.gc.preset_identifiers.set)
            } else {
                class.resolve_id(self.gc.preset_identifiers.get)
            };

            if let Some(accessor_id) = accessor_id {
                return self.index_class_member_by_accessor(expr, accessor_id, member_expr_id, op);
            }

            let class_name = unsafe { &(*class.name).value };
            let accessor_name = if is_store { "setter" } else { "getter" };
            if let Some(member_name) = member_name {
                let member_name = unsafe { &(*member_name).value };
                compiler_error!(self, expr, "Invalid index: {class_name} doesn't have member {member_name} and doesn't have a {accessor_name}")
            } else {
                compiler_error!(self, expr, "Invalid index: {class_name} doesn't have a {accessor_name}")
            };
        }

        match op {
            IndexOp::Load => {
                self.expression(expr)?;
                self.expression(member_expr_id)?;
                self.emit(opcode::GET_INDEX, expr);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(expr)?;
                self.expression(member_expr_id)?;
                self.emit(opcode::SET_INDEX, expr);
                if discarded {
                    self.emit(opcode::POP, expr);
                }
            }
        }
        Ok(())
    }

    fn index_class_member_by_id(&mut self, target_expr: &AstId<Expr>, member_id: u8, op: IndexOp) -> Result<(), anyhow::Error> {
        match op {
            IndexOp::Load => {
                self.expression(target_expr)?;
                self.emit_operand(opcode::GET_PROPERTY_ID, member_id, target_expr);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.expression(target_expr)?;
                let store = if discarded { opcode::SET_PROPERTY_ID_POP } else { opcode::SET_PROPERTY_ID };
                self.emit_operand(store, member_id, target_expr);
            }
        }
        Ok(())
    }

    fn index_class_member_by_accessor(&mut self, target_expr: &AstId<Expr>, accessor_id: u8, member_expr_id: &AstId<Expr>, op: IndexOp) -> Result<(), anyhow::Error> {
        self.expression(target_expr)?;
        self.emit_operand(opcode::GET_PROPERTY_ID, accessor_id, target_expr);

        self.expression(member_expr_id)?;
        let arg_count = match op {
            IndexOp::Load => 1,
            IndexOp::Store { rhs, .. } => {
                self.expression(&rhs)?;
                2
            }
        };
        self.emit_operand(opcode::CALL, arg_count, target_expr);

        // A setter call returns a value; drop it in statement position.
        if let IndexOp::Store { discarded: true, .. } = op {
            self.emit(opcode::POP, target_expr);
        }
        Ok(())
    }

    fn call_expression(&mut self, expr: &AstId<Expr>, args: &Option<AstId<Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Super => {
                let Some(frame) = self.class_frames.last() else {
                    compiler_error!(self, expr, "Cannot use 'super' outside of a class method");
                };
                let Some(superclass) = frame.superclass else {
                    compiler_error!(self, expr, "Cannot use 'super' outside of a child class method");
                };
                let init_str = self.gc.intern("@init");
                let member_id = unsafe { &*superclass.class }.resolve_id(init_str).unwrap();

                self.emit_operand(opcode::GET_LOCAL, 0, expr);
                self.emit_operand(opcode::GET_PROPERTY_ID, member_id, expr);
            },
            _ => { self.expression(expr)? }
        };

        if let Some(args) = args {
            self.expression(args)?;
            self.emit(opcode::CALL, expr);
            self.emit_count(args);
        } else {
            self.emit_operand(opcode::CALL, 0, expr);
        }

        Ok(())
    }

    fn lambda(&mut self, expr: &AstId<Expr>, decl: &FnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, decl, kind)?;
        self.emit_operand(opcode::PUSH_CLOSURE, const_idx, expr);
        return Ok(());
    }

    fn literal(&mut self, expr: &AstId<Expr>, literal: &Literal) -> Result<(), anyhow::Error> {
        match literal {
            Literal::Number(num) => {
                let idx = self.chunk.add_constant(Value::from(*num))?;
                self.emit_operand(opcode::PUSH_CONSTANT, idx, expr);
            },
            Literal::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.chunk.add_constant(Value::from(str))?;
                self.emit_operand(opcode::PUSH_CONSTANT, idx, expr);
            },
            Literal::Null => { self.emit(opcode::PUSH_NULL, expr); },
            Literal::Boolean(true) => { self.emit(opcode::PUSH_TRUE, expr); },
            Literal::Boolean(false) => { self.emit(opcode::PUSH_FALSE, expr); },
            Literal::Array(list) => {
                if let Some(list) = list {
                    self.expression(list)?;
                    self.emit(opcode::ARRAY, expr);
                    self.emit_count(list);
                } else {
                    self.emit_operand(opcode::ARRAY, 0, expr);
                }
            },
            Literal::Lambda(decl) => self.lambda(expr, decl, FnKind::Function)?
        };

        return Ok(());
    }
}
