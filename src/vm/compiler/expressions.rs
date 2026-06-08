use crate::compiler_error;
use crate::parser::{ASTId, Expr, FnDecl, Literal, Operator, Stmt};
use crate::vm::objects::ObjString;
use crate::vm::opcode;
use crate::vm::value::Value;

use super::{AddLocalFusion, Compiler, FnKind};

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Block(stmts, last_expr) => self.block(expr, stmts, last_expr)?,
            Expr::If(cond, then, otherwise) => self.if_expression(cond, then, otherwise)?,
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Index(expr, id) => self.index(expr, id, None)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(name) => {
                let name = self.gc.intern(name);
                self.identifier(expr, name, false)?
            },
            Expr::This => self.this(expr)?,
            Expr::Super => self.super_(expr)?
        };

        Ok(())
    }

    fn block(&mut self, expr: &ASTId<Expr>, stmts: &Vec<ASTId<Stmt>>, last_expr: &Option<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.hoist_declarations(stmts)?;
        for stmt in stmts {
            self.statement(stmt)?;
        }

        if let Some(last_expr) = last_expr {
            self.expression(last_expr)?;
        } else {
            self.emit(opcode::PUSH_NULL, expr);
        }

        self.exit_scope_with_value(expr);
        Ok(())
    }

    /// Compiles an expression in statement position, where its value is discarded.
    pub (super) fn expression_for_effect(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Block(stmts, last_expr) => self.block_for_effect(expr, stmts, last_expr)?,
            Expr::If(cond, then, otherwise) => self.if_for_effect(cond, then, otherwise)?,
            _ => {
                self.setter_pos = None;
                self.add_local_fusion = None;
                self.expression(expr)?;

                // Collapse `dst = a + b` into a single store-and-discard op.
                if let Some(f) = self.add_local_fusion.take() {
                    let is_tail = matches!(self.setter_pos, Some(pos)
                        if pos + 2 == self.chunk.code.len() && self.chunk.code[pos] == opcode::SET_LOCAL);
                    if is_tail {
                        self.chunk.code.truncate(f.start);
                        self.chunk.code_pos.truncate(f.start);
                        let op = if f.is_const { opcode::ADD_LOCAL_CONST } else { opcode::ADD_LOCAL };
                        self.emit(op, expr);
                        self.emit(f.dst, expr);
                        self.emit(f.a, expr);
                        self.emit(f.b, expr);
                        self.setter_pos = None;
                        return Ok(());
                    }
                }

                match self.setter_pos.take() {
                    Some(pos) if pos + 2 == self.chunk.code.len() => {
                        self.chunk.code[pos] = match self.chunk.code[pos] {
                            opcode::SET_LOCAL => opcode::SET_LOCAL_POP,
                            opcode::SET_UPVALUE => opcode::SET_UPVALUE_POP,
                            opcode::SET_PROPERTY_ID => opcode::SET_PROPERTY_ID_POP,
                            _ => unreachable!()
                        };
                    },
                    _ => self.emit(opcode::POP, expr)
                }
            }
        }
        Ok(())
    }

    fn block_for_effect(&mut self, expr: &ASTId<Expr>, stmts: &Vec<ASTId<Stmt>>, last_expr: &Option<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.hoist_declarations(stmts)?;
        for stmt in stmts {
            self.statement(stmt)?;
        }

        // The block's value is discarded: compile a tail expression for effect
        // and emit no `PUSH_NULL` placeholder when there is none.
        if let Some(last_expr) = last_expr {
            self.expression_for_effect(last_expr)?;
        }

        self.exit_scope(expr);
        Ok(())
    }

    fn if_for_effect(&mut self, cond: &ASTId<Expr>, then: &ASTId<Expr>, otherwise: &Option<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        let jump_ref = self.emit_conditional_jump(cond, cond)?;
        self.expression_for_effect(then)?;
        if let Some(otherwise) = otherwise {
            let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
            self.patch_jump(jump_ref)?;
            self.expression_for_effect(otherwise)?;
            self.patch_jump(else_jump_ref)?;
        } else {
            // No else and no value: when the condition is false, jump past the
            // then-branch to here, leaving nothing on the stack.
            self.patch_jump(jump_ref)?;
        }
        Ok(())
    }

    fn this(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            compiler_error!(self, expr, "Cannot use 'this' outside of a class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn super_(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Some(frame) = self.class_frames.last() else {
            compiler_error!(self, expr, "Cannot use 'super' outside of a class method");
        };
        if frame.superclass.is_none() {
            compiler_error!(self, expr, "Cannot use 'super' outside of a child class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn if_expression(&mut self, cond: &ASTId<Expr>, then: &ASTId<Expr>, otherwise: &Option<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        let jump_ref = self.emit_conditional_jump(cond, cond)?;
        self.expression(then)?;
        let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
        self.patch_jump(jump_ref)?;
        if let Some(otherwise) = otherwise {
            self.expression(otherwise)?;
        } else {
            self.emit(opcode::PUSH_NULL, cond);
        }
        self.patch_jump(else_jump_ref)?;
        Ok(())
    }
    
    fn unary_expression(&mut self, op: &Operator, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(opcode::from_operator(op), expr);
        Ok(())
    }

    /// Resolves `expr` to a local slot if it is a bare identifier bound to one.
    fn local_operand(&mut self, expr: &ASTId<Expr>) -> Option<u8> {
        let Expr::Identifier(name) = self.ast.get(expr) else { return None };
        let name = self.gc.intern(name);
        self.resolve_local(name)
    }

    /// For `dst = <right>`, detects whether `right` is `a + b` with `a` a local and
    /// `b` a local or numeric literal, returning the operands for an `ADD_LOCAL`
    /// fusion. `dst` is the already-resolved target local slot.
    fn detect_add_local_fusion(&mut self, dst: u8, right: &ASTId<Expr>) -> Result<Option<AddLocalFusion>, anyhow::Error> {
        let Expr::Binary(Operator::Add, l, r) = self.ast.get(right) else { return Ok(None) };
        let (l, r) = (*l, *r);

        let Some(a) = self.local_operand(&l) else { return Ok(None) };

        if let Some(b) = self.local_operand(&r) {
            return Ok(Some(AddLocalFusion { start: 0, dst, a, b, is_const: false }));
        }
        if let Expr::Literal(Literal::Number(num)) = self.ast.get(&r) {
            let const_idx = self.chunk.add_constant(Value::from(*num))?;
            return Ok(Some(AddLocalFusion { start: 0, dst, a, b: const_idx, is_const: true }));
        }
        Ok(None)
    }

    fn binary_expression(&mut self, op: &Operator, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if let Operator::Assign(_) = op {
            match self.ast.get(left) {
                Expr::Identifier(name) => {
                    let intern_name = self.gc.intern(name);
                    if let Some(local) = self.locals.iter().rev().find(|local| local.name == intern_name) {
                        if !local.is_mutable {
                            compiler_error!(self, left, "Invalid assignment: '{name}' is immutable");
                        }
                    }

                    // Record a potential `dst = a + b` fusion: emit the normal
                    // sequence (correct in value context), then let effect context
                    // collapse the tail. `start` marks where it began.
                    let fusion = match self.resolve_local(intern_name) {
                        Some(dst) => self.detect_add_local_fusion(dst, right)?,
                        None => None
                    };
                    let start = self.chunk.code.len();

                    self.expression(right)?;
                    self.identifier(left, intern_name, true)?;

                    self.add_local_fusion = fusion.map(|f| AddLocalFusion { start, ..f });
                    return Ok(());
                },
                Expr::Index(obj, member) => {
                    self.index(obj, member, Some(right))?;
                    return Ok(());
                },
                _ => compiler_error!(self, left, "Invalid assignment")
            }
        }

        if let Operator::MemberAccess = op {
            self.index(left, right, None)?;
            return Ok(());
        }

        // Value-producing `local - number` fusion (call args like `func(n - 1)`):
        // load the slot, subtract the constant, push the result — one op for three.
        if matches!(op, Operator::Subtract) {
            if let Some(a) = self.local_operand(left) {
                if let Expr::Literal(Literal::Number(num)) = self.ast.get(right) {
                    let const_idx = self.chunk.add_constant(Value::from(*num))?;
                    self.emit(opcode::SUB_LOCAL_CONST, right);
                    self.emit(a, right);
                    self.emit(const_idx, right);
                    return Ok(());
                }
            }
        }

        self.expression(left)?;
        self.expression(right)?;
        if !matches!(op, Operator::Comma) {
            self.emit(opcode::from_operator(op), right);
        }
        Ok(())
    }

    fn index(&mut self, expr: &ASTId<Expr>, member_expr_id: &ASTId<Expr>, assign_expr: Option<&ASTId<Expr>>) -> Result<(), anyhow::Error> {
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
                return self.index_class_member_by_id(expr, member_id, assign_expr);
            }

            let accessor_id = if assign_expr.is_some() {
                class.resolve_id(self.gc.preset_identifiers.set)
            } else {
                class.resolve_id(self.gc.preset_identifiers.get)
            };

            if let Some(accessor_id) = accessor_id {
                return self.index_class_member_by_accessor(expr, accessor_id, member_expr_id, assign_expr);
            }

            let class_name = unsafe { &(*class.name).value };
            let accessor_name = if assign_expr.is_some() { "setter" } else { "getter" };
            if let Some(member_name) = member_name {
                let member_name = unsafe { &(*member_name).value };
                compiler_error!(self, expr, "Invalid index: {class_name} doesn't have member {member_name} and doesn't have a {accessor_name}")
            } else {
                compiler_error!(self, expr, "Invalid index: {class_name} doesn't have a {accessor_name}")
            };
        }

        if let Some(assign_expr) = assign_expr {
            self.expression(assign_expr)?;
        }

        self.expression(expr)?;
        self.expression(member_expr_id)?;

        self.emit(
            if assign_expr.is_some() { opcode::SET_INDEX } else { opcode::GET_INDEX },
            expr,
        );
        Ok(())
    }

    fn index_class_member_by_id(&mut self, target_expr: &ASTId<Expr>, member_id: u8, assign_expr: Option<&ASTId<Expr>>) -> Result<(), anyhow::Error> {
        match assign_expr {
            None => {
                self.expression(target_expr)?;
                self.emit(opcode::GET_PROPERTY_ID, target_expr);
                self.emit(member_id, target_expr);
            },
            Some(assign_expr) => {
                self.expression(assign_expr)?;
                self.expression(target_expr)?;

                self.setter_pos = Some(self.chunk.code.len());
                self.emit(opcode::SET_PROPERTY_ID, target_expr);
                self.emit(member_id, target_expr);
            }
        }

        Ok(())
    }

    fn index_class_member_by_accessor(&mut self, target_expr: &ASTId<Expr>, accessor_id: u8, member_expr_id: &ASTId<Expr>, assign_expr_id: Option<&ASTId<Expr>>) -> Result<(), anyhow::Error> {
        self.expression(target_expr)?;
        self.emit(opcode::GET_PROPERTY_ID, target_expr);
        self.emit(accessor_id, target_expr);

        let args = if let Some(assign_expr_id) = assign_expr_id {
            vec![member_expr_id, assign_expr_id]
        } else {
            vec![member_expr_id]
        };

        for arg in &args {
            self.expression(arg)?;
        }
        self.emit(opcode::CALL, target_expr);
        self.emit(args.len() as u8, target_expr);
        Ok(())
    }
    
    fn call_expression(&mut self, expr: &ASTId<Expr>, args: &Option<ASTId<Expr>>) -> Result<(), anyhow::Error> {
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

                self.emit(opcode::GET_LOCAL, expr);
                self.emit(0, expr);

                self.emit(opcode::GET_PROPERTY_ID, expr);
                self.emit(member_id, expr);
            },
            _ => { self.expression(expr)? }
        };

        if let Some(args) = args {
            self.expression(args)?;
            self.emit(opcode::CALL, expr);
            self.emit_count(args);
        } else {
            self.emit(opcode::CALL, expr);
            self.emit(0, expr);
        }

        Ok(())
    }

    fn lambda(&mut self, expr: &ASTId<Expr>, decl: &FnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, decl, kind)?;
        self.emit(opcode::PUSH_CLOSURE, expr);
        self.emit(const_idx, expr);
        return Ok(());
    }
    
    fn literal(&mut self, expr: &ASTId<Expr>, literal: &Literal) -> Result<(), anyhow::Error> {
        match literal {
            Literal::Number(num) => {
                let idx = self.chunk.add_constant(Value::from(*num))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
            },
            Literal::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.chunk.add_constant(Value::from(str))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
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
                    self.emit(opcode::ARRAY, expr);
                    self.emit(0, expr);
                }
            },
            Literal::Lambda(decl) => self.lambda(expr, decl, FnKind::Function)?
        };

        return Ok(());
    }
    
    fn identifier(&mut self, expr: &ASTId<Expr>, name: *mut ObjString, assign: bool) -> Result<(), anyhow::Error> {
        let (operand, get_op, set_op) = if let Some(local_idx) = self.resolve_local(name) {
            (local_idx, opcode::GET_LOCAL, opcode::SET_LOCAL)
        } else if let Some(upvalue_idx) = self.resolve_upvalue(name)? {
            (upvalue_idx, opcode::GET_UPVALUE, opcode::SET_UPVALUE)
        } else if let Some(id) = self.class_frames.last().and_then(|f| f.class.resolve_id(name)) {
            // Implicit 'this'
            self.emit(opcode::GET_LOCAL, expr);
            self.emit(0, expr);
            (id, opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
        } else {
            let const_idx = self.chunk.add_constant(Value::from(name))?;
            (const_idx, opcode::GET_GLOBAL, opcode::SET_GLOBAL)
        };

        let op = if assign { set_op } else { get_op };

        if assign && matches!(op, opcode::SET_LOCAL | opcode::SET_UPVALUE | opcode::SET_PROPERTY_ID) {
            self.setter_pos = Some(self.chunk.code.len());
        }

        self.emit(op, expr);
        self.emit(operand, expr);
        return Ok(());
    }
}