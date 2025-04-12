use anyhow::bail;

use crate::compiler_error;
use crate::parser::{ASTId, Expr, Literal, Operator};
use crate::vm::objects::ObjString;
use crate::vm::opcode;
use crate::vm::value::Value;

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Ternary(cond, then, otherwise) => self.if_expression(cond, then, otherwise)?,
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Array(exprs) => self.array_expression(expr, exprs)?,
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

    fn this(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            bail!("Cannot use 'this' outside of a class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn super_(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Some(frame) = self.class_frames.last() else {
            bail!("Cannot use 'super' outside of a class method");
        };
        if frame.superclass.is_none() {
            bail!("Cannot use 'super' outside of a child class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn if_expression(&mut self, cond: &ASTId<Expr>, then: &ASTId<Expr>, otherwise: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(cond)?;
        let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
        self.expression(then)?;
        let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
        self.patch_jump(jump_ref)?;
        self.expression(otherwise)?;
        self.patch_jump(else_jump_ref)?;
        Ok(())
    }
    
    fn unary_expression(&mut self, op: &Operator, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(opcode::from_operator(op), expr);
        Ok(())
    }

    fn binary_expression(&mut self, op: &Operator, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if let Operator::Assign(_) = op {
            match self.ast.get(left) {
                Expr::Identifier(name) => {
                    let intern_name = self.gc.intern(name);
                    if let Some(local) = self.locals.iter().rev().find(|local| local.name == intern_name) {
                        if !local.is_mutable {
                            compiler_error!(self, format!("Invalid assignment: '{}' is immutable", name), left);
                        }
                    }

                    self.expression(right)?;
                    self.identifier(left, intern_name, true)?;
                    return Ok(());
                },
                Expr::Index(obj, member) => {
                    self.index(obj, member, Some(right))?;
                    return Ok(());
                },
                _ => compiler_error!(self, "Invalid assignment", left)
            }
        }

        if let Operator::MemberAccess = op {
            self.index(left, right, None)?;
            return Ok(());
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(opcode::from_operator(op), right);
        Ok(())
    }

    fn index(&mut self, expr: &ASTId<Expr>, member_expr_id: &ASTId<Expr>, assign_expr: Option<&ASTId<Expr>>) -> Result<(), anyhow::Error> {
        let expr_type = self.ast.get(expr);
        if matches!(expr_type, Expr::This | Expr::Super) {
            let member_name = match self.ast.get(member_expr_id) {
                Expr::Literal(Literal::String(name)) => Some(self.gc.intern(name)),
                _ => None,
            };

            let class = if matches!(expr_type, Expr::Super) {
                unsafe { &*self.current_class_frame().superclass.unwrap().class }
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
            let error_message = if let Some(member_name) = member_name {
                let member_name = unsafe { &(*member_name).value };
                format!("Invalid index: {class_name} doesn't have member {member_name} and doesn't have a {accessor_name}")
            } else {
                format!("Invalid index: {class_name} doesn't have a {accessor_name}")
            };
            compiler_error!(self, error_message, expr);
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

        self.emit_call(target_expr, args)?;
        Ok(())
    }
    
    fn call_expression(&mut self, expr: &ASTId<Expr>, args: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Super => {
                let frame = self.class_frames.last().unwrap();
                let superclass = frame.superclass.unwrap();
                let init_str = self.gc.intern("@init");
                let member_id = unsafe { &*superclass.class }.resolve_id(init_str).unwrap();

                self.emit(opcode::GET_LOCAL, expr);
                self.emit(0, expr);

                self.emit(opcode::GET_PROPERTY_ID, expr);
                self.emit(member_id, expr);
            },
            _ => { self.expression(expr)? }
        };

        for arg in args {
            self.expression(arg)?;
        }
        self.emit(opcode::CALL, expr);
        self.emit(args.len() as u8, expr);
        Ok(())
    }
    
    fn array_expression(&mut self, expr: &ASTId<Expr>, exprs: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        for item in exprs {
            self.expression(item)?;
        }
        self.emit(opcode::ARRAY, expr);
        self.emit(exprs.len() as u8, expr);
        Ok(())
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
            Literal::Boolean(false) => { self.emit(opcode::PUSH_FALSE, expr); }
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
        self.emit(op, expr);
        self.emit(operand, expr);
        return Ok(());
    }
}