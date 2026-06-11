//! Lowering: AST to HIR transformation.

use anyhow::anyhow;

use crate::ast::{Ast, AstId, CatchClause, ClassDecl, Expr, FieldInit, FnDecl, Literal, Operator, Stmt};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{
    BinOp, Hir, HirCatchClause, HirClassDecl, HirExpr, HirFieldInit, HirFnDecl, HirId, HirLiteral,
    HirStmt, UnOp,
};

pub fn lower(mut ast: Ast) -> Result<Hir, anyhow::Error> {
    let root = ast.get_root();
    let (ident_ids, ident_texts) = ast.take_idents();
    let mut lowerer = Lowerer { ast: &ast, hir: Hir::new(ident_ids, ident_texts) };
    lowerer.stmt(&root)?;
    Ok(lowerer.hir)
}

struct Lowerer<'a> {
    ast: &'a Ast,
    hir: Hir,
}

impl<'a> Lowerer<'a> {
    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &AstId<T>) -> anyhow::Error {
        anyhow!("{}\n\tat {}", msg.into(), self.ast.pos(node_id))
    }

    fn stmt(&mut self, stmt_id: &AstId<Stmt>) -> Result<HirId<HirStmt>, anyhow::Error> {
        let pos = self.ast.pos(stmt_id).clone();
        let kind = match self.ast.get(stmt_id) {
            Stmt::Expression(expr) => HirStmt::Expression(self.expr(expr)?),
            Stmt::Return(expr) => HirStmt::Return(self.opt_expr(expr)?),
            Stmt::Throw(expr) => HirStmt::Throw(self.expr(expr)?),
            Stmt::Try(body, catch, finally) => {
                let body = self.expr(body)?;
                let catch = match catch {
                    Some(catch) => Some(self.catch_clause(catch)?),
                    None => None,
                };
                let finally = self.opt_expr(finally)?;
                HirStmt::Try(body, catch, finally)
            },
            Stmt::While(cond, body) => HirStmt::While(self.expr(cond)?, self.expr(body)?),
            Stmt::If(cond, then, otherwise) => {
                let cond = self.expr(cond)?;
                let then = self.expr(then)?;
                let otherwise = match otherwise {
                    Some(otherwise) => Some(self.stmt(otherwise)?),
                    None => None,
                };
                HirStmt::If(cond, then, otherwise)
            },
            Stmt::Block(body) => HirStmt::Block(self.expr(body)?),
            Stmt::Say(field) => HirStmt::Say(self.field_init(field)?),
            Stmt::Fn(decl) => HirStmt::Fn(self.fn_decl(decl)?),
            Stmt::Class(decl) => HirStmt::Class(Box::new(self.class_decl(decl, &pos)?)),
        };
        Ok(self.hir.add(kind, pos))
    }

    fn opt_expr(&mut self, expr: &Option<AstId<Expr>>) -> Result<Option<HirId<HirExpr>>, anyhow::Error> {
        match expr {
            Some(expr) => Ok(Some(self.expr(expr)?)),
            None => Ok(None),
        }
    }

    fn expr(&mut self, expr_id: &AstId<Expr>) -> Result<HirId<HirExpr>, anyhow::Error> {
        let pos = self.ast.pos(expr_id).clone();
        let kind = match self.ast.get(expr_id) {
            Expr::Block(stmts) => {
                let stmts = stmts.iter().map(|s| self.stmt(s)).collect::<Result<Vec<_>, _>>()?;
                HirExpr::Block(stmts)
            },
            Expr::Unary(op, operand) => HirExpr::Unary(lower_unop(op), self.expr(operand)?),
            Expr::Binary(op, left, right) => return self.binary(expr_id, op, left, right),
            Expr::Call(callee, args) => HirExpr::Call(self.expr(callee)?, self.exprs(args)?),
            Expr::Index(target, member) => HirExpr::Index(self.expr(target)?, self.expr(member)?),
            Expr::Literal(lit) => HirExpr::Literal(self.literal(lit)?),
            Expr::Identifier(name) => HirExpr::Identifier(*name),
            Expr::This => HirExpr::This,
            Expr::Super => HirExpr::Super,
        };
        Ok(self.hir.add(kind, pos))
    }

    fn binary(&mut self, expr_id: &AstId<Expr>, op: &Operator, left: &AstId<Expr>, right: &AstId<Expr>) -> Result<HirId<HirExpr>, anyhow::Error> {
        let pos = self.ast.pos(expr_id).clone();
        let kind = match op {
            Operator::Assign(None) => HirExpr::Assign(self.expr(left)?, self.expr(right)?),
            Operator::Assign(Some(assign_op)) => {
                let assign_op = assign_op.as_ref().clone();
                let normalized_binop = HirExpr::Binary(lower_binop(&assign_op), self.expr(left)?, self.expr(right)?);
                let right = self.hir.add(normalized_binop, self.ast.pos(right).clone());
                HirExpr::Assign(self.expr(left)?, right)
            },
            Operator::MemberAccess => HirExpr::Index(self.expr(left)?, self.expr(right)?),
            Operator::Comma => return Err(self.error("Unexpected ','", right)),
            _ => HirExpr::Binary(lower_binop(op), self.expr(left)?, self.expr(right)?),
        };
        Ok(self.hir.add(kind, pos))
    }

    fn exprs(&mut self, exprs: &[AstId<Expr>]) -> Result<Vec<HirId<HirExpr>>, anyhow::Error> {
        exprs.iter().map(|e| self.expr(e)).collect()
    }

    fn literal(&mut self, literal: &Literal) -> Result<HirLiteral, anyhow::Error> {
        Ok(match literal {
            Literal::Null => HirLiteral::Null,
            Literal::Boolean(b) => HirLiteral::Boolean(*b),
            Literal::Number(n) => HirLiteral::Number(*n),
            Literal::String(s) => HirLiteral::String(s.clone()),
            Literal::Array(elements) => HirLiteral::Array(self.exprs(elements)?),
            Literal::Lambda(decl) => HirLiteral::Lambda(self.fn_decl(decl)?),
        })
    }

    fn fn_decl(&mut self, decl: &FnDecl) -> Result<HirFnDecl, anyhow::Error> {
        Ok(HirFnDecl {
            name: decl.name,
            params: self.exprs(&decl.params)?,
            body: self.expr(&decl.body)?,
        })
    }

    fn field_init(&mut self, field: &FieldInit) -> Result<HirFieldInit, anyhow::Error> {
        Ok(HirFieldInit {
            name: field.name,
            value: self.opt_expr(&field.value)?,
        })
    }

    fn catch_clause(&mut self, catch: &CatchClause) -> Result<HirCatchClause, anyhow::Error> {
        let param = match &catch.param {
            Some(param) => Some(self.expr(param)?),
            None => None,
        };
        Ok(HirCatchClause { param, body: self.expr(&catch.body)? })
    }

    fn class_decl(&mut self, decl: &ClassDecl, class_pos: &SourcePosition) -> Result<HirClassDecl, anyhow::Error> {
        let init = self.lower_init(decl, class_pos)?;
        let getter = match &decl.getter {
            Some(stmt) => Some(self.stmt(stmt)?),
            None => None,
        };
        let setter = match &decl.setter {
            Some(stmt) => Some(self.stmt(stmt)?),
            None => None,
        };
        let methods = decl.methods.iter().map(|m| self.stmt(m)).collect::<Result<Vec<_>, _>>()?;
        Ok(HirClassDecl {
            name: decl.name,
            superclass: decl.superclass,
            init,
            getter,
            setter,
            fields: decl.fields.clone(),
            methods,
        })
    }

    /// Assembles the class initializer: field initializers first, then a `super()`
    /// call for a child class (the explicit one, or a synthesised virtual one), then
    /// the declared body. A class without a declared init gets an empty-bodied one.
    fn lower_init(&mut self, decl: &ClassDecl, class_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
        let is_child = decl.superclass.is_some();

        // The declared init's params, body, and whether it already opens with super().
        let (params, body_stmts, has_super, init_pos): (_, &[AstId<Stmt>], _, _) = match &decl.init {
            Some(init_id) => {
                let init_pos = self.ast.pos(init_id).clone();
                let Stmt::Fn(fn_decl) = self.ast.get(init_id) else { unreachable!() };
                let params = self.exprs(&fn_decl.params)?;
                let Expr::Block(stmts) = self.ast.get(&fn_decl.body) else { unreachable!() };
                let has_super = stmts.first().is_some_and(|s| self.is_super_call(s));
                (params, stmts.as_slice(), has_super, init_pos)
            },
            None => (Vec::new(), &[], false, class_pos.clone()),
        };

        let mut body = Vec::new();

        // 1. Field initializers (`this.f = v`), spliced ahead of everything.
        for (field, value) in &decl.field_inits {
            let target = self.hir.add(HirExpr::Identifier(*field), class_pos.clone());
            let value = self.expr(value)?;
            let assign = self.hir.add(HirExpr::Assign(target, value), class_pos.clone());
            body.push(self.hir.add(HirStmt::Expression(assign), class_pos.clone()));
        }

        // 2. A virtual `super()` for a child class that didn't write one.
        if is_child && !has_super {
            body.push(self.virtual_super_call(&init_pos));
        }

        // 3. The declared body (which may itself open with an explicit `super(...)`).
        for stmt_id in body_stmts {
            body.push(self.stmt(stmt_id)?);
        }

        let body = self.hir.add(HirExpr::Block(body), init_pos.clone());
        let fn_decl = HirFnDecl { name: decl.init_name, params, body };
        Ok(self.hir.add(HirStmt::Fn(fn_decl), init_pos))
    }

    /// Whether `stmt` is a `super(...)` call statement.
    fn is_super_call(&self, stmt: &AstId<Stmt>) -> bool {
        let Stmt::Expression(expr) = self.ast.get(stmt) else { return false };
        let Expr::Call(callee, _) = self.ast.get(expr) else { return false };
        matches!(self.ast.get(callee), Expr::Super)
    }

    fn virtual_super_call(&mut self, pos: &SourcePosition) -> HirId<HirStmt> {
        let super_expr = self.hir.add(HirExpr::Super, pos.clone());
        let call = self.hir.add(HirExpr::Call(super_expr, Vec::new()), pos.clone());
        self.hir.add(HirStmt::Expression(call), pos.clone())
    }
}

fn lower_binop(op: &Operator) -> BinOp {
    match op {
        Operator::Add => BinOp::Add,
        Operator::Subtract => BinOp::Subtract,
        Operator::Multiply => BinOp::Multiply,
        Operator::Divide => BinOp::Divide,
        Operator::LeftShift => BinOp::LeftShift,
        Operator::RightShift => BinOp::RightShift,
        Operator::LessThan => BinOp::LessThan,
        Operator::LessThanEqual => BinOp::LessThanEqual,
        Operator::GreaterThan => BinOp::GreaterThan,
        Operator::GreaterThanEqual => BinOp::GreaterThanEqual,
        Operator::LogicalEqual => BinOp::Equal,
        Operator::LogicalNotEqual => BinOp::NotEqual,
        Operator::LogicalAnd => BinOp::And,
        Operator::LogicalOr => BinOp::Or,
        Operator::BitAnd => BinOp::BitAnd,
        Operator::BitOr => BinOp::BitOr,
        Operator::BitXor => BinOp::BitXor,
        _ => unreachable!("not a runtime binary operator"),
    }
}

fn lower_unop(op: &Operator) -> UnOp {
    match op {
        Operator::Negate => UnOp::Negate,
        Operator::LogicalNot => UnOp::Not,
        Operator::BitNot => UnOp::BitNot,
        _ => unreachable!("not a runtime unary operator"),
    }
}
