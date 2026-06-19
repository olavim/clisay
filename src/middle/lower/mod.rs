//! Lowering: AST to HIR transformation.
//!
//! The generic statement/expression transform lives here. The trait-composition
//! concern is split out: `traits` handles trait scoping and member folding (`with`),
//! and `init` handles initializer assembly and `init` orchestration.

mod init;
mod traits;

use anyhow::anyhow;

use crate::ast::{Ast, AstId, CatchClause, Expr, FieldInit, FnDecl, Literal, Operator, Stmt, Symbol, TypeDecl};
use crate::middle::hir::{
    BinOp, Hir, HirCatchClause, HirExpr, HirFieldInit, HirFnDecl, HirId, HirLiteral, HirStmt, UnOp,
};
use crate::middle::names::NameBindings;

pub fn lower(mut ast: Ast, names: &NameBindings) -> Result<Hir, anyhow::Error> {
    let root = ast.get_root();
    let (ident_ids, ident_texts) = ast.take_idents();
    let mut lowerer = Lowerer {
        ast: &ast,
        names,
        hir: Hir::new(ident_ids, ident_texts),
        provided_traits: std::collections::HashSet::new(),
        emitted_aliases: std::collections::HashSet::new(),
    };
    lowerer.stmt(&root)?;
    Ok(lowerer.hir)
}

struct Lowerer<'a> {
    ast: &'a Ast,
    /// Name-resolution facts resolved over the AST before lowering: each `type`/`trait`'s flattened
    /// `with`-set and resolved `req` traits, and which identifiers name an in-scope trait. See
    /// `middle::names`.
    names: &'a NameBindings,
    hir: Hir,
    /// The traits the composer currently being lowered provides (its flattened `with`-set).
    /// Used to validate qualified `T.method(...)` calls. Empty outside a composer body.
    provided_traits: std::collections::HashSet<Symbol>,
    /// Qualified-call alias method names (`"<Trait>.<method>"`) emitted for the current
    /// composer: the methods a host override shadowed out of the plain namespace, still
    /// reachable via `T.method(...)`. A qualified call resolves to an alias if one exists,
    /// else to the plain method name.
    emitted_aliases: std::collections::HashSet<String>,
}

impl<'a> Lowerer<'a> {
    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &AstId<T>) -> anyhow::Error {
        anyhow!("{}\n\tat {}", msg.into(), self.ast.pos(node_id))
    }

    /// The `TypeDecl` of a `type`/`trait` declaration statement.
    fn ast_type(&self, id: &AstId<Stmt>) -> &'a TypeDecl {
        match self.ast.get(id) {
            Stmt::Type(decl) => decl,
            _ => unreachable!("expected a type/trait declaration"),
        }
    }

    /// The `FnDecl` of a function/method/init declaration statement.
    fn ast_fn(&self, id: &AstId<Stmt>) -> &'a FnDecl {
        match self.ast.get(id) {
            Stmt::Fn(decl) => decl,
            _ => unreachable!("expected a function declaration"),
        }
    }

    /// The statements of a block expression.
    fn ast_block(&self, id: &AstId<Expr>) -> &'a [AstId<Stmt>] {
        match self.ast.get(id) {
            Expr::Block(stmts) => stmts,
            _ => unreachable!("expected a block expression"),
        }
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
            Stmt::Type(decl) => {
                if decl.is_trait {
                    // A trait emits no runtime type, but it's validated on its own.
                    self.check_provide_require_exclusive(decl, &pos)?;
                    HirStmt::Trait(Box::new(self.lower_trait(*stmt_id, decl, &pos)?))
                } else {
                    HirStmt::Type(Box::new(self.lower_type(*stmt_id, decl, &pos)?))
                }
            },
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
                let lowered = stmts.iter().map(|s| self.stmt(s)).collect::<Result<Vec<_>, _>>()?;
                HirExpr::Block(lowered)
            },
            Expr::Unary(op, operand) => HirExpr::Unary(lower_unop(op), self.expr(operand)?),
            Expr::Binary(op, left, right) => return self.binary(expr_id, op, left, right),
            Expr::Call(callee, args) => {
                if let Some((trait_sym, method)) = self.as_qualified_method_call(callee) {
                    let lowered_args = self.exprs(args)?;
                    self.qualified_method_call(trait_sym, &method, lowered_args, callee, &pos)?
                } else {
                    HirExpr::Call(self.expr(callee)?, self.exprs(args)?)
                }
            },
            Expr::Index(target, member, is_dot) => {
                // The per-trait renamed slot names (`"<Trait>.<name>"`) are an internal artifact;
                // a `.` can't appear in a source identifier, so `this["<Trait>.x"]` is an attempt
                // to reach one. Reject it so private/qualified slots can't be probed by name.
                if matches!(self.ast.get(target), Expr::This) {
                    if let Expr::Literal(Literal::String(name)) = self.ast.get(member) {
                        if name.contains('.') {
                            return Err(self.error(format!("Invalid member access: '{name}' is not a member"), expr_id));
                        }
                    }
                }
                HirExpr::Index(self.expr(target)?, self.expr(member)?, *is_dot)
            },
            Expr::Literal(lit) => HirExpr::Literal(self.literal(lit)?),
            Expr::Identifier(name) => {
                if self.names.trait_ref(*expr_id).is_some() {
                    return Err(self.error(format!("'{}' is a trait and cannot be used as a value (traits are not instantiable)", self.hir.text(*name)), expr_id));
                }
                HirExpr::Identifier(*name)
            },
            Expr::Is(target, name) => HirExpr::Is(self.expr(target)?, *name),
            Expr::Construct(callee, fields) => {
                // The callee is a bare type name `C` or a call `C(args)`. Split off the args; the
                // remaining type expression is evaluated to the type value at runtime.
                let (callee, args) = match self.ast.get(callee) {
                    Expr::Call(c, a) => (self.expr(c)?, self.exprs(a)?),
                    _ => (self.expr(callee)?, Vec::new()),
                };
                let mut brace = Vec::with_capacity(fields.len());
                for (name, value) in fields {
                    brace.push((*name, self.expr(value)?));
                }
                HirExpr::Construct(callee, args, brace)
            },
            Expr::This => HirExpr::This,
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
            Operator::MemberAccess => HirExpr::Index(self.expr(left)?, self.expr(right)?, true),
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
            Literal::Dict(pairs) => {
                let mut lowered = Vec::with_capacity(pairs.len());
                for (key, value) in pairs {
                    lowered.push((self.expr(key)?, self.expr(value)?));
                }
                HirLiteral::Dict(lowered)
            },
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
