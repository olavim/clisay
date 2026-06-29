//! Lowering: AST to HIR transformation.
//!
//! The generic statement/expression transform lives here. The trait-composition
//! concern is split out: `traits` handles trait scoping and member folding (`with`),
//! and `init` handles initializer assembly and `init` orchestration.

mod init;
mod traits;

use anyhow::anyhow;

use crate::ast::{Arm, Ast, AstId, CatchClause, Expr, FieldInit, FnDecl, Literal, MatchElem, MatchScalar, Matcher, Operator, Param, Stmt, Symbol, TypeDecl};
use crate::middle::hir::{
    BinOp, HasMatch, HasSpec, Hir, HirMatchArm, HirCatchClause, HirExpr, HirFieldInit, HirFnDecl, HirId, HirLiteral, HirMatcher, HirMatchElem, HirMatchField, HirParam, HirStmt, UnOp,
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
            Stmt::Match(scrutinee, arms) => {
                let scrutinee = self.expr(scrutinee)?;
                let arms = arms.iter().map(|arm| self.lower_match_arm(arm)).collect::<Result<_, _>>()?;
                HirStmt::Match(scrutinee, arms)
            },
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
            // Safe navigation and the non-null assertion stay as dedicated HIR nodes.
            // Codegen desugars them later.
            Expr::SafeAccess(target, member, is_dot) => HirExpr::SafeAccess(self.expr(target)?, self.expr(member)?, *is_dot),
            Expr::Assert(operand) => HirExpr::Assert(self.expr(operand)?),
            Expr::MatchBind(matcher, scrutinee) => {
                let matcher = Box::new(self.lower_matcher(matcher)?);
                HirExpr::MatchBind(matcher, self.expr(scrutinee)?)
            },
        };
        Ok(self.hir.add(kind, pos))
    }

    fn binary(&mut self, expr_id: &AstId<Expr>, op: &Operator, left: &AstId<Expr>, right: &AstId<Expr>) -> Result<HirId<HirExpr>, anyhow::Error> {
        let pos = self.ast.pos(expr_id).clone();
        // A compound assignment desugars to `target = target <op> value`.
        if let Some(binop) = compound_assign_binop(op) {
            let value = HirExpr::Binary(binop, self.expr(left)?, self.expr(right)?);
            let value = self.hir.add(value, self.ast.pos(right).clone());
            let kind = HirExpr::Assign(self.expr(left)?, value);
            return Ok(self.hir.add(kind, pos));
        }
        let kind = match op {
            Operator::Assign => HirExpr::Assign(self.expr(left)?, self.expr(right)?),
            Operator::MemberAccess => HirExpr::Index(self.expr(left)?, self.expr(right)?, true),
            Operator::Has => HirExpr::Has(self.expr(left)?, self.lower_spec(right)?),
            Operator::Comma => return Err(self.error("Unexpected ','", right)),
            // Null-coalescing stays a dedicated HIR node. Codegen short-circuits it later.
            Operator::Coalesce => HirExpr::Coalesce(self.expr(left)?, self.expr(right)?),
            _ => HirExpr::Binary(lower_binop(op), self.expr(left)?, self.expr(right)?),
        };
        Ok(self.hir.add(kind, pos))
    }

    fn exprs(&mut self, exprs: &[AstId<Expr>]) -> Result<Vec<HirId<HirExpr>>, anyhow::Error> {
        exprs.iter().map(|e| self.expr(e)).collect()
    }

    fn lower_spec(&mut self, id: &AstId<Expr>) -> Result<HasSpec, anyhow::Error> {
        match self.ast.get(id) {
            // Any scalar literal is a key: `has "x"`, `has 1`, `has true`, `has null`.
            Expr::Literal(lit @ (Literal::String(_) | Literal::Number(_) | Literal::Boolean(_) | Literal::Null)) => {
                Ok(HasSpec::Key(self.literal(lit)?))
            },
            // An identifier in spec position names a type/trait, never a runtime value.
            Expr::Identifier(name) => {
                if !self.names.is_type_or_trait(*name) {
                    return Err(self.error(format!("'{}' is not a type or trait", self.hir.text(*name)), id));
                }
                Ok(HasSpec::Surface(*name))
            },
            Expr::Literal(Literal::Array(elements)) => {
                let specs = elements.iter().map(|e| self.lower_spec(e)).collect::<Result<_, _>>()?;
                Ok(HasSpec::All(specs))
            },
            Expr::Literal(Literal::Dict(pairs)) => {
                let mut fields = Vec::with_capacity(pairs.len());
                for (key, value) in pairs {
                    let name = self.spec_key(key)?;
                    fields.push((name, self.lower_match(value)?));
                }
                Ok(HasSpec::Fields(fields))
            },
            _ => Err(self.error("The right side of `has` must be a key, a type, or a literal shape, not a value", id)),
        }
    }

    /// Lowers the value side of a `has` spec entry.
    fn lower_match(&mut self, id: &AstId<Expr>) -> Result<HasMatch, anyhow::Error> {
        match self.ast.get(id) {
            Expr::Literal(lit @ (Literal::Null | Literal::Boolean(_) | Literal::Number(_) | Literal::String(_))) => {
                Ok(HasMatch::Eq(self.literal(lit)?))
            },
            Expr::Literal(Literal::Array(_) | Literal::Dict(_)) => {
                Ok(HasMatch::Spec(self.lower_spec(id)?))
            },
            // A bare name in a value position would bind, which `has` does not do.
            // Suggest `has <name>`, the structural test the bare name used to mean.
            Expr::Identifier(name) => Err(self.error(
                format!(
                    "A `has` value must be a literal, `is T`, `has T`, or a nested shape; did you mean `has {}`?",
                    self.hir.text(*name)
                ),
                id,
            )),
            _ => Err(self.error("A `has` value must be a literal, `is T`, `has T`, or a nested shape", id)),
        }
    }

    fn lower_match_arm(&mut self, arm: &Arm) -> Result<HirMatchArm, anyhow::Error> {
        Ok(HirMatchArm {
            matcher: self.lower_matcher(&arm.matcher)?,
            guard: self.opt_expr(&arm.guard)?,
            body: self.expr(&arm.body)?,
        })
    }

    fn lower_matcher(&mut self, id: &AstId<Matcher>) -> Result<HirMatcher, anyhow::Error> {
        Ok(match self.ast.get(id) {
            Matcher::Wildcard => HirMatcher::Wildcard,
            Matcher::Literal(scalar) => HirMatcher::Literal(match_scalar(scalar)),
            Matcher::Binder(name) => HirMatcher::Binder(*name),
            Matcher::Type { nominal, name, shape } => {
                let shape = match shape {
                    Some(shape) => Some(Box::new(self.lower_matcher(shape)?)),
                    None => None,
                };
                HirMatcher::Type { nominal: *nominal, name: *name, shape }
            },
            Matcher::Shape(fields) => {
                let mut lowered = Vec::with_capacity(fields.len());
                for field in fields {
                    lowered.push(HirMatchField { key: match_scalar(&field.key), value: self.lower_matcher(&field.value)? });
                }
                HirMatcher::Shape(lowered)
            },
            Matcher::Array(elements) => {
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(match element {
                        MatchElem::Elem(matcher) => HirMatchElem::Elem(self.lower_matcher(matcher)?),
                        MatchElem::Rest(name) => HirMatchElem::Rest(*name),
                    });
                }
                HirMatcher::Array(lowered)
            },
            Matcher::As(name, inner) => HirMatcher::As(*name, Box::new(self.lower_matcher(inner)?)),
            Matcher::Or(alternatives) => HirMatcher::Or(self.lower_matchers(alternatives)?),
            Matcher::And(parts) => HirMatcher::And(self.lower_matchers(parts)?),
        })
    }

    fn lower_matchers(&mut self, ids: &[AstId<Matcher>]) -> Result<Vec<HirMatcher>, anyhow::Error> {
        ids.iter().map(|id| self.lower_matcher(id)).collect()
    }

    /// The key a `has` dict-spec entry denotes. A key is any scalar literal, mirroring dict
    /// literals; a computed key is a runtime value and is rejected.
    fn spec_key(&mut self, id: &AstId<Expr>) -> Result<HirLiteral, anyhow::Error> {
        match self.ast.get(id) {
            Expr::Literal(lit @ (Literal::String(_) | Literal::Number(_) | Literal::Boolean(_) | Literal::Null)) => {
                Ok(self.literal(lit)?)
            },
            _ => Err(self.error("A `has` key must be a literal value", id)),
        }
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

    /// Lowers a parameter list, carrying each param's nullability and mutability markers.
    pub(super) fn params(&mut self, params: &[Param]) -> Result<Vec<HirParam>, anyhow::Error> {
        params.iter().map(|p| Ok(HirParam {
            name: self.expr(&p.name)?,
            nullable: p.nullable,
            mutable: p.mutable,
        })).collect()
    }

    fn fn_decl(&mut self, decl: &FnDecl) -> Result<HirFnDecl, anyhow::Error> {
        Ok(HirFnDecl {
            name: decl.name,
            params: self.params(&decl.params)?,
            body: self.expr(&decl.body)?,
            ret: decl.ret,
        })
    }

    fn field_init(&mut self, field: &FieldInit) -> Result<HirFieldInit, anyhow::Error> {
        Ok(HirFieldInit {
            name: field.name,
            value: self.opt_expr(&field.value)?,
            nullable: field.nullable,
            mutable: field.mutable,
        })
    }

    fn catch_clause(&mut self, catch: &CatchClause) -> Result<HirCatchClause, anyhow::Error> {
        let param = match &catch.param {
            Some(param) => Some(self.expr(param)?),
            None => None,
        };
        Ok(HirCatchClause { param, mutable: catch.mutable, body: self.expr(&catch.body)? })
    }
}

/// The binary operator a compound assignment applies, or `None` for any other operator.
fn compound_assign_binop(op: &Operator) -> Option<BinOp> {
    Some(match op {
        Operator::AddAssign => BinOp::Add,
        Operator::SubtractAssign => BinOp::Subtract,
        Operator::MultiplyAssign => BinOp::Multiply,
        Operator::DivideAssign => BinOp::Divide,
        Operator::BitAndAssign => BinOp::BitAnd,
        Operator::BitOrAssign => BinOp::BitOr,
        Operator::BitXorAssign => BinOp::BitXor,
        Operator::LeftShiftAssign => BinOp::LeftShift,
        Operator::RightShiftAssign => BinOp::RightShift,
        Operator::LogicalAndAssign => BinOp::And,
        Operator::LogicalOrAssign => BinOp::Or,
        _ => return None,
    })
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

fn match_scalar(scalar: &MatchScalar) -> HirLiteral {
    match scalar {
        MatchScalar::Null => HirLiteral::Null,
        MatchScalar::Boolean(b) => HirLiteral::Boolean(*b),
        MatchScalar::Number(n) => HirLiteral::Number(*n),
        MatchScalar::String(s) => HirLiteral::String(s.clone()),
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
