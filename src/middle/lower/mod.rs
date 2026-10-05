//! Lowering: AST to HIR transformation.

mod init;
mod ref_access;
mod traits;

use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};

use crate::ast::{Ast, AstId, CatchClause, Expr, FnDecl, Literal, MatchArm, MatchArrayElem, MatchRest, MatchScalar, Matcher, Operator, Param, Receiver, SayDecl, SlotClause, Stmt, Symbol, TypeDecl};
use crate::core::objects::UNDECLARED;
use crate::middle::hir::{BinOp, Hir, HirCatchClause, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchArm, HirMatchElem, HirMatchField, HirMatchRest, HirMatcher, HirParam, HirSayDecl, HirStmt, ObligationWitness, TypeId, UnOp};
use crate::middle::names::NameBindings;

pub fn lower(mut ast: Ast, names: &NameBindings) -> Result<Hir, anyhow::Error> {
    let root = ast.get_root();
    let (ident_ids, ident_texts) = ast.take_idents();
    let mut hir = Hir::new(ident_ids, ident_texts);
    let opt = hir.intern("opt");
    hir.intern("fails");
    hir.intern("Err");
    hir.intern("this");
    let mut lowerer = Lowerer {
        ast: &ast,
        names,
        hir,
        opt,
        provided_traits: HashSet::new(),
        type_ids: HashMap::new(),
        emitted_aliases: HashSet::new(),
        in_factory: None,
        nested_bodies: 0,
        expr_substitution: None,
    };
    lowerer.stmt(&root)?;
    Ok(lowerer.hir)
}

pub(super) struct Factory {
    pub(super) fields: IndexSet<Symbol>,
    pub(super) composer: AstId<Stmt>,
}

struct Lowerer<'a> {
    ast: &'a Ast,
    names: &'a NameBindings,
    hir: Hir,
    opt: Symbol,
    /// The traits the composer currently being lowered provides.
    provided_traits: HashSet<Symbol>,
    type_ids: HashMap<AstId<Stmt>, TypeId>,
    /// Method name aliases (`"<Trait>.<method>"`) usable in the current composer.
    emitted_aliases: HashSet<String>,
    in_factory: Option<Factory>,
    nested_bodies: usize,
    expr_substitution: Option<(AstId<Expr>, HirId<HirExpr>)>,
}

impl<'a> Lowerer<'a> {
    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &AstId<T>) -> anyhow::Error {
        self.error_at(msg, self.ast.pos(node_id))
    }

    fn error_at(&self, msg: impl Into<String>, pos: &SourcePosition) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, pos.clone()))
    }

    fn error_help_at(&self, msg: impl Into<String>, pos: &SourcePosition, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, pos.clone()).with_help(help))
    }

    fn binder_name(&self, binder: &AstId<Matcher>) -> Symbol {
        match self.ast.get(binder) {
            Matcher::Binder(name) => *name,
            _ => unreachable!("a rest element holds a binder"),
        }
    }

    /// A declaration's identity, assigned on first request.
    fn type_id(&mut self, decl: AstId<Stmt>) -> Result<TypeId, anyhow::Error> {
        if let Some(id) = self.type_ids.get(&decl) {
            return Ok(*id);
        }
        let next = TypeId::try_from(self.type_ids.len()).ok().filter(|id| *id != UNDECLARED);
        let Some(next) = next else {
            return Err(self.error(format!("a program may declare at most {} types and traits", UNDECLARED as usize), &decl));
        };
        self.type_ids.insert(decl, next);
        Ok(next)
    }

    fn ast_type(&self, id: &AstId<Stmt>) -> &'a TypeDecl {
        match self.ast.get(id) {
            Stmt::Type(decl) => decl,
            _ => unreachable!("expected a type/trait declaration"),
        }
    }

    fn ast_fn(&self, id: &AstId<Stmt>) -> &'a FnDecl {
        match self.ast.get(id) {
            Stmt::Fn(decl) => decl,
            _ => unreachable!("expected a function declaration"),
        }
    }

    fn ast_block_stmts(&self, id: &AstId<Expr>) -> &'a [AstId<Stmt>] {
        match self.ast.get(id) {
            Expr::Block(stmts) => stmts,
            _ => unreachable!("expected a block expression"),
        }
    }

    fn stmt(&mut self, stmt_id: &AstId<Stmt>) -> Result<HirId<HirStmt>, anyhow::Error> {
        let pos = self.ast.pos(stmt_id).clone();
        let kind = match self.ast.get(stmt_id) {
            Stmt::Expression(expr) => HirStmt::Expression(self.expr(expr)?),
            Stmt::Discard(value) => HirStmt::Discard(self.expr(value)?),
            Stmt::Return(None) if self.in_factory.is_some() && self.nested_bodies == 0 => {
                let pos = self.ast.pos(stmt_id).clone();
                let mut out = self.factory_epilogue(&pos)?;
                out.push(self.hir.add(HirStmt::Return(None), pos.clone()));
                let block = self.hir.add(HirExpr::Block(out), pos.clone());
                HirStmt::Block(block)
            },
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
            Stmt::Defer(body) => HirStmt::Defer(self.expr(body)?),
            Stmt::Match(scrutinee, arms) => {
                let scrutinee = self.expr(scrutinee)?;
                let arms = arms.iter().map(|arm| self.lower_match_arm(arm)).collect::<Result<_, _>>()?;
                HirStmt::Match(scrutinee, arms)
            },
            Stmt::Say(field) => HirStmt::Say(self.say_decl(field)?),
            Stmt::Obligation { name, witness, rules } => {
                let (name, witness, rules) = (*name, *witness, *rules);
                let decl = self.names.witness_decl(name).expect("a witness names a declared type or trait");
                let witness = ObligationWitness { name: witness, id: self.type_id(decl)? };
                self.hir.declare_obligation(name, witness, rules);
                HirStmt::Nop
            },
            Stmt::Fn(decl) => HirStmt::Fn(self.fn_decl(decl)?),
            Stmt::Type(decl) => {
                if decl.is_trait {
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
        if let Some((at, with)) = self.expr_substitution {
            if at == *expr_id {
                return Ok(with);
            }
        }
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
                if matches!(self.ast.get(target), Expr::This) {
                    if let Expr::Literal(Literal::String(name)) = self.ast.get(member) {
                        if name.contains('.') {
                            return Err(self.error(format!("Invalid member access: '{name}' is not a member"), expr_id));
                        }
                        // Inside a factory `this` is not a value, so `this.<field>` names a field and
                        // nothing else. It desugars to the field's synthetic local.
                        if self.in_factory.is_some() {
                            let field = self.hir.symbol_of(name).filter(|sym| self.in_factory.as_ref().unwrap().fields.contains(sym));
                            if let Some(field) = field {
                                let local = self.field_local_sym(field);
                                return Ok(self.hir.add(HirExpr::Identifier(local), pos));
                            }
                            return Err(self.error_help_at(format!("cannot access `this.{name}` in a factory: `this` is not a value there, only its fields"), &pos,
                                "compute the value and assign it to a field with `this.<field> = ...`"));
                        }
                    } else if self.in_factory.is_some() {
                        return Err(self.error("`this` is not a value in a factory; index only its fields by name".to_string(), expr_id));
                    }
                }
                if let Expr::Literal(Literal::String(name)) = self.ast.get(member) {
                    self.hir.intern(&name);
                }
                HirExpr::Index { base: self.expr(target)?, member: self.expr(member)?, is_dot: *is_dot, safe: false }
            },
            Expr::Literal(lit) => HirExpr::Literal(self.literal(lit)?),
            Expr::Identifier(name) => {
                if self.names.trait_ref(*expr_id).is_some() {
                    return Err(self.error(format!("'{}' is a trait and cannot be used as a value (traits are not instantiable)", self.hir.text(*name)), expr_id));
                }
                HirExpr::Identifier(*name)
            },
            // `x is T` is `x ~ T`: the same nominal test, and the same code.
            Expr::Is(target, name) => {
                let target = self.expr(target)?;
                let matcher = HirMatcher::Type { nominal: true, name: *name, shape: None };
                HirExpr::Match(target, self.hir.add(matcher, pos.clone()))
            },
            Expr::Construct(callee, fields) => {
                // The callee is a bare type name `C` or an empty call `C()`.
                let callee = match self.ast.get(callee) {
                    Expr::Call(c, _) => self.expr(c)?,
                    _ => self.expr(callee)?,
                };
                let mut brace = Vec::with_capacity(fields.len());
                for (name, value) in fields {
                    brace.push((*name, self.expr(value)?));
                }
                HirExpr::Construct(callee, brace)
            },
            Expr::This => {
                // A `this.<field>` access is handled in the `Index` arm above. Reaching here means
                // `this` is used as a value, which a factory forbids.
                if self.in_factory.is_some() {
                    return Err(self.error_help_at("the partially-constructed 'this' is not a value in a factory", &pos,
                        "assign or read its fields with `this.<field>`"));
                }
                HirExpr::This
            },
            Expr::SafeAccess(target, member, is_dot) => HirExpr::Index { base: self.expr(target)?, member: self.expr(member)?, is_dot: *is_dot, safe: true },
            Expr::SafeCall(callee, args) => HirExpr::SafeCall(self.expr(callee)?, self.exprs(args)?),
            Expr::Propagate(operand) => HirExpr::Propagate(self.expr(operand)?),
            Expr::Handle(left, binder, handler) => HirExpr::Handle(self.expr(left)?, *binder, self.expr(handler)?),
            Expr::Assert(operand) => HirExpr::Assert(self.expr(operand)?),
            Expr::Anchor(path) => HirExpr::Anchor(self.expr(path)?),
            Expr::Has(left, matcher) => {
                let left = self.expr(left)?;
                self.validate_has_operand(matcher)?;
                // `x has M` is equivalent to `x ~ has M`.
                HirExpr::Match(left, self.lower_matcher(matcher)?)
            },
            Expr::RefAccess(inner) => self.read_ref_access(&inner.clone())?,
            Expr::Match(scrutinee, matcher) => {
                let scrutinee = self.expr(scrutinee)?;
                HirExpr::Match(scrutinee, self.lower_matcher(matcher)?)
            },
        };
        Ok(self.hir.add(kind, pos))
    }

    fn binary(&mut self, expr_id: &AstId<Expr>, op: &Operator, left: &AstId<Expr>, right: &AstId<Expr>) -> Result<HirId<HirExpr>, anyhow::Error> {
        let pos = self.ast.pos(expr_id).clone();
        if let Some(binop) = compound_assign_binop(op) {
            if let Some(target) = self.ref_access_target_of(left) {
                return self.assign_through_ref_access(target, Some(binop), right, &pos);
            }
            let kind = HirExpr::CompoundAssign(self.expr(left)?, binop, self.expr(right)?);
            return Ok(self.hir.add(kind, pos));
        }
        if matches!(op, Operator::Assign) {
            if let Some(target) = self.ref_access_target_of(left) {
                return self.assign_through_ref_access(target, None, right, &pos);
            }
        }
        let kind = match op {
            Operator::Assign => HirExpr::Assign(self.expr(left)?, self.expr(right)?),
            Operator::MemberAccess => HirExpr::Index { base: self.expr(left)?, member: self.expr(right)?, is_dot: true, safe: false },
            Operator::Comma => return Err(self.error("Unexpected ','", right)),
            // Null-coalescing stays a dedicated HIR node. Codegen short-circuits it later.
            Operator::Coalesce => HirExpr::Coalesce(self.expr(left)?, self.expr(right)?),
            _ => HirExpr::Binary(lower_binop(op), self.expr(left)?, self.expr(right)?),
        };
        Ok(self.hir.add(kind, pos))
    }

    fn lower_with_expr_substitution(&mut self, node: &AstId<Expr>, at: AstId<Expr>, with: HirId<HirExpr>) -> Result<HirId<HirExpr>, anyhow::Error> {
        let outer = self.expr_substitution.replace((at, with));
        let lowered = self.expr(node);
        self.expr_substitution = outer;
        lowered
    }

    fn exprs(&mut self, exprs: &[AstId<Expr>]) -> Result<Vec<HirId<HirExpr>>, anyhow::Error> {
        exprs.iter().map(|e| self.expr(e)).collect()
    }

    fn lower_rest(&mut self, rest: &MatchRest) -> Result<HirMatchRest, anyhow::Error> {
        let every = rest.matcher.map(|e| self.lower_matcher(&e)).transpose()?;
        if let Some(every) = every.filter(|e| self.hir.get(e).binds_anything(&self.hir)) {
            return Err(self.error_at(
                "a quantified matcher runs once per value, so it cannot bind a name".to_string(),
                self.hir.pos(&every)));
        }
        Ok(HirMatchRest { binder: rest.binder.map(|b| self.binder_name(&b)), every })
    }

    fn validate_has_operand(&self, id: &AstId<Matcher>) -> Result<(), anyhow::Error> {
        match self.ast.get(id) {
            Matcher::Wildcard | Matcher::Literal(_) => Ok(()),
            Matcher::Binder(_) => Err(self.error_help_at(
                "unexpected binding in a `has` test",
                self.ast.pos(id),
                "`has` only tests. To test and bind, use the `~` match operator; to test without binding, use `_`")),
            Matcher::As(..) => Err(self.error("`has` binds nothing; an `@` as-binding is only for `match`", id)),
            Matcher::Type { name, shape, .. } => {
                if !self.names.is_type_or_trait(*name) {
                    return Err(self.error(format!("'{}' is not a type or trait", self.hir.text(*name)), id));
                }
                // A trailing shape is fine as long as its fields bind nothing.
                match shape {
                    Some(shape) => self.validate_has_operand(shape),
                    None => Ok(()),
                }
            },
            Matcher::Dict(shape) => self.validate_has_operand(&shape.clone()),
            Matcher::Shape { fields, rest } => {
                if rest.is_some() {
                    return Err(self.error_help_at(
                        "a `has` shape cannot take a rest",
                        self.ast.pos(id),
                        "`has { }` tests an instance as well as a dict, and only a dict has a remainder. Use `~ { .. }` to match a dict"));
                }
                for field in fields {
                    if let Matcher::Binder(b) = self.ast.get(&field.value) {
                        if let MatchScalar::String(s) = &field.key {
                            if s == self.hir.text(*b) {
                                return Err(self.error_help_at(
                                    "unexpected binding in a `has` test",
                                    self.ast.pos(&field.value),
                                    format!("for key presence write `{{ {s}: _ }}`")));
                            }
                        }
                    }
                    self.validate_has_operand(&field.value)?;
                }
                Ok(())
            },
            Matcher::Array(elements) => {
                for element in elements {
                    match element {
                        MatchArrayElem::Elem(m) => self.validate_has_operand(m)?,
                        MatchArrayElem::Rest(rest) if rest.binder.is_some() => return Err(self.error(
                            "a `has` array cannot bind a rest; `..name` is only for `match`. Use a nameless `..` to skip the middle", id)),
                        MatchArrayElem::Rest(rest) => {
                            if let Some(every) = &rest.matcher { self.validate_has_operand(every)?; }
                        },
                    }
                }
                Ok(())
            },
            Matcher::And(_) | Matcher::Or(_) => Err(self.error(
                "`&` and `|` combine matchers only in `match`; `has` is a single test, so combine with `&&` or `||`", id)),
        }
    }

    fn lower_match_arm(&mut self, arm: &MatchArm) -> Result<HirMatchArm, anyhow::Error> {
        Ok(HirMatchArm {
            matcher: self.lower_matcher(&arm.matcher)?,
            guard: self.opt_expr(&arm.guard)?,
            body: self.expr(&arm.body)?,
        })
    }

    fn lower_matcher(&mut self, id: &AstId<Matcher>) -> Result<HirId<HirMatcher>, anyhow::Error> {
        let kind = match self.ast.get(id) {
            Matcher::Wildcard => HirMatcher::Wildcard,
            Matcher::Literal(scalar) => HirMatcher::Literal(match_scalar(scalar)),
            Matcher::Binder(name) => HirMatcher::Binder(*name),
            Matcher::Type { nominal, name, shape } => {
                let shape = match shape {
                    Some(shape) => Some(self.lower_matcher(shape)?),
                    None => None,
                };
                HirMatcher::Type { nominal: *nominal, name: *name, shape }
            },
            Matcher::Shape { fields, rest } => {
                let rest = rest.clone();
                let fields: Vec<_> = fields.iter().map(|f| (match_scalar(&f.key), f.value)).collect();
                let mut lowered = Vec::with_capacity(fields.len());
                for (key, value) in fields {
                    if let HirLiteral::String(text) = &key {
                        self.hir.intern(&text.clone());
                    }
                    lowered.push(HirMatchField { key, value: self.lower_matcher(&value)? });
                }
                let rest = rest.as_ref().map(|rest| self.lower_rest(rest)).transpose()?;
                HirMatcher::Shape { fields: lowered, rest }
            },
            Matcher::Dict(shape) => HirMatcher::Dict(self.lower_matcher(&shape.clone())?),
            Matcher::Array(elements) => {
                let elements: Vec<_> = elements.to_vec();
                let mut lowered = Vec::with_capacity(elements.len());
                for element in &elements {
                    lowered.push(match element {
                        MatchArrayElem::Elem(matcher) => HirMatchElem::Elem(self.lower_matcher(matcher)?),
                        MatchArrayElem::Rest(rest) => HirMatchElem::Rest(self.lower_rest(rest)?),
                    });
                }
                HirMatcher::Array(lowered)
            },
            Matcher::As(name, inner) => HirMatcher::As(*name, self.lower_matcher(inner)?),
            Matcher::Or(alternatives) => HirMatcher::Or(self.lower_matchers(&alternatives.clone())?),
            Matcher::And(parts) => HirMatcher::And(self.lower_matchers(&parts.clone())?),
        };
        Ok(self.hir.add(kind, self.ast.pos(id).clone()))
    }

    fn lower_matchers(&mut self, ids: &[AstId<Matcher>]) -> Result<Vec<HirId<HirMatcher>>, anyhow::Error> {
        ids.iter().map(|id| self.lower_matcher(id)).collect()
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

    pub(super) fn field_clauses(&self, decl: &TypeDecl) -> HashMap<Symbol, SlotClause> {
        decl.field_clauses.iter()
            .map(|(field, clause)| (*field, clause.clone()))
            .collect()
    }

    fn receiver(&self, receiver: &Receiver) -> Receiver {
        let clause = receiver.clause.clone();
        Receiver { pos: receiver.pos.clone(), clause, reassignable: receiver.reassignable, anchor: receiver.anchor }
    }

    /// Lowers a parameter list, desugaring each param's `?` marker and `:` clause into one clause.
    pub(super) fn params(&mut self, params: &[Param]) -> Result<Vec<HirParam>, anyhow::Error> {
        params.iter().enumerate().map(|(i, p)| {
            let clause = p.clause.clone();
            let (sym, pattern) = self.param_slot(p, i)?;
            Ok(HirParam {
                anchor: p.anchor,
                name: self.hir.add(HirExpr::Identifier(sym), p.pos.clone()),
                pattern,
                pos: p.pos.clone(),
                reassignable: p.reassignable,
                clause,
            })
        }).collect()
    }

    /// A parameter's slot name and the pattern its entry step matches, decided together.
    fn param_slot(&mut self, param: &Param, index: usize) -> Result<(Symbol, Option<HirId<HirMatcher>>), anyhow::Error> {
        let name = match param.binder(self.ast) {
            Some(name) => name,
            None => self.hir.intern(&format!("{}{index}", crate::middle::hir::SYNTHETIC_PARAM)),
        };
        Ok((name, self.pattern_left_to_match(&param.pattern)?))
    }

    /// What a pattern still has to match once the slot has taken its name. A lone name leaves
    /// nothing, and `name @ inner` leaves the inner, since the name became the slot.
    pub(super) fn pattern_left_to_match(&mut self, pattern: &AstId<Matcher>) -> Result<Option<HirId<HirMatcher>>, anyhow::Error> {
        match self.ast.get(pattern) {
            Matcher::Binder(_) | Matcher::Wildcard => Ok(None),
            Matcher::As(_, inner) => Ok(Some(self.lower_matcher(&(*inner))?)),
            // Listed rather than caught, so a new matcher has to answer here.
            Matcher::Literal(_)
            | Matcher::Type { .. }
            | Matcher::Shape { .. }
            | Matcher::Dict(_)
            | Matcher::Array(_)
            | Matcher::Or(_)
            | Matcher::And(_) => Ok(Some(self.lower_matcher(pattern)?)),
        }
    }

    fn wrap_expression_body(&mut self, body: HirId<HirExpr>, at: &AstId<Expr>) -> HirId<HirExpr> {
        if matches!(self.hir.get(&body), HirExpr::Block(_)) {
            return body;
        }
        let pos = self.ast.pos(at).clone();
        let ret = self.hir.add(HirStmt::Return(Some(body)), pos.clone());
        self.hir.add(HirExpr::Block(vec![ret]), pos)
    }

    fn fn_decl(&mut self, decl: &FnDecl) -> Result<HirFnDecl, anyhow::Error> {
        let clause = decl.clause.clone();
        let receiver = decl.receiver.as_ref().map(|r| self.receiver(r));
        let params = self.params(&decl.params)?;
        self.nested_bodies += 1;
        let body = self.expr(&decl.body).map(|body| self.wrap_expression_body(body, &decl.body));
        self.nested_bodies -= 1;
        Ok(HirFnDecl {
            name: decl.name,
            sig_pos: decl.sig_pos.clone(),
            receiver,
            params,
            body: body?,
            clause,
        })
    }

    /// The synthetic local a factory's `this.<field>` desugars to. A source name cannot start with
    /// `$`, so it never collides with a user binding.
    pub(super) fn field_local_sym(&mut self, field: Symbol) -> Symbol {
        let name = format!("${}", self.hir.text(field));
        self.hir.intern(&name)
    }

    fn say_decl(&mut self, field: &SayDecl) -> Result<HirSayDecl, anyhow::Error> {
        let clause = field.clause.clone();
        let pattern = match &field.pattern {
            Some(pattern) => self.pattern_left_to_match(pattern)?,
            None => None,
        };
        Ok(HirSayDecl {
            name: field.name,
            pattern,
            otherwise: self.opt_expr(&field.otherwise)?,
            value: self.opt_expr(&field.value)?,
            reassignable: field.reassignable,
            clause,
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
