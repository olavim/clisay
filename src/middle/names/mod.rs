//! AST-level name resolution.

use std::collections::{HashMap, HashSet};
use indexmap::IndexSet;

use anyhow::anyhow;

use crate::core::builtins::is_builtin;
use crate::frontend::lex::{Diagnostic, SourcePosition};

use crate::ast::{builtin_obligation_rules, Ast, AstId, CatchClause, Expr, FnDecl, Literal, MatchElem, Matcher, ObligationRules, Operator, SlotClause, Stmt, Symbol, TypeDecl};

/// What an identifier reference binds to.
pub enum Binding {
    Trait(AstId<Stmt>),
}

/// A `type`/`trait` declaration's resolved trait relationships.
struct ResolvedTraits {
    /// The flattened `with`-set (symbol, declaration).
    with: Vec<(Symbol, AstId<Stmt>)>,
    /// The resolved `req` traits (symbol, declaration).
    req: Vec<(Symbol, AstId<Stmt>)>,
    /// The resolved `gives` delegations: `(field, trait, trait declaration)`.
    gives: Vec<(Symbol, Symbol, AstId<Stmt>)>,
}

/// The output of name resolution: per-declaration trait-graph facts and per-reference bindings,
/// consumed by lowering.
pub struct NameBindings {
    type_traits: HashMap<AstId<Stmt>, ResolvedTraits>,
    name_refs: HashMap<AstId<Expr>, Binding>,
    types: HashSet<Symbol>,
    /// Each obligation => the declaration its witness names. A built-in witness has none.
    witness_decls: HashMap<Symbol, AstId<Stmt>>,
}

impl NameBindings {
    /// The flattened `with`-set of a `type`/`trait` declaration.
    pub fn flattened_with(&self, ty: &AstId<Stmt>) -> &[(Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.with)
    }

    /// The resolved `req` traits of a `trait` declaration.
    pub fn req_traits(&self, ty: &AstId<Stmt>) -> &[(Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.req)
    }

    /// The resolved `gives` delegations of a `type`/`trait` declaration: `(field, trait, decl)`.
    pub fn gives_traits(&self, ty: &AstId<Stmt>) -> &[(Symbol, Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.gives)
    }

    /// The trait declaration an identifier / qualified-call-target expression names, if any.
    pub fn trait_ref(&self, expr: AstId<Expr>) -> Option<AstId<Stmt>> {
        match self.name_refs.get(&expr) {
            Some(Binding::Trait(id)) => Some(*id),
            None => None,
        }
    }

    pub fn is_type_or_trait(&self, name: Symbol) -> bool {
        self.types.contains(&name)
    }

    /// The declaration an obligation's witness names.
    pub fn witness_decl(&self, obligation: Symbol) -> Option<AstId<Stmt>> {
        self.witness_decls.get(&obligation).copied()
    }
}

/// The obligation names the compiler defines. A program cannot take one.
const BUILTIN_OBLIGATIONS: [&str; 3] = ["opt", "fails", "void"];

pub fn resolve(ast: &Ast) -> Result<NameBindings, anyhow::Error> {
    let mut resolver = Resolver {
        ast,
        scopes: Vec::new(),
        trait_flatten_cache: HashMap::new(),
        obligation_rules: HashMap::new(),
        witness_owners: HashMap::new(),
        in_condition: false,
        out: NameBindings {
            type_traits: HashMap::new(),
            name_refs: HashMap::new(),
            types: HashSet::new(),
            witness_decls: HashMap::new(),
        },
    };
    resolver.push_scope();
    resolver.predeclare_intrinsics();
    resolver.visit_stmt(&ast.get_root())?;
    resolver.pop_scope();
    Ok(resolver.out)
}

/// Where an obligation clause was written, for the placement rules that key on it.
#[derive(Clone, Copy, PartialEq)]
enum ClauseSite { Field, Member, Return, Other }

/// What kind of binding a declared name introduces.
#[derive(Clone, Copy, PartialEq)]
enum DeclKind {
    Say,
    Param,
    Item,
}

/// One lexical scope.
struct Scope {
    declared: HashMap<Symbol, DeclKind>,
    /// A type or trait name to its declaration. A `type` records `None`, so its name answers
    /// that it is declared and is not a trait.
    traits: HashMap<Symbol, Option<AstId<Stmt>>>,
    /// Every `type`/`trait` name in scope, with the declaration it names.
    types: HashMap<Symbol, Option<AstId<Stmt>>>,
}

struct Resolver<'a> {
    ast: &'a Ast,
    /// The lexical scope stack, innermost last.
    scopes: Vec<Scope>,
    trait_flatten_cache: HashMap<AstId<Stmt>, Vec<(Symbol, AstId<Stmt>)>>,
    /// Each obligation's declared rules, hoisted so a use may precede its declaration.
    obligation_rules: HashMap<Symbol, ObligationRules>,
    /// Which obligation each witness identifies.
    witness_owners: HashMap<Symbol, String>,
    in_condition: bool,
    out: NameBindings,
}

impl<'a> Resolver<'a> {
    fn error<T>(&self, msg: impl Into<String>, at: &AstId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.ast.pos(at).clone()))
    }

    fn error_at(&self, msg: impl Into<String>, pos: &SourcePosition) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, pos.clone()))
    }

    fn error_help<T>(&self, msg: impl Into<String>, at: &AstId<T>, help: impl Into<String>) -> anyhow::Error {
        self.error_help_at(msg, self.ast.pos(at), help)
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope { declared: HashMap::new(), traits: HashMap::new(), types: HashMap::new() });
    }

    /// Reserves the intrinsic names in the root scope: `opt`, `fails`, `void`, and the `fails`
    /// witness type `Err`. Only names the source interned need reserving, since an intrinsic the
    /// program never mentions can never be referenced.
    fn predeclare_intrinsics(&mut self) {
        for name in BUILTIN_OBLIGATIONS {
            if let Some(sym) = self.ast.symbol(name) {
                self.scopes.last_mut().unwrap().declared.insert(sym, DeclKind::Item);
            }
        }
        if let Some(sym) = self.ast.symbol("Err") {
            self.witness_owners.insert(sym, "fails".to_string());
        }
    }

    /// Whether the walk is in a module's own block. The stack is the intrinsic scope the resolver
    /// opens, then the module block, so anything deeper is nested.
    fn at_top_level(&self) -> bool {
        self.scopes.len() <= 2
    }

    /// Whether `name` refers to a `type` or `trait` in scope (the valid right operands of `is`).
    fn is_type_or_trait(&self, name: Symbol) -> bool {
        self.scopes.iter().any(|scope| scope.types.contains_key(&name))
    }

    /// The declaration a type or trait name reaches, innermost first. A built-in answers `None`,
    /// as does a name nothing declares.
    fn lookup_type_decl(&self, name: Symbol) -> Option<AstId<Stmt>> {
        self.scopes.iter().rev().find_map(|scope| scope.types.get(&name)).copied().flatten()
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Resolves a trait name against the scope stack, innermost-first.
    fn lookup_trait(&self, name: Symbol) -> Option<AstId<Stmt>> {
        self.scopes.iter().rev().find_map(|scope| scope.traits.get(&name)).copied().flatten()
    }

    /// Records a declaration in the current scope. A `say` may shadow an earlier value binding
    /// (another `say` or a param); any other collision is rejected.
    fn declare<T>(&mut self, name: Symbol, kind: DeclKind, at: &AstId<T>) -> Result<(), anyhow::Error> {
        self.reject_builtin_name(name, at)?;
        let scope = self.scopes.last_mut().unwrap();
        if let Some(&existing) = scope.declared.get(&name) {
            let can_shadow = kind == DeclKind::Say && existing != DeclKind::Item;
            if !can_shadow {
                return Err(self.error(format!("'{}' already declared in this scope", self.ast.text(name)), at));
            }
        }
        scope.declared.insert(name, kind);
        Ok(())
    }

    /// Refuses a `type`/`trait` that shadows one declared outside its scope.
    fn reject_shadowed_type(&self, name: Symbol, at: &AstId<Stmt>) -> Result<(), anyhow::Error> {
        let enclosing = self.scopes.len() - 1;
        if !self.scopes[..enclosing].iter().any(|scope| scope.types.contains_key(&name)) {
            return Ok(());
        }
        Err(self.error_help(format!("'{}' shadows a type or trait declared in an enclosing scope", self.ast.text(name)), at,
            "rename it, since the outer declaration cannot be named here"))
    }

    fn reject_builtin_name<T>(&self, name: Symbol, at: &AstId<T>) -> Result<(), anyhow::Error> {
        self.reject_builtin_at(name, self.ast.pos(at))
    }

    fn reject_builtin_at(&self, name: Symbol, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        let text = self.ast.text(name);
        match is_builtin(text) {
            true => Err(self.error_at(format!("'{text}' is a built-in and cannot be redeclared"), pos)),
            false => Ok(()),
        }
    }

    fn name_span<T>(&self, name: Symbol, at: &AstId<T>) -> SourcePosition {
        let pos = self.ast.pos(at);
        let end = pos.start + self.ast.text(name).len();
        SourcePosition { source: pos.source.clone(), start: pos.start, end, line: pos.line }
    }

    /// Whether a statement is a declaration the compiler supplies rather than the program.
    fn is_builtin_decl(&self, stmt: &AstId<Stmt>) -> bool {
        matches!(self.ast.get(stmt), Stmt::Type(decl) if decl.builtin.is_some())
    }

    /// Hoists a block's `type`/`trait` declarations into the current scope so a later-declared one
    /// is visible block-wide.
    fn hoist_types(&mut self, stmts: &[AstId<Stmt>]) {
        for stmt in stmts {
            if let Stmt::Obligation { name, rules, .. } = self.ast.get(stmt) {
                self.obligation_rules.insert(*name, *rules);
            }
            if let Stmt::Type(decl) = self.ast.get(stmt) {
                let (name, is_trait) = (decl.name, decl.is_trait);
                self.out.types.insert(name);
                let scope = self.scopes.last_mut().unwrap();
                scope.types.insert(name, Some(*stmt));
                scope.traits.insert(name, is_trait.then_some(*stmt));
            }
        }
    }

    /// Records which obligation a witness identifies.
    fn declare_witness(&mut self, name: Symbol, witness: Symbol, stmt: &AstId<Stmt>) -> Result<(), anyhow::Error> {
        if !self.is_type_or_trait(witness) {
            return Err(self.error(format!("'{}' is not a type or trait", self.ast.text(witness)), stmt));
        }
        // Which declaration the witness names, so identity survives two of them sharing a name.
        if let Some(decl) = self.lookup_type_decl(witness) {
            self.out.witness_decls.insert(name, decl);
        }
        let text = self.ast.text(name).to_string();
        match self.witness_owners.insert(witness, text.clone()) {
            Some(first) if first != text => Err(self.error_help(
                format!("Witness '{}' is already claimed by '{first}'", self.ast.text(witness)), stmt,
                "one witness names one obligation, so give this one its own type or trait")),
            _ => Ok(()),
        }
    }

    /// An obligation's declared rules. A built-in has no declaration to read them from, so it
    /// answers by name.
    fn rules_of(&self, name: Symbol, pos: &SourcePosition) -> Result<ObligationRules, anyhow::Error> {
        if let Some(rules) = self.obligation_rules.get(&name) {
            return Ok(*rules);
        }
        let text = self.ast.text(name);
        builtin_obligation_rules(text).ok_or_else(|| self.error_help_at(
            format!("Obligation '{text}' is not declared"), pos,
            "declare it with `obligation` first, or name a built-in (`opt`, `fails`)"))
    }

    /// A slot may not declare an obligation whose rules forbid the values it would hold. Each rule
    /// refuses for its own reason, so an obligation carrying one of them needs no other alongside it.
    fn check_clause_placement<T>(&self, clause: &SlotClause, site: ClauseSite, at: &AstId<T>) -> Result<(), anyhow::Error> {
        let pos = clause.pos.as_ref().unwrap_or_else(|| self.ast.pos(at));
        // A container and a field both outlive the binding, which is `no persist`'s reason, and
        // neither can discharge, which is `discharge before drop`'s. A return has its own rule.
        let (outlives, prevents, header) = if clause.container {
            (true, "storing it in a container", "A container cannot hold values owing")
        } else if site == ClauseSite::Field {
            (true, "storing it in a field", "A field cannot owe")
        } else if site == ClauseSite::Member {
            (true, "holding it in a member", "A required member cannot owe")
        } else {
            (false, "returning it", "A return cannot owe")
        };
        let ret = site == ClauseSite::Return;
        for name in clause.names.iter().copied() {
            let rules = self.rules_of(name, pos)?;
            let rule = match (outlives, ret) {
                (true, _) if rules.no_persist => "no persist",
                (true, _) if rules.before_drop => "discharge before drop",
                (_, true) if rules.no_return => "no return",
                _ => continue,
            };
            return Err(self.placement_error(name, rule, prevents, header, pos));
        }
        Ok(())
    }

    /// A placement rejection, citing the rule when the author declared the obligation. A built-in has
    /// no declaration to point at, so it gets the guidance that applies to its values instead.
    fn placement_error(&self, name: Symbol, rule: &str, prevents: &str, header: &str, pos: &SourcePosition) -> anyhow::Error {
        let text = self.ast.text(name);
        let help = match builtin_obligation_rules(text).is_none() {
            true => format!("`{text}` declares `{rule}`, which prevents {prevents}"),
            false => builtin_clause_guidance(text).to_string(),
        };
        self.error_help_at(format!("{header} '{text}'"), pos, help)
    }

    /// A `help:` error at a span rather than a node, for a clause that carries its own position.
    fn error_help_at(&self, msg: impl Into<String>, pos: &SourcePosition, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, pos.clone()).with_help(help))
    }

    /// The declared name and kind of a block-level statement.
    fn decl_name(&self, stmt: &AstId<Stmt>) -> Option<(Symbol, DeclKind)> {
        match self.ast.get(stmt) {
            Stmt::Type(decl) => Some((decl.name, DeclKind::Item)),
            Stmt::Fn(decl) => Some((decl.name, DeclKind::Item)),
            Stmt::Obligation { name, .. } => Some((*name, DeclKind::Item)),
            Stmt::Say(field) => Some((field.name, DeclKind::Say)),
            _ => None,
        }
    }

    /// Processes a block's statements in the current (already-pushed) scope: hoist traits for
    /// lookup, check every declaration for collisions, then recurse.
    fn block(&mut self, stmts: &[AstId<Stmt>]) -> Result<(), anyhow::Error> {
        self.hoist_types(stmts);
        for stmt in stmts {
            if self.is_builtin_decl(stmt) {
                continue;
            }

            if let Some((name, kind)) = self.decl_name(stmt) {
                self.declare(name, kind, stmt)?;
            }

            if let Stmt::Say(field) = self.ast.get(stmt) {
                if let Some(pattern) = field.pattern {
                    let binders = self.collect_matcher_binders(&pattern)?;
                    if binders.is_empty() {
                        return Err(self.error_help("This pattern binds no name".to_string(), &pattern,
                            "a binding reads names out of a value; use `_` to ignore the value instead"));
                    }
                    for name in binders {
                        self.declare(name, DeclKind::Say, stmt)?;
                    }
                }
            }
            if let Stmt::Type(decl) = self.ast.get(stmt) {
                self.reject_shadowed_type(decl.name, stmt)?;
            }
        }
        for s in stmts {
            self.visit_stmt(s)?;
        }
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &AstId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt) {
            Stmt::Expression(expr) => self.visit_expr(expr)?,
            Stmt::Return(expr) => if let Some(expr) = expr { self.visit_expr(expr)?; },
            Stmt::Throw(expr) => self.visit_expr(expr)?,
            Stmt::Try(body, catch, finally) => {
                self.visit_expr(body)?;
                if let Some(catch) = catch { self.visit_catch(catch)?; }
                if let Some(finally) = finally { self.visit_expr(finally)?; }
            },
            Stmt::While(cond, body) => { self.visit_condition(cond)?; self.visit_expr(body)?; },
            Stmt::If(cond, then, otherwise) => {
                self.visit_condition(cond)?;
                self.visit_expr(then)?;
                if let Some(otherwise) = otherwise { self.visit_stmt(otherwise)?; }
            },
            Stmt::Block(body) | Stmt::Defer(body) => self.visit_expr(body)?,
            Stmt::Discard(value) => self.visit_expr(value)?,
            Stmt::Say(field) => {
                self.check_clause_placement(&field.clause, ClauseSite::Other, stmt)?;
                if let Some(value) = &field.value { self.visit_expr(value)?; }
                if let Some(otherwise) = &field.otherwise { self.visit_expr(otherwise)?; }
            },
            Stmt::Obligation { name, witness, rules } => {
                let text = self.ast.text(*name);
                if !self.at_top_level() {
                    return Err(self.error(format!("obligation '{text}' must be declared at the top level"), stmt));
                }
                if BUILTIN_OBLIGATIONS.contains(&text) {
                    return Err(self.error(format!("'{text}' is a built-in obligation and cannot be redeclared"), stmt));
                }
                if rules.no_drop {
                    return Err(self.error_help("'no drop' is not available yet", stmt,
                        "the 'no drop' rule is not implemented yet"));
                }
                match witness {
                    Some(witness) => self.declare_witness(*name, *witness, stmt)?,
                    None if rules.to_use || rules.before_drop => {
                        return Err(self.error_help(
                            format!("Obligation '{}' declares a `discharge` rule with no witness", self.ast.text(*name)), stmt,
                            format!("name the bad state it is about, as in `obligation {} {{ witness <Type>; ... }}`", self.ast.text(*name))));
                    },
                    None => {},
                }
            },
            Stmt::Fn(decl) => self.visit_fn(decl)?,
            Stmt::Type(decl) => self.visit_type(stmt, decl)?,
            Stmt::Match(scrutinee, arms) => {
                self.visit_expr(scrutinee)?;
                for arm in arms {
                    self.collect_matcher_binders(&arm.matcher)?;
                    if let Some(guard) = &arm.guard { self.visit_condition(guard)?; }
                    self.visit_expr(&arm.body)?;
                }
            },
        }
        Ok(())
    }

    /// A function/lambda/method. Params live in their own scope.
    fn visit_fn(&mut self, decl: &FnDecl) -> Result<(), anyhow::Error> {
        self.check_clause_placement(&decl.clause, ClauseSite::Return, &decl.body)?;
        if let Some(receiver) = &decl.receiver {
            self.check_clause_placement(&receiver.clause, ClauseSite::Other, &decl.body)?;
        }
        self.push_scope();
        for param in &decl.params {
            self.check_clause_placement(&param.clause, ClauseSite::Other, &param.pattern)?;
            // Every name a pattern binds is a parameter name, so they share one scope and the same
            // duplicate rule. A `_` or a bare test binds nothing and declares nothing.
            for name in self.collect_matcher_binders(&param.pattern)? {
                self.declare(name, DeclKind::Param, &param.pattern)?;
            }
        }
        self.visit_expr(&decl.body)?;
        self.pop_scope();
        Ok(())
    }

    /// A catch clause. Its parameter and body share one scope.
    fn visit_catch(&mut self, catch: &CatchClause) -> Result<(), anyhow::Error> {
        self.push_scope();
        if let Some(param) = &catch.param {
            let Expr::Identifier(name) = self.ast.get(param) else { unreachable!("a catch parameter is an identifier") };
            self.declare(*name, DeclKind::Param, param)?;
        }
        let Expr::Block(stmts) = self.ast.get(&catch.body) else { unreachable!("a catch body is a block") };
        self.block(stmts)?;
        self.pop_scope();
        Ok(())
    }

    /// Resolves a `type` or `trait`.
    fn visit_type(&mut self, stmt: &AstId<Stmt>, decl: &TypeDecl) -> Result<(), anyhow::Error> {
        let with = self.flatten_traits(&decl.with_traits, stmt)?;
        // What a trait mixes is settled where it is declared. Recording it here stops a later composition
        // from re-resolving those names in its own scope, where they may mean something else.
        if decl.is_trait {
            let mut flattened = with.clone();
            flattened.push((decl.name, *stmt));
            self.trait_flatten_cache.insert(*stmt, flattened);
        }
        let req = self.resolve_reqs(decl, stmt)?;
        let gives = self.resolve_gives(decl, stmt)?;
        self.out.type_traits.insert(*stmt, ResolvedTraits { with, req, gives });

        for (_, clause) in &decl.field_clauses { self.check_clause_placement(clause, ClauseSite::Field, stmt)?; }
        // A required member holds a value the way a field does, so the same placement rules apply.
        for req in &decl.req_members { self.check_clause_placement(&req.clause, ClauseSite::Member, stmt)?; }
        for req_fn in &decl.req_fns {
            for param in &req_fn.params { self.collect_matcher_binders(&param.pattern)?; }
        }
        for method in &decl.methods { self.visit_stmt(method)?; }
        if let Some(init) = &decl.init { self.visit_stmt(init)?; }
        for (_, value) in &decl.field_inits { self.visit_expr(value)?; }
        Ok(())
    }

    fn visit_condition(&mut self, e: &AstId<Expr>) -> Result<(), anyhow::Error> {
        self.in_condition = true;
        self.visit_expr(e)
    }

    fn check_match_expr(&self, matcher: &AstId<Matcher>, in_condition: bool) -> Result<(), anyhow::Error> {
        let binders = self.collect_matcher_binders(matcher)?;
        if !binders.is_empty() && !in_condition {
            return Err(self.error("a `~` that binds names is only allowed in a condition", matcher));
        }
        Ok(())
    }

    fn visit_expr(&mut self, e: &AstId<Expr>) -> Result<(), anyhow::Error> {
        let in_condition = std::mem::replace(&mut self.in_condition, false);
        match self.ast.get(e) {
            Expr::Block(stmts) => {
                self.push_scope();
                self.block(stmts)?;
                self.pop_scope();
            },
            Expr::Unary(_, operand) => self.visit_expr(operand)?,
            Expr::Binary(op, left, right) => {
                let propagate = in_condition && matches!(op, Operator::LogicalAnd | Operator::LogicalOr);
                self.in_condition = propagate;
                self.visit_expr(left)?;
                self.in_condition = propagate;
                self.visit_expr(right)?;
            },
            Expr::Call(callee, args) => {
                self.visit_expr(callee)?;
                for arg in args { self.visit_expr(arg)?; }
            },
            Expr::Index(target, member, _) => { self.visit_expr(target)?; self.visit_expr(member)?; },
            Expr::Literal(lit) => self.visit_literal(lit)?,
            Expr::Identifier(name) => {
                if let Some(trait_id) = self.lookup_trait(*name) {
                    self.out.name_refs.insert(*e, Binding::Trait(trait_id));
                }
            },
            // `expr is T`: the right operand must be a static `type`/`trait` name (not a value).
            Expr::Is(target, name) => {
                self.visit_expr(target)?;
                if !self.is_type_or_trait(*name) {
                    return Err(self.error(format!("'{}' is not a type or trait", self.ast.text(*name)), e));
                }
            },
            Expr::Construct(callee, fields) => {
                self.visit_expr(callee)?;
                for (_, value) in fields { self.visit_expr(value)?; }
            },
            Expr::Mut(inner) => self.visit_expr(inner)?,
            Expr::This => {},
            Expr::SafeAccess(target, member, _) => { self.visit_expr(target)?; self.visit_expr(member)?; },
            Expr::SafeCall(target, args) => { 
                self.visit_expr(target)?;
                for arg in args { self.visit_expr(arg)?; }
            },
            Expr::Propagate(operand) => self.visit_expr(operand)?,
            Expr::Handle(scrutinee, name, handler) => {
                self.visit_expr(scrutinee)?;
                self.reject_builtin_name(*name, e)?;
                self.visit_expr(handler)?;
            },
            Expr::Assert(operand) => self.visit_expr(operand)?,
            Expr::Has(left, _) => self.visit_expr(left)?,
            Expr::Match(scrutinee, matcher) => {
                self.visit_expr(scrutinee)?;
                self.check_match_expr(matcher, in_condition)?;
            },
        }
        Ok(())
    }

    fn visit_literal(&mut self, lit: &Literal) -> Result<(), anyhow::Error> {
        match lit {
            Literal::Array(elems) => for elem in elems { self.visit_expr(elem)?; },
            Literal::Dict(pairs) => for (key, value) in pairs { self.visit_expr(key)?; self.visit_expr(value)?; },
            Literal::Lambda(decl) => self.visit_fn(decl)?,
            _ => {},
        }
        Ok(())
    }

    fn collect_matcher_binders(&self, id: &AstId<Matcher>) -> Result<IndexSet<Symbol>, anyhow::Error> {
        match self.ast.get(id) {
            Matcher::Wildcard | Matcher::Literal(_) => Ok(IndexSet::new()),
            Matcher::Binder(name) => {
                self.reject_builtin_name(*name, id)?;
                Ok(IndexSet::from([*name]))
            },
            Matcher::Type { name, shape, .. } => {
                if !self.is_type_or_trait(*name) {
                    return Err(self.error(format!("'{}' is not a type or trait", self.ast.text(*name)), id));
                }
                match shape {
                    Some(shape) => self.collect_matcher_binders(shape),
                    None => Ok(IndexSet::new()),
                }
            },
            Matcher::Shape(fields) => {
                let mut binders = IndexSet::new();
                for field in fields {
                    let sub = self.collect_matcher_binders(&field.value)?;
                    self.merge_distinct(&mut binders, sub, id)?;
                }
                Ok(binders)
            },
            Matcher::Array(elements) => {
                let mut binders = IndexSet::new();
                for element in elements {
                    let sub = match element {
                        MatchElem::Elem(matcher) => self.collect_matcher_binders(matcher)?,
                        MatchElem::Rest(Some(binder)) => self.collect_matcher_binders(binder)?,
                        MatchElem::Rest(None) => IndexSet::new(),
                    };
                    self.merge_distinct(&mut binders, sub, id)?;
                }
                Ok(binders)
            },
            Matcher::As(name, inner) => {
                // The node starts at the name, so the span is trimmed to it rather than the whole
                // `name @ m`.
                self.reject_builtin_at(*name, &self.name_span(*name, id))?;
                let mut binders = IndexSet::from([*name]);
                let sub = self.collect_matcher_binders(inner)?;
                self.merge_distinct(&mut binders, sub, id)?;
                Ok(binders)
            },
            Matcher::And(parts) => {
                let mut binders = IndexSet::new();
                for part in parts {
                    let sub = self.collect_matcher_binders(part)?;
                    self.merge_distinct(&mut binders, sub, id)?;
                }
                Ok(binders)
            },
            // Alternatives that bind names must agree on the set. A bindingless alternative may sit
            // beside a destructure if it's an obligation witness.
            Matcher::Or(alternatives) => {
                let mut binders: Option<IndexSet<Symbol>> = None;
                for alt in alternatives {
                    let set = self.collect_matcher_binders(alt)?;
                    if set.is_empty() { continue; }
                    match &binders {
                        Some(first) if *first != set =>
                            return Err(self.error("or-matcher alternatives must bind the same names", id)),
                        None => binders = Some(set),
                        _ => {},
                    }
                }
                Ok(binders.unwrap_or_default())
            },
        }
    }

    /// Folds one conjunctive part's binders into the running set, rejecting a name bound twice.
    fn merge_distinct(&self, into: &mut IndexSet<Symbol>, from: IndexSet<Symbol>, at: &AstId<Matcher>) -> Result<(), anyhow::Error> {
        for name in from {
            if !into.insert(name) {
                return Err(self.error(format!("binder '{}' is bound more than once in this matcher", self.ast.text(name)), at));
            }
        }
        Ok(())
    }

    /// The flattened `with`-set of a declaration: every transitively-composed trait, identity-deduped
    /// and post-ordered. Rejects unknown traits and composition cycles.
    fn flatten_traits(&mut self, with_traits: &[Symbol], stmt: &AstId<Stmt>) -> Result<Vec<(Symbol, AstId<Stmt>)>, anyhow::Error> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        let mut path = Vec::new();
        for trait_name in with_traits {
            for entry in self.flatten_trait(*trait_name, &mut path, stmt)? {
                if seen.insert(entry.1) { out.push(entry); }
            }
        }
        Ok(out)
    }

    /// The memoized flattened `with`-set of one trait (itself plus its transitive `with`-traits,
    /// deduped, post-ordered). Rejects composition cycles and unknown trait names. `path` carries
    /// the in-progress recursion stack for cycle detection.
    fn flatten_trait(&mut self, trait_name: Symbol, path: &mut Vec<Symbol>, stmt: &AstId<Stmt>) -> Result<Vec<(Symbol, AstId<Stmt>)>, anyhow::Error> {
        let Some(trait_stmt) = self.lookup_trait(trait_name) else {
            return Err(self.error(format!("Trait '{}' is not declared", self.ast.text(trait_name)), stmt));
        };
        if let Some(cached) = self.trait_flatten_cache.get(&trait_stmt) {
            return Ok(cached.clone());
        }
        if path.contains(&trait_name) {
            return Err(self.error(format!("Cyclic trait composition involving '{}'", self.ast.text(trait_name)), stmt));
        }
        let Stmt::Type(type_decl) = self.ast.get(&trait_stmt) else { unreachable!("trait scope holds only type/trait declarations") };
        path.push(trait_name);
        let mut out: Vec<(Symbol, AstId<Stmt>)> = Vec::new();
        let mut seen: HashSet<AstId<Stmt>> = HashSet::new();
        for sub in &type_decl.with_traits {
            for entry in self.flatten_trait(*sub, path, stmt)? {
                if seen.insert(entry.1) { out.push(entry); }
            }
        }
        path.pop();
        if seen.insert(trait_stmt) { out.push((trait_name, trait_stmt)); }
        self.trait_flatten_cache.insert(trait_stmt, out.clone());
        Ok(out)
    }

    /// Resolves a declaration's `req` traits to their declarations. A `trait`'s unknown `req` is a
    /// hard error (its surface depends on the trait).
    fn resolve_reqs(&self, decl: &TypeDecl, stmt: &AstId<Stmt>) -> Result<Vec<(Symbol, AstId<Stmt>)>, anyhow::Error> {
        let mut out = Vec::new();
        for trait_name in &decl.req_traits {
            match self.lookup_trait(*trait_name) {
                Some(id) => out.push((*trait_name, id)),
                None if decl.is_trait => return Err(self.error(format!("Trait '{}' is not declared", self.ast.text(*trait_name)), stmt)),
                None => {},
            }
        }
        Ok(out)
    }

    /// Resolves a declaration's `gives` delegations to their trait declarations. The right-hand
    /// name must be a declared trait (the forwarders are synthesized from it).
    fn resolve_gives(&self, decl: &TypeDecl, stmt: &AstId<Stmt>) -> Result<Vec<(Symbol, Symbol, AstId<Stmt>)>, anyhow::Error> {
        let mut out = Vec::new();
        for (field, trait_name) in &decl.gives {
            match self.lookup_trait(*trait_name) {
                Some(id) => out.push((*field, *trait_name, id)),
                None => return Err(self.error(format!("Trait '{}' is not declared", self.ast.text(*trait_name)), stmt)),
            }
        }
        Ok(out)
    }
}

fn builtin_clause_guidance(name: &str) -> &'static str {
    match name {
        "opt" => "declare what a narrowing leaves behind, not the value that may be `null`",
        "fails" => "store what the `Err` carries, not the `Err` itself",
        _ => "declare what a discharge leaves behind, not the value that owes the obligation",
    }
}
