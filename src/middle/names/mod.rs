//! Name resolution.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::ast::{Ast, AstId, CatchClause, Expr, FnDecl, Literal, Stmt, Symbol, TypeDecl};

pub enum Binding {
    Trait(AstId<Stmt>),
}

struct ResolvedTraits {
    with: Vec<(Symbol, AstId<Stmt>)>,
    req: Vec<(Symbol, AstId<Stmt>)>,
    gives: Vec<(Symbol, Symbol, AstId<Stmt>)>,
}

pub struct NameBindings {
    type_traits: HashMap<AstId<Stmt>, ResolvedTraits>,
    name_refs: HashMap<AstId<Expr>, Binding>,
}

impl NameBindings {
    pub fn flattened_with(&self, ty: &AstId<Stmt>) -> &[(Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.with)
    }

    pub fn req_traits(&self, ty: &AstId<Stmt>) -> &[(Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.req)
    }

    pub fn gives_traits(&self, ty: &AstId<Stmt>) -> &[(Symbol, Symbol, AstId<Stmt>)] {
        self.type_traits.get(ty).map_or(&[], |rt| &rt.gives)
    }

    /// The trait declaration an expression names.
    pub fn trait_ref(&self, expr: AstId<Expr>) -> Option<AstId<Stmt>> {
        match self.name_refs.get(&expr) {
            Some(Binding::Trait(id)) => Some(*id),
            None => None,
        }
    }
}

pub fn resolve(ast: &Ast) -> Result<NameBindings, anyhow::Error> {
    let mut resolver = Resolver {
        ast,
        scopes: Vec::new(),
        trait_flatten_cache: HashMap::new(),
        out: NameBindings { type_traits: HashMap::new(), name_refs: HashMap::new() },
    };
    resolver.visit_stmt(&ast.get_root())?;
    Ok(resolver.out)
}

struct Scope {
    declared: HashSet<Symbol>,
    traits: HashMap<Symbol, AstId<Stmt>>,
    types: HashSet<Symbol>,
}

struct Resolver<'a> {
    ast: &'a Ast,
    scopes: Vec<Scope>,
    trait_flatten_cache: HashMap<AstId<Stmt>, Vec<(Symbol, AstId<Stmt>)>>,
    out: NameBindings,
}

impl<'a> Resolver<'a> {
    fn error<T>(&self, msg: impl Into<String>, at: &AstId<T>) -> anyhow::Error {
        anyhow!("{}\n\tat {}", msg.into(), self.ast.pos(at))
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope { declared: HashSet::new(), traits: HashMap::new(), types: HashSet::new() });
    }

    fn is_type_or_trait(&self, name: Symbol) -> bool {
        self.scopes.iter().any(|scope| scope.types.contains(&name))
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_trait(&self, name: Symbol) -> Option<AstId<Stmt>> {
        self.scopes.iter().rev().find_map(|scope| scope.traits.get(&name).copied())
    }

    fn declare<T>(&mut self, name: Symbol, at: &AstId<T>) -> Result<(), anyhow::Error> {
        if !self.scopes.last_mut().unwrap().declared.insert(name) {
            return Err(self.error(format!("'{}' already declared in this scope", self.ast.text(name)), at));
        }
        Ok(())
    }

    fn hoist_types(&mut self, stmts: &[AstId<Stmt>]) {
        for stmt in stmts {
            if let Stmt::Type(decl) = self.ast.get(stmt) {
                let scope = self.scopes.last_mut().unwrap();
                scope.types.insert(decl.name);
                if decl.is_trait {
                    scope.traits.insert(decl.name, *stmt);
                }
            }
        }
    }

    fn decl_name(&self, stmt: &AstId<Stmt>) -> Option<Symbol> {
        match self.ast.get(stmt) {
            Stmt::Type(decl) => Some(decl.name),
            Stmt::Fn(decl) => Some(decl.name),
            Stmt::Say(field) => Some(field.name),
            _ => None,
        }
    }

    fn block(&mut self, stmts: &[AstId<Stmt>]) -> Result<(), anyhow::Error> {
        self.hoist_types(stmts);
        for stmt in stmts {
            if let Some(name) = self.decl_name(stmt) {
                self.declare(name, stmt)?;
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
            Stmt::While(cond, body) => { self.visit_expr(cond)?; self.visit_expr(body)?; },
            Stmt::If(cond, then, otherwise) => {
                self.visit_expr(cond)?;
                self.visit_expr(then)?;
                if let Some(otherwise) = otherwise { self.visit_stmt(otherwise)?; }
            },
            Stmt::Block(body) => self.visit_expr(body)?,
            Stmt::Say(field) => if let Some(value) = &field.value { self.visit_expr(value)?; },
            Stmt::Fn(decl) => self.visit_fn(decl)?,
            Stmt::Type(decl) => self.visit_type(stmt, decl)?,
        }
        Ok(())
    }

    fn visit_fn(&mut self, decl: &FnDecl) -> Result<(), anyhow::Error> {
        self.push_scope();
        for param in &decl.params {
            let Expr::Identifier(name) = self.ast.get(&param.name) else { unreachable!("a parameter is an identifier") };
            self.declare(*name, &param.name)?;
        }
        self.visit_expr(&decl.body)?;
        self.pop_scope();
        Ok(())
    }

    fn visit_catch(&mut self, catch: &CatchClause) -> Result<(), anyhow::Error> {
        self.push_scope();
        if let Some(param) = &catch.param {
            let Expr::Identifier(name) = self.ast.get(param) else { unreachable!("a catch parameter is an identifier") };
            self.declare(*name, param)?;
        }
        let Expr::Block(stmts) = self.ast.get(&catch.body) else { unreachable!("a catch body is a block") };
        self.block(stmts)?;
        self.pop_scope();
        Ok(())
    }

    fn visit_type(&mut self, stmt: &AstId<Stmt>, decl: &TypeDecl) -> Result<(), anyhow::Error> {
        let with = self.flatten_traits(&decl.with_traits, stmt)?;
        let req = self.resolve_reqs(decl, stmt)?;
        let gives = self.resolve_gives(decl, stmt)?;
        self.out.type_traits.insert(*stmt, ResolvedTraits { with, req, gives });

        for method in &decl.methods { self.visit_stmt(method)?; }
        if let Some(init) = &decl.init { self.visit_stmt(init)?; }
        for (_, value) in &decl.field_inits { self.visit_expr(value)?; }
        Ok(())
    }

    fn visit_expr(&mut self, e: &AstId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(e) {
            Expr::Block(stmts) => {
                self.push_scope();
                self.block(stmts)?;
                self.pop_scope();
            },
            Expr::Unary(_, operand) => self.visit_expr(operand)?,
            Expr::Binary(_, left, right) => { self.visit_expr(left)?; self.visit_expr(right)?; },
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
            Expr::This => {},
            Expr::SafeAccess(target, member, _) => { self.visit_expr(target)?; self.visit_expr(member)?; },
            Expr::Assert(operand) => self.visit_expr(operand)?,
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

    fn flatten_traits(&mut self, with_traits: &[Symbol], stmt: &AstId<Stmt>) -> Result<Vec<(Symbol, AstId<Stmt>)>, anyhow::Error> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        let mut path = Vec::new();
        for trait_name in with_traits {
            for entry in self.flatten_trait(*trait_name, &mut path, stmt)? {
                if seen.insert(entry.0) { out.push(entry); }
            }
        }
        Ok(out)
    }

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
        let mut seen: HashSet<Symbol> = HashSet::new();
        for sub in &type_decl.with_traits {
            for entry in self.flatten_trait(*sub, path, stmt)? {
                if seen.insert(entry.0) { out.push(entry); }
            }
        }
        path.pop();
        if seen.insert(trait_name) { out.push((trait_name, trait_stmt)); }
        self.trait_flatten_cache.insert(trait_stmt, out.clone());
        Ok(out)
    }

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
