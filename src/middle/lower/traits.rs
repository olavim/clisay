//! Trait composition.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::ast::{AstId, Expr, Stmt, Symbol};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::HirTypeDecl;

use super::Lowerer;

struct ComposedMembers {
    fields: HashSet<Symbol>,
    field_inits: Vec<(Symbol, AstId<Expr>)>,
    methods: Vec<AstId<Stmt>>,
    pub_members: HashSet<Symbol>,
    trait_inits: Vec<Symbol>,
    seen: HashSet<Symbol>,
    path: Vec<Symbol>,
}

impl ComposedMembers {
    fn new(decl: &crate::ast::TypeDecl) -> ComposedMembers {
        ComposedMembers {
            fields: decl.fields.clone(),
            field_inits: decl.field_inits.clone(),
            methods: decl.methods.clone(),
            pub_members: decl.pub_members.clone(),
            trait_inits: Vec::new(),
            seen: HashSet::new(),
            path: Vec::new(),
        }
    }
}

impl<'a> Lowerer<'a> {
    pub(super) fn scan_traits(&self, stmts: &[AstId<Stmt>]) -> Result<HashMap<Symbol, AstId<Stmt>>, anyhow::Error> {
        let mut traits: HashMap<Symbol, AstId<Stmt>> = HashMap::new();
        let mut seen: HashMap<Symbol, bool> = HashMap::new(); // name -> was declared as a trait
        for s in stmts {
            let (name, is_trait) = match self.ast.get(s) {
                Stmt::Type(decl) => (decl.name, decl.is_trait),
                Stmt::Fn(decl) => (decl.name, false),
                Stmt::Say(field) => (field.name, false),
                _ => continue,
            };
            if let Some(&prev_is_trait) = seen.get(&name) {
                if is_trait || prev_is_trait {
                    return Err(self.error(format!("'{}' already declared in this scope", self.hir.text(name)), s));
                }
            }
            seen.insert(name, is_trait);
            if is_trait { traits.insert(name, *s); }
        }
        Ok(traits)
    }

    pub(super) fn lookup_trait(&self, name: Symbol) -> Option<AstId<Stmt>> {
        self.trait_scopes.iter().rev().find_map(|scope| scope.get(&name).copied())
    }

    pub(super) fn is_trait_in_scope(&self, name: Symbol) -> bool {
        self.trait_scopes.iter().any(|scope| scope.contains_key(&name))
    }

    pub(super) fn lower_type(&mut self, decl: &crate::ast::TypeDecl, type_pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        self.check_init_orchestration(decl, type_pos)?;

        let mut members = ComposedMembers::new(decl);
        for trait_name in &decl.with_traits {
            self.splice_trait(*trait_name, &mut members, type_pos)?;
        }

        let init = self.lower_type_init(decl, &members.field_inits, type_pos)?;
        let getter = match &decl.getter {
            Some(stmt) => Some(self.stmt(stmt)?),
            None => None,
        };
        let setter = match &decl.setter {
            Some(stmt) => Some(self.stmt(stmt)?),
            None => None,
        };
        let mut methods = members.methods.iter().map(|m| self.stmt(m)).collect::<Result<Vec<_>, _>>()?;
        for trait_sym in &members.trait_inits {
            let init_method = self.lower_trait_init(*trait_sym)?;
            methods.push(init_method);
        }

        Ok(HirTypeDecl {
            name: decl.name,
            superclass: decl.superclass,
            init,
            getter,
            setter,
            fields: members.fields,
            methods,
            pub_members: members.pub_members,
        })
    }

    fn splice_trait(&self, name: Symbol, members: &mut ComposedMembers, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        if members.seen.contains(&name) {
            return Ok(()); // already folded in via another path, dedupe
        }
        if members.path.contains(&name) {
            return Err(anyhow!("Cyclic trait composition involving '{}'\n\tat {}", self.hir.text(name), pos));
        }
        let Some(trait_stmt) = self.lookup_trait(name) else {
            return Err(anyhow!("Trait '{}' is not declared\n\tat {}", self.hir.text(name), pos));
        };
        let trait_decl = self.ast_type(&trait_stmt);

        members.path.push(name);
        for sub in &trait_decl.with_traits {
            self.splice_trait(*sub, members, pos)?;
        }
        for f in &trait_decl.fields { members.fields.insert(*f); }
        for fi in &trait_decl.field_inits { members.field_inits.push(*fi); }
        for m in &trait_decl.methods { members.methods.push(*m); }
        for p in &trait_decl.pub_members { members.pub_members.insert(*p); }
        // A trait with a declared `init` contributes a `"<Trait>.init"` method.
        if trait_decl.init.is_some() {
            members.trait_inits.push(name);
        }
        members.path.pop();
        members.seen.insert(name);
        Ok(())
    }
}
