//! Trait composition: lexical trait scoping, `with`-expansion (member folding), exposed-member
//! collision detection, and qualified `T.method(...)` calls.
//!
//! Traits are compile-time splice-templates. A `type`/`trait` that mixes traits via `with` has
//! every trait's members folded into it here, so the resolver and codegen downstream see a
//! self-contained type.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::ast::{AstId, Expr, Literal, Stmt, Symbol, TypeDecl};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirStmt, HirTypeDecl};

use super::Lowerer;

struct Composed {
    fields: HashSet<Symbol>,
    field_inits: Vec<(Symbol, AstId<Expr>)>,
    pub_members: HashSet<Symbol>,
    methods: Vec<HirId<HirStmt>>,
    /// The declaring trait of each entry in `methods` (parallel); `None` = host-declared.
    method_traits: Vec<Option<Symbol>>,
    /// Per trait, its private members' plain name → renamed slot symbol.
    trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    trait_inits: Vec<Symbol>,
}

impl Composed {
    /// Seeds the accumulator with the host type's own declared members.
    fn seed(decl: &TypeDecl) -> Composed {
        Composed {
            fields: decl.fields.clone(),
            field_inits: decl.field_inits.clone(),
            pub_members: decl.pub_members.clone(),
            methods: Vec::new(),
            method_traits: Vec::new(),
            trait_privates: HashMap::new(),
            trait_inits: Vec::new(),
        }
    }
}

/// Whether a member is exposed (`inner` or `pub`). A member in neither set is private (per-trait).
fn is_exposed(td: &TypeDecl, name: &Symbol) -> bool {
    td.pub_members.contains(name) || td.inner_members.contains(name)
}

impl<'a> Lowerer<'a> {
    /// Collects the `trait` declarations directly in `stmts` (one block's scope). Traits
    /// share the one-name-per-scope namespace with other names (`say`/`fn`/`type`).
    /// However, traits are lowered away here and don't reach the normal name collision checks
    /// in name resolution, so we'll have to check for trait/non-trait name clashes here.
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
                    // The second declaration is the error site, matching `declare_local`.
                    return Err(self.error(format!("'{}' already declared in this scope", self.hir.text(name)), s));
                }
                // A non-trait/non-trait clash is left to `declare_local`.
            }
            seen.insert(name, is_trait);
            if is_trait { traits.insert(name, *s); }
        }
        Ok(traits)
    }

    /// Resolves a trait name against the lexical scope stack, innermost-first.
    pub(super) fn lookup_trait(&self, name: Symbol) -> Option<AstId<Stmt>> {
        self.trait_scopes.iter().rev().find_map(|scope| scope.get(&name).copied())
    }

    /// Whether `name` refers to a trait in scope (traits aren't usable as values).
    pub(super) fn is_trait_in_scope(&self, name: Symbol) -> bool {
        self.trait_scopes.iter().any(|scope| scope.contains_key(&name))
    }

    /// Lowers a `type` declaration: flatten the `with`-set, check exposed-member collisions,
    /// fold every trait's members in (renaming private members per trait), then lower the
    /// composer's own init, accessors, methods, and each `with`-trait's init method.
    pub(super) fn lower_type(&mut self, decl: &TypeDecl, type_pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        self.check_init_orchestration(decl, type_pos)?;

        // The flattened `with`-set (identity dedupe, cycle detection).
        let traits = self.collect_traits(decl, type_pos)?;
        let host_methods: HashSet<Symbol> = decl.methods.iter().map(|m| self.ast_fn(m).name).collect();
        let exposed_methods = self.check_exposed_collisions(&traits, &host_methods, decl, type_pos)?;

        // Install the composer's context: which traits are provided (for qualified `T.method(...)`)
        // and which qualified aliases exist.
        let aliases = self.override_aliases(&exposed_methods, &host_methods);
        let prev_provided = std::mem::replace(&mut self.provided_traits, traits.iter().map(|(s, _)| *s).collect());
        let prev_aliases = std::mem::replace(&mut self.emitted_aliases, aliases);

        let mut composed = Composed::seed(decl);
        // The host's own methods (host members are plain; no trait scope).
        for m in &decl.methods {
            let lowered = self.stmt(m)?;
            composed.methods.push(lowered);
            composed.method_traits.push(None);
        }
        for (trait_sym, td) in &traits {
            self.fold_trait(*trait_sym, td, &host_methods, &mut composed)?;
        }

        let init = self.lower_type_init(decl, &composed.field_inits, type_pos)?;
        let getter = decl.getter.as_ref().map(|stmt| self.stmt(stmt)).transpose()?;
        let setter = decl.setter.as_ref().map(|stmt| self.stmt(stmt)).transpose()?;
        // Each `with`-mixed trait that declares an init contributes a `"<Trait>.init"` method.
        for trait_sym in std::mem::take(&mut composed.trait_inits) {
            let init_method = self.lower_trait_init(trait_sym)?;
            composed.methods.push(init_method);
            composed.method_traits.push(Some(trait_sym));
        }

        // Restore the previous composer context so sibling types in the same scope
        // don't see this type's traits or aliases.
        self.provided_traits = prev_provided;
        self.emitted_aliases = prev_aliases;

        Ok(HirTypeDecl {
            name: decl.name,
            superclass: decl.superclass,
            init,
            getter,
            setter,
            fields: composed.fields,
            methods: composed.methods,
            method_traits: composed.method_traits,
            pub_members: composed.pub_members,
            trait_privates: composed.trait_privates,
        })
    }

    /// Checks exposed-member collisions across a flattened trait set, returning the exposed
    /// methods grouped by name. A method clash is resolvable by a host override, but field
    /// clashes and unresolved method clashes are errors.
    fn check_exposed_collisions(&self, traits: &[(Symbol, &'a TypeDecl)], host_methods: &HashSet<Symbol>, decl: &TypeDecl, pos: &SourcePosition) -> Result<HashMap<Symbol, Vec<Symbol>>, anyhow::Error> {
        let mut exposed_methods: HashMap<Symbol, Vec<Symbol>> = HashMap::new();
        let mut exposed_fields: HashMap<Symbol, Vec<Symbol>> = HashMap::new();
        for (trait_sym, type_decl) in traits {
            for field in &type_decl.fields {
                if is_exposed(type_decl, field) { exposed_fields.entry(*field).or_default().push(*trait_sym); }
            }
            for method in &type_decl.methods {
                let name = self.ast_fn(method).name;
                if is_exposed(type_decl, &name) { exposed_methods.entry(name).or_default().push(*trait_sym); }
            }
        }
        for (name, providers) in &exposed_methods {
            if !host_methods.contains(name) && providers.len() >= 2 {
                return Err(anyhow!("Exposed method '{}' clashes between traits {} — the host type must declare its own '{}' to resolve it\n\tat {}",
                    self.hir.text(*name), self.trait_list(providers), self.hir.text(*name), pos));
            }
        }
        for (name, providers) in &exposed_fields {
            if providers.len() + decl.fields.contains(name) as usize >= 2 {
                return Err(anyhow!("Exposed field '{}' clashes between {} — rename one or make it private\n\tat {}",
                    self.hir.text(*name), self.field_clash_sources(providers, decl.fields.contains(name)), pos));
            }
        }
        Ok(exposed_methods)
    }

    /// The `"<Trait>.<method>"` aliases for exposed methods a host declaration overrides.
    fn override_aliases(&self, exposed_methods: &HashMap<Symbol, Vec<Symbol>>, host_methods: &HashSet<Symbol>) -> HashSet<String> {
        let mut aliases = HashSet::new();
        for (name, providers) in exposed_methods {
            if host_methods.contains(name) {
                for trait_sym in providers {
                    aliases.insert(format!("{}.{}", self.hir.text(*trait_sym), self.hir.text(*name)));
                }
            }
        }
        aliases
    }

    /// Folds one flattened trait's members into `composed`. Exposed members take their plain name
    /// (or a `"<Trait>.<method>"` alias when a host override shadows them); private members take a
    /// per-trait slot name so two traits' same-named privates never collide (the resolver scopes
    /// access to them, recorded in `trait_privates`).
    fn fold_trait(&mut self, trait_sym: Symbol, td: &TypeDecl, host_methods: &HashSet<Symbol>, composed: &mut Composed) -> Result<(), anyhow::Error> {
        let renames = self.trait_renames(td);
        let mut private_map: HashMap<Symbol, Symbol> = HashMap::new();

        for f in &td.fields {
            if is_exposed(td, f) {
                composed.fields.insert(*f);
                if td.pub_members.contains(f) { composed.pub_members.insert(*f); }
            } else {
                let renamed = self.hir.intern(&renames[self.hir.text(*f)]);
                composed.fields.insert(renamed);
                private_map.insert(*f, renamed);
            }
        }
        for (f, value) in &td.field_inits {
            let slot = if is_exposed(td, f) { *f } else { self.hir.intern(&renames[self.hir.text(*f)]) };
            composed.field_inits.push((slot, *value));
        }

        let tname = self.hir.text(trait_sym).to_string();
        for m in &td.methods {
            let name = self.ast_fn(m).name;
            let name_text = self.hir.text(name).to_string();
            let slot = if !is_exposed(td, &name) {
                let renamed = self.hir.intern(&renames[&name_text]);
                private_map.insert(name, renamed);
                renamed
            } else if host_methods.contains(&name) {
                self.hir.intern(&format!("{}.{}", tname, name_text))
            } else {
                if td.pub_members.contains(&name) { composed.pub_members.insert(name); }
                name
            };
            let lowered = self.lower_method_named(m, slot)?;
            composed.methods.push(lowered);
            composed.method_traits.push(Some(trait_sym));
        }

        composed.trait_privates.insert(trait_sym, private_map);
        if td.init.is_some() { composed.trait_inits.push(trait_sym); }
        Ok(())
    }

    /// The flattened `with`-set of `decl`: every transitively-composed trait, identity-deduped
    /// and post-ordered (a trait's `with`-mixed sub-traits precede it). Rejects composition cycles
    /// and unknown trait names.
    fn collect_traits(&self, decl: &TypeDecl, pos: &SourcePosition) -> Result<Vec<(Symbol, &'a TypeDecl)>, anyhow::Error> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        let mut path = Vec::new();
        for t in &decl.with_traits {
            self.collect_trait(*t, &mut out, &mut seen, &mut path, pos)?;
        }
        Ok(out)
    }

    fn collect_trait(&self, name: Symbol, out: &mut Vec<(Symbol, &'a TypeDecl)>, seen: &mut HashSet<Symbol>, path: &mut Vec<Symbol>, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        if seen.contains(&name) {
            return Ok(()); // already folded in via another path, dedupe
        }
        if path.contains(&name) {
            return Err(anyhow!("Cyclic trait composition involving '{}'\n\tat {}", self.hir.text(name), pos));
        }
        let Some(trait_stmt) = self.lookup_trait(name) else {
            return Err(anyhow!("Trait '{}' is not declared\n\tat {}", self.hir.text(name), pos));
        };
        let td = self.ast_type(&trait_stmt);
        path.push(name);
        for sub in &td.with_traits {
            self.collect_trait(*sub, out, seen, path, pos)?;
        }
        path.pop();
        seen.insert(name);
        out.push((name, td));
        Ok(())
    }

    /// The private-member rename map for a trait:
    /// each private field or method name -> its per-trait form `"<Trait>.<name>"`.
    pub(super) fn trait_renames(&self, td: &TypeDecl) -> HashMap<String, String> {
        let tname = self.hir.text(td.name).to_string();
        let mut map = HashMap::new();
        let private_names = td.fields.iter().copied().chain(td.methods.iter().map(|m| self.ast_fn(m).name));
        for name in private_names {
            if !is_exposed(td, &name) {
                let txt = self.hir.text(name).to_string();
                map.insert(txt.clone(), format!("{}.{}", tname, txt));
            }
        }
        map
    }

    /// Lowers a method declaration under a given (possibly renamed) member name.
    fn lower_method_named(&mut self, fn_stmt: &AstId<Stmt>, name: Symbol) -> Result<HirId<HirStmt>, anyhow::Error> {
        let pos = self.ast.pos(fn_stmt).clone();
        let decl = self.ast_fn(fn_stmt);
        let params = self.exprs(&decl.params)?;
        let body = self.expr(&decl.body)?;
        Ok(self.hir.add(HirStmt::Fn(HirFnDecl { name, params, body }), pos))
    }

    pub(super) fn as_qualified_method_call(&self, callee: &AstId<Expr>) -> Option<(Symbol, String)> {
        let Expr::Index(target, member, true) = self.ast.get(callee) else { return None };
        let Expr::Identifier(t) = self.ast.get(target) else { return None };
        if !self.is_trait_in_scope(*t) { return None; }
        let Expr::Literal(Literal::String(m)) = self.ast.get(member) else { return None };
        if m == "init" { return None; } // init orchestration has its own path
        Some((*t, m.clone()))
    }

    /// Builds the HIR for `Trait.method(args)`: runs `Trait`'s version of `method` with the
    /// current `this` implicit. Valid only for a trait the enclosing type provides. Resolves to
    /// the qualified alias when a host override shadowed the plain name, else to the plain method.
    pub(super) fn qualified_method_call(&mut self, trait_sym: Symbol, method: &str, args: Vec<HirId<HirExpr>>, callee: &AstId<Expr>, pos: &SourcePosition) -> Result<HirExpr, anyhow::Error> {
        if !self.provided_traits.contains(&trait_sym) {
            return Err(self.error(format!("'{}.{}(...)': '{}' is not a trait provided by this type",
                self.hir.text(trait_sym), method, self.hir.text(trait_sym)), callee));
        }
        let alias = format!("{}.{}", self.hir.text(trait_sym), method);
        let target_name = if self.emitted_aliases.contains(&alias) { alias } else { method.to_string() };
        Ok(HirExpr::Call(self.this_method(&target_name, pos), args))
    }

    fn trait_list(&self, traits: &[Symbol]) -> String {
        traits.iter().map(|t| format!("'{}'", self.hir.text(*t))).collect::<Vec<_>>().join(" and ")
    }

    fn field_clash_sources(&self, traits: &[Symbol], host: bool) -> String {
        let mut parts: Vec<String> = traits.iter().map(|t| format!("trait '{}'", self.hir.text(*t))).collect();
        if host { parts.push("the host type".to_string()); }
        parts.join(" and ")
    }
}
