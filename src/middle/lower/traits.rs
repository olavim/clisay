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
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl};

use super::Lowerer;

struct Composed {
    fields: HashSet<Symbol>,
    field_inits: Vec<(Symbol, AstId<Expr>)>,
    pub_members: HashSet<Symbol>,
    methods: Vec<HirId<HirStmt>>,
    /// The declaring trait of each entry in `methods` (parallel); `None` = host-declared.
    method_traits: Vec<Option<Symbol>>,
    /// Per trait, its private methods' plain name -> renamed slot symbol.
    trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
}

impl Composed {
    /// An empty accumulator. A standalone trait folds its own members into one of these.
    fn empty() -> Composed {
        Composed {
            fields: HashSet::new(),
            field_inits: Vec::new(),
            pub_members: HashSet::new(),
            methods: Vec::new(),
            method_traits: Vec::new(),
            trait_privates: HashMap::new(),
        }
    }

    /// Seeds the accumulator with the host type's own declared members (plain, un-scoped).
    fn seed(decl: &TypeDecl) -> Composed {
        Composed {
            fields: decl.fields.clone(),
            field_inits: decl.field_inits.clone(),
            pub_members: decl.pub_members.clone(),
            ..Composed::empty()
        }
    }
}

/// Whether a member is exposed (`inner` or `pub`). A member in neither set is private (per-trait).
fn is_exposed(type_decl: &TypeDecl, name: &Symbol) -> bool {
    type_decl.pub_members.contains(name) || type_decl.inner_members.contains(name)
}

impl<'a> Lowerer<'a> {
    /// Lowers a `type` declaration: flatten the `with`-set, check exposed-member collisions,
    /// fold every trait's members in (renaming private members per trait), then lower the
    /// composer's own init, accessors, methods, and each `with`-trait's init method.
    pub(super) fn lower_type(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl, type_pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        // The flattened `with`-set, resolved by the `names` pre-pass.
        let traits = self.flattened_with(type_id);
        self.check_provide_require_exclusive(decl, type_pos)?;
        self.check_provide_once(type_id, decl, type_pos)?;
        // Traits provided by delegation (`field gives Trait`): they satisfy `req T` and `is T`.
        let gives_traits: Vec<Symbol> = self.names.gives_traits(&type_id).iter().map(|(_, t, _)| *t).collect();
        self.check_requirements(decl, &traits, &gives_traits, type_pos)?;
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
        // `field gives Trait`: synthesize a forwarder per exposed trait method (host methods of the
        // same name are overrides and keep their own definition).
        self.lower_gives(type_id, &host_methods, type_pos, &mut composed)?;

        let init = self.lower_type_init(type_id, decl, &composed.field_inits, type_pos)?;

        // Restore the previous composer context so sibling types in the same scope
        // don't see this type's traits or aliases.
        self.provided_traits = prev_provided;
        self.emitted_aliases = prev_aliases;

        // What `x is T` matches: the type's own name, every transitively `with`-mixed trait, and
        // every trait it provides by `gives` delegation.
        let provides = std::iter::once(decl.name)
            .chain(self.names.flattened_with(&type_id).iter().map(|(sym, _)| *sym))
            .chain(gives_traits.iter().copied())
            .collect();

        Ok(HirTypeDecl {
            name: decl.name,
            init,
            fields: composed.fields,
            methods: composed.methods,
            method_traits: composed.method_traits,
            pub_members: composed.pub_members,
            trait_privates: composed.trait_privates,
            surface: HashSet::new(), // gating applies to standalone traits, not composed types
            provides,
        })
    }

    /// Lowers a `trait` declaration into a standalone `HirTypeDecl` for **self-containment validation**.
    /// The resolver validates the body against the trait's surface independently of any composing type.
    pub(super) fn lower_trait(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl, pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        let surface = self.trait_surface(type_id, decl)?;

        let mut composed = Composed::empty();
        self.fold_trait(decl.name, decl, &HashSet::new(), &mut composed)?;

        // A placeholder empty init: a trait's init body is validated at the composing type.
        let empty = self.hir.add(HirExpr::Block(Vec::new()), pos.clone());
        let init = self.hir.add(HirStmt::Fn(HirFnDecl { name: decl.init_name, params: Vec::new(), body: empty }), pos.clone());

        Ok(HirTypeDecl {
            name: decl.name,
            init,
            fields: composed.fields,
            methods: composed.methods,
            method_traits: composed.method_traits,
            pub_members: composed.pub_members,
            trait_privates: composed.trait_privates,
            surface,
            provides: Vec::new(), // a standalone trait emits no runtime type
        })
    }

    /// The set of member names a trait's body may reach through `this`: its own members, the
    /// exposed (`inner`/`pub`) members of every trait it `with`-flattens or `req`-depends on
    /// (and *their* `with`-provided members), and its own `req fn` / `req <member>` holes.
    fn trait_surface(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl) -> Result<HashSet<Symbol>, anyhow::Error> {
        let mut surface: HashSet<Symbol> = HashSet::new();
        for field in &decl.fields { surface.insert(*field); }
        for method in &decl.methods { surface.insert(self.ast_fn(method).name); }
        for (name, _) in &decl.req_fns { surface.insert(*name); }
        for name in &decl.req_members { surface.insert(*name); }

        // Exposed members provided through `with` (transitively).
        for (_, type_decl) in &self.flattened_with(type_id) {
            self.add_exposed(type_decl, &mut surface);
        }
        // Exposed members of each `req`-depended trait (and that trait's `with`-provided set);
        // both resolved by the `names` pre-pass.
        for (_, req_id) in self.names.req_traits(&type_id) {
            self.add_exposed(self.ast_type(req_id), &mut surface);
            for (_, type_decl) in &self.flattened_with(*req_id) {
                self.add_exposed(type_decl, &mut surface);
            }
        }
        Ok(surface)
    }

    fn add_exposed(&self, type_decl: &TypeDecl, surface: &mut HashSet<Symbol>) {
        for name in type_decl.pub_members.iter().chain(type_decl.inner_members.iter()) {
            surface.insert(*name);
        }
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
                return Err(anyhow!("Exposed method '{}' clashes between traits {}; declare '{}' in the host type to resolve it\n\tat {}",
                    self.hir.text(*name), self.trait_list(providers), self.hir.text(*name), pos));
            }
        }
        for (name, providers) in &exposed_fields {
            if providers.len() + decl.fields.contains(name) as usize >= 2 {
                return Err(anyhow!("Exposed field '{}' clashes between {}; rename one or make it private\n\tat {}",
                    self.hir.text(*name), self.field_clash_sources(providers, decl.fields.contains(name)), pos));
            }
        }
        Ok(exposed_methods)
    }

    /// `req T` and `with T` are mutually exclusive on one composer. You cannot both provide
    /// and depend on the same trait. Applies to types and traits alike.
    pub(super) fn check_provide_require_exclusive(&self, decl: &TypeDecl, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        for trait_sym in &decl.req_traits {
            if decl.with_traits.contains(trait_sym) {
                return Err(anyhow!("Trait '{}' appears in both `with` and `req`; keep only one\n\tat {}",
                    self.hir.text(*trait_sym), pos));
            }
            if decl.gives.iter().any(|(_, t)| t == trait_sym) {
                return Err(anyhow!("Trait '{}' appears in both `req` and `gives`; keep only one\n\tat {}",
                    self.hir.text(*trait_sym), pos));
            }
        }
        Ok(())
    }

    /// A trait may be *declared* as provided at most once across the composer's direct `with` and `gives` clauses.
    fn check_provide_once(&self, type_id: AstId<Stmt>, decl: &TypeDecl, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        let with: HashSet<Symbol> = decl.with_traits.iter().copied().collect();
        let mut given: HashSet<Symbol> = HashSet::new();
        for (_, trait_sym, _) in self.names.gives_traits(&type_id) {
            if with.contains(trait_sym) {
                return Err(anyhow!("Trait '{}' appears in both `with` and `gives`; keep only one\n\tat {}",
                    self.hir.text(*trait_sym), pos));
            }
            if !given.insert(*trait_sym) {
                return Err(anyhow!("Trait '{}' appears in `gives` more than once; keep only one\n\tat {}",
                    self.hir.text(*trait_sym), pos));
            }
        }
        Ok(())
    }

    /// Synthesizes a forwarding method for each exposed method of every `gives` trait's surface
    /// (the trait's own methods plus its `with`-flattened set). Each forwarder calls
    /// `this.<field>.<method>(args)` and carries the trait method's visibility. A method the host
    /// already declares is an override.
    fn lower_gives(&mut self, type_id: AstId<Stmt>, host_methods: &HashSet<Symbol>, pos: &SourcePosition, composed: &mut Composed) -> Result<(), anyhow::Error> {
        let mut forwarded: HashSet<Symbol> = HashSet::new();
        for (field, _, trait_id) in self.names.gives_traits(&type_id).to_vec() {
            let mut decls = self.flattened_with(trait_id);
            decls.push((self.ast_type(&trait_id).name, self.ast_type(&trait_id)));
            for (_, type_decl) in decls {
                for method in &type_decl.methods {
                    let fd = self.ast_fn(method);
                    let name = fd.name;
                    if !is_exposed(type_decl, &name) { continue; }
                    if host_methods.contains(&name) { continue; }
                    if !forwarded.insert(name) { continue; }
                    let arity = fd.params.len();
                    let is_pub = type_decl.pub_members.contains(&name);
                    let forwarder = self.make_forwarder(field, name, arity, pos);
                    composed.methods.push(forwarder);
                    composed.method_traits.push(None);
                    if is_pub { composed.pub_members.insert(name); }
                }
            }
        }
        Ok(())
    }

    /// Builds a forwarder `fn <method>($g0, …) { return this.<field>.<method>($g0, …); }`.
    fn make_forwarder(&mut self, field: Symbol, method: Symbol, arity: usize, pos: &SourcePosition) -> HirId<HirStmt> {
        let field_name = self.hir.text(field).to_string();
        let method_name = self.hir.text(method).to_string();

        let mut params = Vec::with_capacity(arity);
        let mut args = Vec::with_capacity(arity);
        for i in 0..arity {
            let psym = self.hir.intern(&format!("$g{i}"));
            params.push(self.hir.add(HirExpr::Identifier(psym), pos.clone()));
            args.push(self.hir.add(HirExpr::Identifier(psym), pos.clone()));
        }

        let this = self.hir.add(HirExpr::This, pos.clone());
        let field_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(field_name)), pos.clone());
        let field_access = self.hir.add(HirExpr::Index(this, field_lit, true), pos.clone());
        let method_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(method_name)), pos.clone());
        let method_access = self.hir.add(HirExpr::Index(field_access, method_lit, true), pos.clone());
        let call = self.hir.add(HirExpr::Call(method_access, args), pos.clone());
        let ret = self.hir.add(HirStmt::Return(Some(call)), pos.clone());
        let body = self.hir.add(HirExpr::Block(vec![ret]), pos.clone());
        self.hir.add(HirStmt::Fn(HirFnDecl { name: method, params, body }), pos.clone())
    }

    /// At an instantiable type, every `req T`, `req fn`, and `req <member>` of the flattened trait
    /// set (and the type's own) must be satisfied.
    fn check_requirements(&self, decl: &TypeDecl, traits: &[(Symbol, &'a TypeDecl)], gives: &[Symbol], pos: &SourcePosition) -> Result<(), anyhow::Error> {
        let provided: HashSet<Symbol> = traits.iter().map(|(s, _)| *s).chain(gives.iter().copied()).collect();

        // `req T`: the type's own and every flattened trait's required traits must be provided.
        let req_traits = decl.req_traits.iter().map(|t| (*t, None))
            .chain(traits.iter().flat_map(|(ts, td)| td.req_traits.iter().map(move |t| (*t, Some(*ts)))));
        for (rt, by) in req_traits {
            if !provided.contains(&rt) {
                let by = by.map_or(String::new(), |t| format!(" (required by trait '{}')", self.hir.text(t)));
                return Err(anyhow!("Unsatisfied requirement: trait '{}'{by} is not provided by any `with`\n\tat {}",
                    self.hir.text(rt), pos));
            }
        }

        // The composed type's exposed (`inner`/`pub`) methods, keyed by (name, arity), and the set
        // of all exposed member names (fields + methods).
        let mut exposed: HashSet<(Symbol, usize)> = HashSet::new();
        let mut exposed_names: HashSet<Symbol> = HashSet::new();
        for field in &decl.fields {
            if is_exposed(decl, field) { exposed_names.insert(*field); }
        }
        for m in &decl.methods {
            let fd = self.ast_fn(m);
            if is_exposed(decl, &fd.name) { exposed.insert((fd.name, fd.params.len())); exposed_names.insert(fd.name); }
        }
        for (_, type_decl) in traits {
            for field in &type_decl.fields {
                if is_exposed(type_decl, field) { exposed_names.insert(*field); }
            }
            for m in &type_decl.methods {
                let fd = self.ast_fn(m);
                if is_exposed(type_decl, &fd.name) { exposed.insert((fd.name, fd.params.len())); exposed_names.insert(fd.name); }
            }
        }

        // `req fn`: every hole must be filled by an exposed method of matching name and arity.
        let req_fns = decl.req_fns.iter().copied()
            .chain(traits.iter().flat_map(|(_, type_decl)| type_decl.req_fns.iter().copied()));
        for (func_sym, arity) in req_fns {
            if !exposed.contains(&(func_sym, arity)) {
                return Err(anyhow!("Unsatisfied `req fn {}` (arity {arity}): needs an `inner`/`pub` method '{}' taking {arity} argument(s)\n\tat {}",
                    self.hir.text(func_sym), self.hir.text(func_sym), pos));
            }
        }

        // `req <member>`: every member hole must be filled by an exposed field/method of that name.
        let req_members = decl.req_members.iter().copied()
            .chain(traits.iter().flat_map(|(_, type_decl)| type_decl.req_members.iter().copied()));
        for member_sym in req_members {
            if !exposed_names.contains(&member_sym) {
                return Err(anyhow!("Unsatisfied `req {}`: needs an `inner`/`pub` member '{}'\n\tat {}",
                    self.hir.text(member_sym), self.hir.text(member_sym), pos));
            }
        }
        Ok(())
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

    /// Folds one trait's **methods** into `composed`, recording its private-method slots under
    /// `trait_sym`. Exposed methods take their plain name (or a `"<Trait>.<method>"` alias when a
    /// host override in `host_methods` shadows them); private methods take a per-trait slot name so
    /// two traits' same-named privates never collide. Traits are stateless, so there are no fields
    /// to fold.
    fn fold_trait(&mut self, trait_sym: Symbol, type_decl: &TypeDecl, host_methods: &HashSet<Symbol>, composed: &mut Composed) -> Result<(), anyhow::Error> {
        let renames = self.trait_renames(type_decl);
        let mut private_map: HashMap<Symbol, Symbol> = HashMap::new();

        let tname = self.hir.text(trait_sym).to_string();
        for method in &type_decl.methods {
            let name = self.ast_fn(method).name;
            let name_text = self.hir.text(name).to_string();
            let slot = if !is_exposed(type_decl, &name) {
                let renamed = self.hir.intern(&renames[&name_text]);
                private_map.insert(name, renamed);
                renamed
            } else if host_methods.contains(&name) {
                self.hir.intern(&format!("{}.{}", tname, name_text))
            } else {
                if type_decl.pub_members.contains(&name) { composed.pub_members.insert(name); }
                name
            };
            let lowered = self.lower_method_named(method, slot)?;
            composed.methods.push(lowered);
            composed.method_traits.push(Some(trait_sym));
        }

        composed.trait_privates.insert(trait_sym, private_map);
        Ok(())
    }

    /// The flattened `with`-set of a `type`/`trait`, resolved to the live `TypeDecl`s lowering folds.
    /// The `names` pre-pass computed the post-ordered, deduped, cycle-checked set; here it's mapped
    /// from declaration handles to declarations.
    fn flattened_with(&self, type_id: AstId<Stmt>) -> Vec<(Symbol, &'a TypeDecl)> {
        self.names.flattened_with(&type_id).iter().map(|(sym, id)| (*sym, self.ast_type(id))).collect()
    }

    /// The private-method rename map for a trait:
    /// each private method name -> its per-trait form `"<Trait>.<name>"`.
    pub(super) fn trait_renames(&self, td: &TypeDecl) -> HashMap<String, String> {
        let tname = self.hir.text(td.name).to_string();
        let mut map = HashMap::new();
        for method in &td.methods {
            let name = self.ast_fn(method).name;
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
        if self.names.trait_ref(*target).is_none() { return None; }
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
