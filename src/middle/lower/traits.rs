//! Trait composition.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::ast::{AstId, Expr, Literal, ReqFn, ReturnShape, Stmt, Symbol, TraitClause, TypeDecl};
use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::hir::{HirSlotClause, HirExpr, HirFnDecl, HirId, HirLiteral, HirParam, HirReqFn, HirReqParam, HirStmt, HirTypeDecl};

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
    pub(super) fn lower_type(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl, type_pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        // The flattened `with`-set, resolved by the `names` pre-pass.
        let traits = self.flattened_with(type_id);
        self.check_provide_require_exclusive(decl, type_pos)?;
        self.check_provide_once(type_id, decl, type_pos)?;
        // Traits provided by delegation (`field gives Trait`): they satisfy `req T` and `is T`.
        let gives_traits: Vec<Symbol> = self.names.gives_traits(&type_id).iter().map(|(_, t, _)| *t).collect();
        self.check_requirements(decl, &traits, &gives_traits, type_pos)?;
        let host_methods: HashSet<Symbol> = decl.methods.iter().map(|m| self.ast_fn(m).name).collect();
        let exposed_methods = self.check_exposed_collisions(&traits, &host_methods, type_pos)?;

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

        let init = self.lower_factory(type_id, decl, &composed.field_inits, type_pos)?;

        // The `req fn` holes this type must satisfy: its own and those of every `with` trait.
        let mut req_fns: Vec<HirReqFn> = decl.req_fns.iter().map(|rf| self.lower_req_fn(rf, decl.name)).collect();
        for (trait_sym, td) in &traits {
            req_fns.extend(td.req_fns.iter().map(|rf| self.lower_req_fn(rf, *trait_sym)));
        }

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
            nullable_fields: decl.nullable_fields.clone(),
            mut_fields: decl.mut_fields.clone(),
            methods: composed.methods,
            req_fns,
            method_traits: composed.method_traits,
            pub_members: composed.pub_members,
            inner_members: decl.inner_members.clone(),
            trait_privates: composed.trait_privates,
            surface: HashSet::new(), // gating applies to standalone traits, not composed types
            provides,
            gives: self.names.gives_traits(&type_id).iter().map(|(f, t, _)| (*f, *t)).collect(),
        })
    }

    /// Lowers a `trait` declaration into a standalone `HirTypeDecl` for self-containment validation.
    /// The resolver validates the body against the trait's surface independently of any composing type.
    pub(super) fn lower_trait(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl, pos: &SourcePosition) -> Result<HirTypeDecl, anyhow::Error> {
        let surface = self.trait_surface(type_id, decl)?;

        let mut composed = Composed::empty();
        self.fold_trait(decl.name, decl, &HashSet::new(), &mut composed)?;
        let init = self.hir.add(HirStmt::Nop, pos.clone());

        Ok(HirTypeDecl {
            name: decl.name,
            init,
            fields: composed.fields,
            nullable_fields: decl.nullable_fields.clone(),
            mut_fields: decl.mut_fields.clone(),
            methods: composed.methods,
            req_fns: Vec::new(), // satisfaction is checked at composing types, not the trait itself
            method_traits: composed.method_traits,
            pub_members: composed.pub_members,
            inner_members: decl.inner_members.clone(),
            trait_privates: composed.trait_privates,
            surface,
            gives: Vec::new(),
            provides: Vec::new(),
        })
    }

    /// The set of member names a trait's body may reach through `this`.
    fn trait_surface(&mut self, type_id: AstId<Stmt>, decl: &TypeDecl) -> Result<HashSet<Symbol>, anyhow::Error> {
        let mut surface: HashSet<Symbol> = HashSet::new();
        for field in &decl.fields { surface.insert(*field); }
        for method in &decl.methods { surface.insert(self.ast_fn(method).name); }
        for rf in &decl.req_fns { surface.insert(rf.name); }
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
    /// methods grouped by name.
    fn check_exposed_collisions(&self, traits: &[(Symbol, &'a TypeDecl)], host_methods: &HashSet<Symbol>, pos: &SourcePosition) -> Result<HashMap<Symbol, Vec<Symbol>>, anyhow::Error> {
        let mut exposed_methods: HashMap<Symbol, Vec<Symbol>> = HashMap::new();
        for (trait_sym, type_decl) in traits {
            for method in &type_decl.methods {
                let name = self.ast_fn(method).name;
                if is_exposed(type_decl, &name) { exposed_methods.entry(name).or_default().push(*trait_sym); }
            }
        }
        for (name, providers) in &exposed_methods {
            if !host_methods.contains(name) && providers.len() >= 2 {
                return Err(self.error_help_at(format!("Exposed method '{}' clashes between traits {}", self.hir.text(*name), self.trait_list(providers)),
                    pos, format!("declare '{}' in the host type to resolve it", self.hir.text(*name))));
            }
        }
        Ok(exposed_methods)
    }

    /// `req T` and `with T` are mutually exclusive on one composer. You cannot both provide
    /// and depend on the same trait. Applies to types and traits alike.
    pub(super) fn check_provide_require_exclusive(&self, decl: &TypeDecl, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        for trait_sym in &decl.req_traits {
            if decl.with_traits.contains(trait_sym) {
                return Err(self.dup_trait_error(decl, *trait_sym, &[TraitClause::With, TraitClause::Req],
                    format!("Trait '{}' appears in both `with` and `req`", self.hir.text(*trait_sym)), pos));
            }
            if decl.gives.iter().any(|(_, t)| t == trait_sym) {
                return Err(self.dup_trait_error(decl, *trait_sym, &[TraitClause::Req, TraitClause::Gives],
                    format!("Trait '{}' appears in both `req` and `gives`", self.hir.text(*trait_sym)), pos));
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
                return Err(self.dup_trait_error(decl, *trait_sym, &[TraitClause::With, TraitClause::Gives],
                    format!("Trait '{}' appears in both `with` and `gives`", self.hir.text(*trait_sym)), pos));
            }
            if !given.insert(*trait_sym) {
                return Err(self.dup_trait_error(decl, *trait_sym, &[TraitClause::Gives],
                    format!("Trait '{}' appears in `gives` more than once", self.hir.text(*trait_sym)), pos));
            }
        }
        Ok(())
    }

    /// A "trait named twice" error that carets each occurrence in the given clauses. Falls back to
    /// a single span at `fallback` if two textual mentions cannot be found.
    fn dup_trait_error(&self, decl: &TypeDecl, trait_sym: Symbol, clauses: &[TraitClause], msg: String, fallback: &SourcePosition) -> anyhow::Error {
        let mut refs: Vec<&SourcePosition> = decl.trait_refs.iter()
            .filter(|r| r.trait_sym == trait_sym && clauses.contains(&r.clause))
            .map(|r| &r.pos)
            .collect();
        refs.sort_by_key(|p| p.start);
        if refs.len() >= 2 {
            return anyhow!("{}", Diagnostic::new(msg, refs[0].clone())
                .with_label("first appears here")
                .with_span(refs[1].clone(), "and a second time here")
                .with_help("keep only one"));
        }
        anyhow!("{}", Diagnostic::new(msg, fallback.clone()).with_help("keep only one"))
    }

    /// Synthesizes a forwarding method for each exposed method of every `gives` trait's surface.
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
                    let ret = fd.ret;
                    let is_pub = type_decl.pub_members.contains(&name);
                    let forwarder = self.make_forwarder(field, name, arity, ret, pos);
                    composed.methods.push(forwarder);
                    composed.method_traits.push(None);
                    if is_pub { composed.pub_members.insert(name); }
                }
            }
        }
        Ok(())
    }

    /// Builds a forwarder `fn <method>($g0, …) { return this.<field>.<method>($g0, …); }`.
    /// The forwarder carries the delegated method's return shape so it conforms to the trait.
    fn make_forwarder(&mut self, field: Symbol, method: Symbol, arity: usize, ret: ReturnShape, pos: &SourcePosition) -> HirId<HirStmt> {
        let field_name = self.hir.text(field).to_string();
        let method_name = self.hir.text(method).to_string();

        let mut params = Vec::with_capacity(arity);
        let mut args = Vec::with_capacity(arity);
        for i in 0..arity {
            let psym = self.hir.intern(&format!("$g{i}"));
            params.push(HirParam {
                name: self.hir.add(HirExpr::Identifier(psym), pos.clone()),
                pos: pos.clone(),
                nullable: false,
                mutable: false,
                clause: HirSlotClause::default(),
            });
            args.push(self.hir.add(HirExpr::Identifier(psym), pos.clone()));
        }

        let this = self.hir.add(HirExpr::This, pos.clone());
        let field_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(field_name)), pos.clone());
        let field_access = self.hir.add(HirExpr::Index(this, field_lit, true), pos.clone());
        let method_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(method_name)), pos.clone());
        let method_access = self.hir.add(HirExpr::Index(field_access, method_lit, true), pos.clone());
        let call = self.hir.add(HirExpr::Call(method_access, args), pos.clone());
        let ret_stmt = self.hir.add(HirStmt::Return(Some(call)), pos.clone());
        let body = self.hir.add(HirExpr::Block(vec![ret_stmt]), pos.clone());
        self.hir.add(HirStmt::Fn(HirFnDecl { name: method, sig_pos: pos.clone(), params, body, ret, clause: HirSlotClause::default() }), pos.clone())
    }

    /// Lowers a `req fn` hole to its per-slot clauses, folding each `?` marker into the clause the
    /// same way a declared parameter or return does. The `[obl]` container flag rides along so the
    /// variance check can keep container and bare shapes distinct.
    fn lower_req_fn(&self, rf: &ReqFn, trait_name: Symbol) -> HirReqFn {
        let params = rf.params.iter()
            .map(|p| HirReqParam { pos: p.pos.clone(), clause: self.slot_clause(p.nullable, &p.clause) })
            .collect();
        let ret = self.slot_clause(rf.ret == ReturnShape::Nullable, &rf.clause);
        HirReqFn { name: rf.name, trait_name, pos: rf.pos.clone(), params, ret }
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
                return Err(self.error_at(format!("Unsatisfied requirement: trait '{}'{by} is not provided by any `with`",
                    self.hir.text(rt)), pos));
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
        let req_fns = decl.req_fns.iter()
            .chain(traits.iter().flat_map(|(_, type_decl)| type_decl.req_fns.iter()));
        for rf in req_fns {
            let arity = rf.params.len();
            if !exposed.contains(&(rf.name, arity)) {
                return Err(self.error_at(format!("Unsatisfied `req fn {}` (arity {arity}): needs an `inner`/`pub` method '{}' taking {arity} argument(s)",
                    self.hir.text(rf.name), self.hir.text(rf.name)), pos));
            }
        }

        // `req <member>`: every member hole must be filled by an exposed field/method of that name.
        let req_members = decl.req_members.iter().copied()
            .chain(traits.iter().flat_map(|(_, type_decl)| type_decl.req_members.iter().copied()));
        for member_sym in req_members {
            if !exposed_names.contains(&member_sym) {
                return Err(self.error_at(format!("Unsatisfied `req {}`: needs an `inner`/`pub` member '{}'",
                    self.hir.text(member_sym), self.hir.text(member_sym)), pos));
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

    /// Folds one trait's methods into `composed`.
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
        let sig_pos = decl.sig_pos.clone();
        let params = self.params(&decl.params)?;
        let (ret, clause) = self.return_clause(decl);
        let body = self.expr(&decl.body)?;
        Ok(self.hir.add(HirStmt::Fn(HirFnDecl { name, sig_pos, params, body, ret, clause }), pos))
    }

    pub(super) fn as_qualified_method_call(&self, callee: &AstId<Expr>) -> Option<(Symbol, String)> {
        let Expr::Index(target, member, true) = self.ast.get(callee) else { return None };
        let Expr::Identifier(t) = self.ast.get(target) else { return None };
        if self.names.trait_ref(*target).is_none() { return None; }
        let Expr::Literal(Literal::String(m)) = self.ast.get(member) else { return None };
        if m == "init" { return None; } // init orchestration has its own path
        Some((*t, m.clone()))
    }

    /// Builds the HIR for `Trait.method(args)`.
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
}
