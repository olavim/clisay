//! Trait-contract shape: what a type or trait owes the traits it mixes.

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{same_scalar, HirId, HirMatchElem, HirMatchField, HirMatcher, HirParam, HirReqFn, HirReqMember, HirReqParam, HirStmt, HirTypeDecl, Symbol, SYNTHETIC_PARAM};
use crate::middle::signatures::RetSig;
use crate::middle::obligations::{quoted_obligation_list, Obligations};

use super::Shape;

#[derive(Clone, Copy)]
enum SatisfierKind { Member, Requirement }

impl SatisfierKind {
    fn returns_owing(self) -> &'static str {
        match self {
            SatisfierKind::Member => "returns a value owing",
            SatisfierKind::Requirement => "requires a return owing",
        }
    }
}

/// A method or `req fn` judged against a `req fn`.
struct SatisfyingFn<'p> {
    kind: SatisfierKind,
    pos: &'p SourcePosition,
    anchor_receiver_pos: Option<&'p SourcePosition>,
    ret_owed: Obligations,
    /// `Owner.member`.
    owner: String,
    params: Vec<SatisfyingParam<'p>>,
}

/// A parameter judged against a `req fn` parameter.
struct SatisfyingParam<'p> {
    pos: &'p SourcePosition,
    name_pos: &'p SourcePosition,
    anchor: bool,
    pattern: Option<&'p HirId<HirMatcher>>,
    /// Absent where no clause was collected for the parameter, which asks nothing of it.
    owed: Option<Obligations>,
    /// `Owner.member`.
    owner: String,
    /// How a label names the parameter itself.
    subject: String,
}

impl<'a> Shape<'a> {
    pub(super) fn check_type_override_returns(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for method in &decl.methods {
            let HirStmt::Fn(folded) = self.hir.get(method) else { continue };
            let Some((trait_name, base)) = split_trait_alias(self.hir.text(folded.name)) else { continue };
            let Some(base_sym) = self.hir.symbol_of(base) else { continue };

            if !decl.pub_members.contains(&base_sym) {
                continue;
            }

            let host = decl.methods.iter().copied().find(|s| matches!(self.hir.get(s), HirStmt::Fn(h) if h.name == base_sym));
            let (Some(host), Some(trait_ret)) = (host, self.sigs.fn_sig_of(method).map(|f| &f.ret)) else { continue };
            let Some(host_ret) = self.sigs.fn_sig_of(&host).map(|f| &f.ret) else { continue };

            if !self.ret_conforms(host_ret, trait_ret) {
                return Err(self.error_labeled("override is more nullable than the trait allows".to_string(),
                    &host, format!("`{base}` may return null where trait '{trait_name}' declares non-null")));
            }

            let extra: Obligations = host_ret.obligations.iter().copied()
                .filter(|o| *o != self.sigs.opt && !trait_ret.obligations.contains(o))
                .collect();
            if !extra.is_empty() {
                let owed = quoted_obligation_list(self.hir, &extra);
                return Err(self.error_labeled(format!("override owes {owed} where the trait does not"),
                    &host, format!("a caller holding trait '{trait_name}' expects `{base}` not to owe {owed}")));
            }
        }
        Ok(())
    }

    pub(super) fn check_type_satisfies_req_fns(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for req in &decl.req_fns {
            if let Some(sat @ SatisfyingFn { kind: SatisfierKind::Member, .. }) = self.satisfying_fn_of(decl, req) {
                self.check_req_fn_met(req, &sat)?;
            }
        }
        Ok(())
    }

    fn satisfying_fn_of(&self, decl: &'a HirTypeDecl, hole: &HirReqFn) -> Option<SatisfyingFn<'a>> {
        if let Some(method) = self.satisfying_method(decl, hole.name) {
            let (Some(sig), HirStmt::Fn(sat)) = (self.sigs.fn_sig_of(&method), self.hir.get(&method)) else { return None };
            let owner = format!("{}.{}", self.hir.text(decl.name), self.hir.text(hole.name));
            return Some(SatisfyingFn {
                kind: SatisfierKind::Member,
                pos: self.hir.pos(&method),
                ret_owed: sig.ret.obligations.clone(),
                owner: owner.clone(),
                anchor_receiver_pos: sat.receiver.as_ref().filter(|r| r.anchor).map(|r| &r.pos),
                params: sat.params.iter().enumerate().map(|(i, param)| SatisfyingParam {
                    pos: &param.pos,
                    name_pos: self.hir.pos(&param.name),
                    anchor: param.anchor,
                    pattern: param.pattern.as_ref(),
                    owed: sig.param_clauses.get(i).cloned(),
                    owner: owner.clone(),
                    subject: self.param_subject(param, i),
                }).collect(),
            });
        }

        let own = decl.req_fns.iter().find(|own| own.name == hole.name)?;
        let owner = format!("{}.{}", self.hir.text(own.trait_name), self.hir.text(own.name));
        Some(SatisfyingFn {
            kind: SatisfierKind::Requirement,
            pos: &own.pos,
            ret_owed: own.ret.owed(),
            owner: owner.clone(),
            anchor_receiver_pos: None,
            params: own.params.iter().enumerate().map(|(i, param)| SatisfyingParam {
                pos: &param.pos,
                name_pos: &param.pos,
                anchor: false,
                pattern: param.pattern.as_ref(),
                owed: Some(param.clause.owed()),
                owner: owner.clone(),
                subject: format!("argument {}", i + 1),
            }).collect(),
        })
    }

    fn check_req_fn_met(&self, hole: &HirReqFn, sat: &SatisfyingFn) -> Result<(), anyhow::Error> {
        let name = self.hir.text(hole.name);
        let trait_name = self.hir.text(hole.trait_name);
        let (sat_owner, hole_owner) = (&sat.owner, format!("{trait_name}.{name}"));

        // A satisfier may not owe a return obligation the requirement does not permit.
        let forbidden = self.sorted_difference(&sat.ret_owed, &hole.ret.owed());
        if !forbidden.is_empty() {
            return Err(self.error_ctx("return owes an obligation the trait forbids",
                sat.pos, format!("`{sat_owner}` {} {}", sat.kind.returns_owing(), quote_list(&forbidden)),
                &hole.pos, format!("`{hole_owner}` forbids {}", quote_list(&forbidden))));
        }

        if let Some(receiver) = sat.anchor_receiver_pos {
            return Err(self.error_ctx_help("method wants an anchor receiver where the trait passes a value receiver",
                receiver, format!("`{sat_owner}` wants an anchor receiver"),
                &hole.pos, format!("`{hole_owner}` passes a value receiver"),
                "a call through the trait passes a value receiver, so drop the `&`"));
        }

        for (hole_param, sat_param) in hole.params.iter().zip(&sat.params) {
            self.check_req_fn_param_met(hole_param, &hole_owner, &hole.pos, sat_param)?;
        }
        Ok(())
    }

    fn mixed_in_traits<'d>(&'d self, decl: &'d HirTypeDecl) -> impl Iterator<Item = &'a HirTypeDecl> + 'd {
        decl.provides.iter().filter(|(_, id)| *id != decl.id).filter_map(|(_, id)| {
            match self.hir.get(&self.sigs.type_decl_of_id(*id)?) {
                HirStmt::Trait(mixed) => Some(&**mixed),
                _ => None,
            }
        })
    }

    pub(super) fn check_mixed_in_req_fns(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for mixed in self.mixed_in_traits(decl) {
            for hole in &mixed.req_fns {
                if let Some(sat) = self.satisfying_fn_of(decl, hole) {
                    self.check_req_fn_met(hole, &sat)?;
                }
            }
        }
        Ok(())
    }

    fn check_req_fn_param_met(&self, hole: &HirReqParam, hole_owner: &str, hole_fn_pos: &SourcePosition, sat: &SatisfyingParam) -> Result<(), anyhow::Error> {
        let (sat_owner, subject) = (&sat.owner, &sat.subject);

        if sat.anchor {
            return Err(self.error_ctx_help("parameter takes an anchor where the trait declares a value",
                sat.name_pos, format!("`{sat_owner}` takes an anchor for {subject}"),
                hole_fn_pos, format!("`{hole_owner}` passes a value"),
                "a caller reaching this through the trait passes a value, so drop the `&`"));
        }

        if !self.accepts_at_least(hole.pattern.as_ref(), sat.pattern) {
            return Err(self.error_ctx_help("parameter accepts less than the trait declares",
                sat.pos, format!("`{sat_owner}` accepts {} for {subject}", self.describe_pattern(sat.pattern)),
                &hole.pos, format!("`{hole_owner}` passes {}", self.describe_pattern(hole.pattern.as_ref())),
                format!("accept at least what the trait passes, or drop the pattern on {subject}")));
        }

        let Some(accepted) = &sat.owed else { return Ok(()) };
        let unaccepted = self.sorted_difference(&hole.clause.owed(), accepted);
        if !unaccepted.is_empty() {
            return Err(self.error_ctx("parameter rejects an obligation the trait passes",
                sat.name_pos, format!("`{sat_owner}` does not accept {} for {subject}", quote_list(&unaccepted)),
                hole_fn_pos, format!("`{hole_owner}` passes {}", quote_list(&unaccepted))));
        }
        Ok(())
    }

    pub(super) fn check_mixed_req_var(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for mixed in self.mixed_in_traits(decl) {
            for inherited in mixed.req_members.iter().filter(|r| r.reassignable) {
                let Some(own) = decl.req_members.iter().find(|o| o.name == inherited.name && !o.reassignable) else { continue };
                let (name, owner) = (self.hir.text(own.name), self.hir.text(mixed.name));
                return Err(self.error_ctx_help(format!("`req {name}` loosens the `req var {name}` required by trait '{owner}'"),
                    &own.pos, format!("`{name}` is required here without `var`"),
                    &inherited.pos, format!("`{owner}` requires a reassignable `{name}`"),
                    format!("declare it as `req var {name};`, or drop it and inherit the requirement")));
            }
        }
        Ok(())
    }

    /// Checks each required member against the member filling it.
    pub(super) fn check_type_satisfies_req_members(&self, node: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let Some(layout) = self.layout_of(node) else { return Ok(()) };
        let type_name = self.hir.text(decl.name);
        for req in &decl.req_members {
            let name = self.hir.text(req.name);
            let trait_name = self.hir.text(req.trait_name);
            let required = req.clause.owed();
            let error_header = format!("member `{name}` does not satisfy `{trait_name}.{name}`");

            // The member's own declaration is what has to change, so that is where the caret goes.
            // A missing member is the composer's error, and lowering reports it at the type.
            let at = decl.field_positions.get(&req.name).unwrap_or(self.hir.pos(node));
            if !layout.is_field(req.name) {
                if !req.reassignable && required.is_empty() {
                    continue;
                }
                let method = self.satisfying_method(decl, req.name).map(|m| self.hir.pos(&m).clone());
                return Err(anyhow!("{}", self.member_error_frame(error_header, method.as_ref().unwrap_or(at),
                    format!("`{type_name}.{name}` is a method"), node, req,
                    format!("`{trait_name}` requires state from `{name}`"))
                    .with_help(format!("fill `{name}` with a field, or drop what the requirement asks of it"))));
            }

            if req.reassignable && !layout.is_reassignable(req.name) {
                return Err(anyhow!("{}", self.member_error_frame(error_header, at,
                    format!("`{type_name}.{name}` is not reassignable"), node, req,
                    format!("`{trait_name}` requires a reassignable `{name}`"))
                    .with_help(format!("declare it `pub var {name}` on `{type_name}`"))));
            }

            let owed = layout.owed(req.name);
            let extra = self.sorted_difference(&owed, &required);
            if !extra.is_empty() {
                return Err(anyhow!("{}", self.member_error_frame(error_header, at,
                    format!("`{type_name}.{name}` owes {}", quote_list(&extra)), node, req,
                    format!("`{trait_name}.{name}` does not declare {}", quote_list(&extra)))));
            }

            let missing = self.sorted_difference(&required, &owed);
            if req.reassignable && !missing.is_empty() {
                return Err(anyhow!("{}", self.member_error_frame(error_header, at,
                    format!("`{type_name}.{name}` does not owe {}", quote_list(&missing)), node, req,
                    format!("`{trait_name}` writes {} into `{name}`", quote_list(&missing)))
                    .with_help(format!("declare `{name}: {}` on `{type_name}`", missing.join(" ")))));
            }
        }
        Ok(())
    }

    fn member_error_frame(&self, header: String, at: &SourcePosition, label: String, node: &HirId<HirStmt>,
        req: &HirReqMember, site_label: String) -> Diagnostic {
        let mut frame = Diagnostic::new(header, at.clone())
            .with_label(label)
            .with_context_span(req.pos.clone(), site_label);
        frame = self.enclose_error_site(frame, self.hir.pos(node), at);
        match self.sigs.trait_decl(req.trait_name) {
            Some(declaring) => self.enclose_error_site(frame, self.hir.pos(&declaring), &req.pos),
            None => frame,
        }
    }

    /// Shows the declaration an error site sits inside.
    fn enclose_error_site(&self, frame: Diagnostic, declaration: &SourcePosition, site: &SourcePosition) -> Diagnostic {
        match declaration.line == site.line {
            true => frame,
            false => frame.with_enclosing(declaration.clone()),
        }
    }

    fn param_subject(&self, param: &HirParam, position: usize) -> String {
        let name = self.hir.text(self.hir.ident_sym(&param.name));
        match name.starts_with(SYNTHETIC_PARAM) {
            true => format!("argument {}", position + 1),
            false => format!("`{name}`"),
        }
    }

    /// How to name a pattern in a variance diagnostic.
    fn describe_pattern(&self, pattern: Option<&HirId<HirMatcher>>) -> String {
        match pattern {
            None => "any value".to_string(),
            Some(pattern) => format!("`{}`", self.hir.pos(pattern).snippet()),
        }
    }

    /// Whether the satisfier accepts at least every value the hole does.
    fn accepts_at_least(&self, hole: Option<&HirId<HirMatcher>>, sat: Option<&HirId<HirMatcher>>) -> bool {
        match (hole, sat) {
            (_, None) => true,
            (None, Some(sat)) => self.hir.get(sat).is_irrefutable(self.hir),
            (Some(hole), Some(sat)) => self.matcher_accepts_at_least(hole, sat),
        }
    }

    fn matcher_accepts_at_least(&self, hole: &HirId<HirMatcher>, sat: &HirId<HirMatcher>) -> bool {
        let (hole, sat) = (&self.test_of(hole), &self.test_of(sat));
        let (h, s) = (self.hir.get(hole), self.hir.get(sat));
        if s.is_irrefutable(self.hir) {
            return true;
        }
        if h.is_irrefutable(self.hir) {
            return false;
        }
        match (h, s) {
            (HirMatcher::Or(alternatives), _) => alternatives.iter().all(|a| self.matcher_accepts_at_least(a, sat)),
            (_, HirMatcher::Or(alternatives)) => alternatives.iter().any(|a| self.matcher_accepts_at_least(hole, a)),
            (_, HirMatcher::And(parts)) => parts.iter().all(|p| self.matcher_accepts_at_least(hole, p)),
            (HirMatcher::Literal(hole), HirMatcher::Literal(sat)) => same_scalar(hole, sat),
            (HirMatcher::Type { nominal: true, name: hole_name, shape: hole_shape },
             HirMatcher::Type { nominal: true, name: sat_name, shape: sat_shape }) =>
                hole_name == sat_name && self.type_shape_accepts_at_least(hole_shape.as_ref(), sat_shape.as_ref()),
            (HirMatcher::Shape { fields: hole, .. }, HirMatcher::Shape { fields: sat, .. }) => self.fields_accept_at_least(hole, sat),
            (HirMatcher::Dict(hole), HirMatcher::Dict(sat)) => self.matcher_accepts_at_least(hole, sat),
            (HirMatcher::Dict(_), _) => false,
            (HirMatcher::Shape { .. }, HirMatcher::Dict(_)) => true,
            (HirMatcher::Literal(_) | HirMatcher::Array(_) | HirMatcher::Type { nominal: true, .. }, HirMatcher::Dict(_)) => false,
            (HirMatcher::Array(hole), HirMatcher::Array(sat)) => self.elements_accept_at_least(hole, sat),
            (HirMatcher::Type { nominal: true, .. }, HirMatcher::Literal(_) | HirMatcher::Array(_)) => false,
            (HirMatcher::Literal(_), _) => false,
            (HirMatcher::Shape { .. }, HirMatcher::Literal(_) | HirMatcher::Array(_)) => false,
            (HirMatcher::Shape { .. }, HirMatcher::Type { nominal: true, .. }) => false,
            (HirMatcher::Array(_), HirMatcher::Literal(_)) => false,
            (HirMatcher::Array(_), HirMatcher::Type { nominal: true, .. }) => false,
            (HirMatcher::Array(_), HirMatcher::Shape { .. }) => true,
            (HirMatcher::Type { nominal: true, .. }, HirMatcher::Shape { .. }) => true,
            (HirMatcher::Type { nominal: false, .. }, _) => true,
            (_, HirMatcher::Type { nominal: false, .. }) => true,
            (HirMatcher::And(_), _) => true,
            (HirMatcher::Wildcard | HirMatcher::Binder(_) | HirMatcher::As(..), _) => true,
            (_, HirMatcher::Wildcard | HirMatcher::Binder(_) | HirMatcher::As(..)) => true,
        }
    }

    /// The test a matcher makes, with any `name @` wrapper taken off.
    fn test_of(&self, pattern: &HirId<HirMatcher>) -> HirId<HirMatcher> {
        match self.hir.get(pattern) {
            HirMatcher::As(_, inner) => self.test_of(inner),
            _ => *pattern,
        }
    }

    fn type_shape_accepts_at_least(&self, hole: Option<&HirId<HirMatcher>>, sat: Option<&HirId<HirMatcher>>) -> bool {
        match (hole, sat) {
            (_, None) => true,
            (None, Some(sat)) => self.shape_only_binds(sat),
            (Some(hole), Some(sat)) => self.matcher_accepts_at_least(hole, sat),
        }
    }

    /// Whether a shape publishes names without testing anything.
    fn shape_only_binds(&self, shape: &HirId<HirMatcher>) -> bool {
        match self.hir.get(shape) {
            HirMatcher::Shape { fields, .. } => fields.iter().all(|f| self.hir.get(&f.value).is_irrefutable(self.hir)),
            _ => false,
        }
    }

    fn fields_accept_at_least(&self, hole: &[HirMatchField], sat: &[HirMatchField]) -> bool {
        sat.iter().all(|sat| match hole.iter().find(|hole| same_scalar(&hole.key, &sat.key)) {
            Some(hole) => self.matcher_accepts_at_least(&hole.value, &sat.value),
            None => false,
        })
    }

    fn elements_accept_at_least(&self, hole: &[HirMatchElem], sat: &[HirMatchElem]) -> bool {
        let fixed = |elements: &[HirMatchElem]| elements.iter().all(|e| matches!(e, HirMatchElem::Elem(_)));
        if !fixed(hole) || !fixed(sat) {
            return true;
        }
        if hole.len() != sat.len() {
            return false;
        }
        hole.iter().zip(sat).all(|(hole, sat)| match (hole, sat) {
            (HirMatchElem::Elem(hole), HirMatchElem::Elem(sat)) => self.matcher_accepts_at_least(hole, sat),
            _ => true,
        })
    }

    /// The exposed method that fills a `req fn` hole, matched by its plain name.
    fn satisfying_method(&self, decl: &HirTypeDecl, name: Symbol) -> Option<HirId<HirStmt>> {
        decl.methods.iter().copied().find(|m| matches!(self.hir.get(m), HirStmt::Fn(h) if h.name == name))
    }

    fn sorted_difference(&self, set: &Obligations, other: &Obligations) -> Vec<String> {
        let mut names: Vec<String> = set.difference(other).map(|o| self.hir.text(*o).to_string()).collect();
        names.sort();
        names
    }

    /// Whether a member's return conforms to a trait method's.
    fn ret_conforms(&self, host: &RetSig, trait_ret: &RetSig) -> bool {
        if trait_ret.void {
            return true;
        }
        if trait_ret.obligations.contains(&self.sigs.opt) {
            // The trait is nullable: the host may be non-null or nullable, but must return a value.
            return !host.void;
        }
        // The trait is non-null: the host must return a non-null value.
        !host.void && !host.obligations.contains(&self.sigs.opt)
    }
}

/// Splits a folded `"Trait.method"` alias into its trait and base method names.
fn split_trait_alias(name: &str) -> Option<(&str, &str)> {
    name.split_once('.')
}

/// Quotes each name and joins them, like `'fails', 'opt'`.
fn quote_list(names: &[String]) -> String {
    names.iter().map(|n| format!("'{n}'")).collect::<Vec<_>>().join(", ")
}
