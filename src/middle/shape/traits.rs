//! Trait-contract shape: override return conformance and `req fn` variance.

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{HirId, HirLiteral, HirMatchElem, HirMatchField, HirMatcher, HirParam, HirReqMember, HirStmt, HirTypeDecl, Symbol, SYNTHETIC_PARAM};
use crate::middle::signatures::RetSig;
use crate::middle::obligations::Obligations;

use super::Shape;

/// What a parameter pattern lets through, as far as this pass can name it.
impl<'a> Shape<'a> {
    /// A member overriding a trait method may return non-null where the trait method is nullable,
    /// but not the reverse.
    pub(super) fn check_method_overrides(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for method in &decl.methods {
            let HirStmt::Fn(folded) = self.hir.get(method) else { continue };
            // A trait method the host overrides is folded under a `"Trait.method"` alias.
            let Some((trait_name, base)) = split_trait_alias(self.hir.text(folded.name)) else { continue };
            let Some(base_sym) = self.hir.symbol_of(base) else { continue };
            // A renamed private slot is also dotted. Only an exposed override is a contract.
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
        }
        Ok(())
    }

    /// Checks obligation variance where a `req fn` hole is satisfied.
    pub(super) fn check_req_conformance(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for req in &decl.req_fns {
            let Some(method) = self.satisfying_method(decl, req.name) else { continue };
            let (Some(sig), HirStmt::Fn(sat)) = (self.sigs.fn_sig_of(&method), self.hir.get(&method)) else { continue };
            let name = self.hir.text(req.name);
            let type_name = self.hir.text(decl.name);
            let trait_name = self.hir.text(req.trait_name);

            // The `[obl]` container shape is invariant: a bare value and a container are not
            // interchangeable, since the trait body reads one by index and the other directly.
            if req.ret.container != sat.clause.container {
                let (mine, theirs) = shape_words(req.ret.container);
                return Err(self.error_ctx("return shape does not match the trait",
                    self.hir.pos(&method), format!("`{type_name}.{name}` returns {mine}"),
                    &req.pos, format!("`{trait_name}.{name}` declares {theirs} return")));
            }


            // A satisfier may not owe a return obligation the requirement does not permit.
            let diff = self.sorted_difference(&sig.ret.obligations, &req.ret.owed());
            if !diff.is_empty() {
                return Err(self.error_ctx("return owes an obligation the trait forbids",
                    self.hir.pos(&method), format!("`{type_name}.{name}` returns a value owing {}", quote_list(&diff)),
                    &req.pos, format!("`{trait_name}.{name}` forbids {}", quote_list(&diff))));
            }

            // A satisfier's parameter must accept at least what the hole passes it.
            for (i, hole) in req.params.iter().enumerate() {
                let Some(sat_param) = sat.params.get(i) else { continue };
                let param = self.param_subject(sat_param, i);
                if hole.clause.container != sat_param.clause.container {
                    let (mine, theirs) = shape_words(hole.clause.container);
                    return Err(self.error_ctx("parameter shape does not match the trait",
                        self.hir.pos(&sat_param.name), format!("`{type_name}.{name}` takes {mine}"),
                        &req.pos, format!("`{trait_name}.{name}` declares {theirs} parameter")));
                }


                // A satisfier may widen a pattern but not narrow it.
                if !self.accepts_at_least(hole.pattern.as_ref(), sat_param.pattern.as_ref()) {
                    return Err(self.error_ctx_help("parameter accepts less than the trait declares",
                        &sat_param.pos, format!("`{type_name}.{name}` accepts {} for {param}", self.describe_pattern(sat_param.pattern.as_ref())),
                        &hole.pos, format!("`{trait_name}.{name}` passes {}", self.describe_pattern(hole.pattern.as_ref())),
                        format!("accept at least what the trait passes, or drop the pattern on {param}")));
                }

                let Some(accepted) = sig.param_clauses.get(i) else { continue };
                let missing = self.sorted_difference(&hole.clause.owed(), accepted);
                if !missing.is_empty() {
                    return Err(self.error_ctx("parameter rejects an obligation the trait passes",
                        self.hir.pos(&sat_param.name), format!("`{type_name}.{name}` does not accept {} for {param}", quote_list(&missing)),
                        &req.pos, format!("`{trait_name}.{name}` passes {}", quote_list(&missing))));
                }
            }
        }
        Ok(())
    }

    /// Checks each required member against the member filling it.
    pub(super) fn check_req_members(&self, node: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
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

            let owed = layout.owed(req.name, self.sigs.opt);
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
            (HirMatcher::Shape(hole), HirMatcher::Shape(sat)) => self.fields_accept_at_least(hole, sat),
            (HirMatcher::Array(hole), HirMatcher::Array(sat)) => self.elements_accept_at_least(hole, sat),
            (HirMatcher::Type { nominal: true, .. }, HirMatcher::Literal(_) | HirMatcher::Array(_)) => false,
            (HirMatcher::Literal(_), _) => false,
            (HirMatcher::Shape(_), HirMatcher::Literal(_) | HirMatcher::Array(_)) => false,
            (HirMatcher::Shape(_), HirMatcher::Type { nominal: true, .. }) => false,
            (HirMatcher::Array(_), HirMatcher::Literal(_)) => false,
            (HirMatcher::Array(_), HirMatcher::Type { nominal: true, .. }) => false,
            (HirMatcher::Array(_), HirMatcher::Shape(_)) => true,
            (HirMatcher::Type { nominal: true, .. }, HirMatcher::Shape(_)) => true,
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
            HirMatcher::Shape(fields) => fields.iter().all(|f| self.hir.get(&f.value).is_irrefutable(self.hir)),
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

fn same_scalar(a: &HirLiteral, b: &HirLiteral) -> bool {
    match (a, b) {
        (HirLiteral::Null, HirLiteral::Null) => true,
        (HirLiteral::Boolean(a), HirLiteral::Boolean(b)) => a == b,
        (HirLiteral::Number(a), HirLiteral::Number(b)) => a == b,
        (HirLiteral::String(a), HirLiteral::String(b)) => a == b,
        _ => false,
    }
}

/// Splits a folded `"Trait.method"` alias into its trait and base method names.
fn split_trait_alias(name: &str) -> Option<(&str, &str)> {
    name.split_once('.')
}

fn shape_words(container: bool) -> (&'static str, &'static str) {
    if container { ("a bare value", "a container") } else { ("a container", "a bare") }
}

/// Quotes each name and joins them, like `'fails', 'opt'`.
fn quote_list(names: &[String]) -> String {
    names.iter().map(|n| format!("'{n}'")).collect::<Vec<_>>().join(", ")
}
