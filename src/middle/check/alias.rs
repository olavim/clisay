//! Aliasing: which value a name denotes, who may write it, and when that is given back.
//!
//! The family that answers one-writer questions, wherever they are asked: at a binding, a store,
//! a capture, or an argument.

use crate::middle::diagnose::Diagnose;
use std::collections::HashSet;

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::hir::{Capability, HirExpr, HirId, HirLiteral, HirStmt, Symbol};

use super::{BinderSource, Checker, Ctx, Debt, Guard, Mutability, Site, ValueState};

/// Whether a binding's write-ownership is known to have gone, or only might have.
#[derive(Clone, Copy, PartialEq)]
pub enum WriteOwnershipTransfer {
    /// Write-ownership was transferred. The value was bound, stored, returned, or passed
    /// to a `*mut` parameter, so the binding no longer has write-ownership over it.
    Transferred,
    /// Write-ownership may have been transferred. The value went to a callee this pass cannot
    /// resolve, so only the callee that actually arrives settles it. Using the binding again
    /// demands that proof at runtime. Carries the callee node and argument position the proof needs.
    Unknown(HirId<HirExpr>, u8),
}

/// Where a binding's write-ownership went, or may have gone.
#[derive(Clone, Copy, PartialEq)]
pub struct TransferSite {
    pub node: HirId<HirExpr>,
    pub transfer: WriteOwnershipTransfer,
}

/// Which element within an aggregate a name holds. A dict keys on any value, so a key is any
/// literal, not just an index.
#[derive(Clone, Copy, PartialEq)]
pub enum ElementKey {
    Number(f64),
    Bool(bool),
    Null,
    /// A string key, held as its member node. A dict key is not interned, so the text is what two
    /// reads have in common rather than a symbol.
    Name(HirId<HirExpr>),
}


/// Everything the one-writer rule tracks about one binding. It sits behind a single field on
/// `Local`, so the rest of the check pass carries it without reaching into it.
#[derive(Clone, Default)]
pub(super) struct AliasLocal {
    pub(super) mutability: Mutability,
    pub(super) borrowed: bool,
    /// Where the binding gave its write-ownership away, or `None` while it still writes.
    pub(super) transfer_site: Option<TransferSite>,
    /// The prior slots this binding took its mutable value from.
    pub(super) mutable_provenance: Vec<usize>,
    /// The closure that took over writing this binding's value, and where it writes it.
    pub(super) writing_captor: Option<(Symbol, HirId<HirExpr>)>,
    /// Every aggregate slot and key this binding may have read its value out of.
    pub(super) extracted_from: Vec<(usize, Option<ElementKey>)>,
    pub(super) holds_write_ownership: bool,
    /// Whether the value may already be named somewhere this pass cannot see.
    pub(super) shared_origin: bool,
    pub(super) first_written_at: Option<HirId<HirExpr>>,
    pub(super) borrowed_maybe_mutable: bool,
    /// Whether the value cannot leave the call, so anything it's stored into can't leave either.
    pub(super) confined: bool,
    /// Whether anything besides this name may reach the value.
    pub(super) may_be_shared: bool,
    pub(super) unshared_stores: Vec<HirId<HirExpr>>,
}

impl AliasLocal {
    pub(super) fn reached_from_elsewhere(&self) -> bool {
        self.shared_origin || !self.extracted_from.is_empty()
    }
}

/// Where a value lives: the thing a path starts from, and each element read out of it.
#[derive(Clone)]
pub(super) struct Place {
    pub(super) base: Base,
    /// One element read per entry, outermost first.
    pub(super) steps: Vec<Option<ElementKey>>,
}


/// What a place is rooted in.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum Base {
    /// A binding in the current frame.
    Local(usize),
    /// A value made right here, which nothing else can already name.
    Fresh,
    Unknown,
}


impl<'a> Ctx<'a> {
    pub(super) fn arg_capability_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>, want: &str, got: &str) -> anyhow::Error {
        let (a, c) = (self.arg_display_name(arg), self.callee_display_name(callee));
        self.error_ctx(format!("expected {want} argument"), self.hir.pos(arg), format!("{a} is {got}"),
            self.hir.pos(callee), format!("{c} expects {a} to be {want}"))
    }

    fn is_this(&self, target: &HirId<HirExpr>) -> bool {
        match self.hir.get(target) {
            HirExpr::This => true,
            HirExpr::Index(inner, _, _) | HirExpr::SafeAccess(inner, _, _)
            | HirExpr::Assert(inner) | HirExpr::Propagate(inner) | HirExpr::Mut(inner) => self.is_this(inner),
            _ => false,
        }
    }

    pub(super) fn element_key(&self, member: &HirId<HirExpr>) -> Option<ElementKey> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::Number(n)) => Some(ElementKey::Number(*n)),
            HirExpr::Literal(HirLiteral::Boolean(b)) => Some(ElementKey::Bool(*b)),
            HirExpr::Literal(HirLiteral::Null) => Some(ElementKey::Null),
            HirExpr::Literal(HirLiteral::String(_)) => Some(ElementKey::Name(*member)),
            _ => None,
        }
    }

    pub(super) fn non_var_field_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field_display_name(decl, field);
        self.error_help(format!("Cannot reassign field `{name}`"), lhs,
            format!("you can make `{name}` reassignable by declaring it as `{};`", self.var_decl_error_hint(decl, field)))
    }

    pub(super) fn keeps_receiver_error(&self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>) -> anyhow::Error {
        let subject = self.quoted_subject(receiver);
        let c = self.callee_display_name(callee);
        self.error_ctx(
            format!("{subject} is mutable and this method stores its receiver; freeze or copy it, or take the receiver by '*mut'"),
            self.hir.pos(receiver), format!("{subject} is mutable"),
            self.hir.pos(callee), format!("{c} stores its receiver"))
    }

    pub(super) fn method_assign_error(&self, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        self.error(format!("Cannot assign to method '{}'", self.hir.text(field)), lhs)
    }

    pub(super) fn mutable_in_immutable_error(&self, node: &HirId<HirExpr>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(
            "cannot store a mutable value in an immutable container".to_string(),
            self.hir.pos(node).clone())
            .with_label("this value is mutable")
            .with_help("freeze the value, or mark the container `mut`"))
    }

    pub(super) fn param_needs_mut_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        self.arg_capability_error(callee, arg, "mutable", "immutable")
    }

    pub(super) fn is_same_key(&self, a: &ElementKey, b: &ElementKey) -> bool {
        match (a, b) {
            (ElementKey::Number(x), ElementKey::Number(y)) => x == y,
            (ElementKey::Bool(x), ElementKey::Bool(y)) => x == y,
            (ElementKey::Null, ElementKey::Null) => true,
            (ElementKey::Name(x), ElementKey::Name(y)) => self.member_display_name(x) == self.member_display_name(y),
            _ => false,
        }
    }

    pub(super) fn var_decl_error_hint(&self, decl: &HirId<HirStmt>, field: Symbol) -> String {
        let visibility = self.layout_of(decl).map_or("", |layout| {
            if layout.is_public(field) { "pub " } else if layout.is_inner(field) { "inner " } else { "" }
        });
        format!("{visibility}var {}", self.hir.text(field))
    }
}

impl Place {
    pub(super) fn at(base: Base) -> Place {
        Place { base, steps: Vec::new() }
    }
}

impl<'a> Checker<'a> {
    pub(super) fn places_of(&self, node: &HirId<HirExpr>) -> Vec<Place> {
        match self.ctx.hir.get(node) {
            HirExpr::Assert(x) | HirExpr::Propagate(x) | HirExpr::Mut(x) => self.places_of(x),
            HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r) => {
                let mut out = self.places_of(l);
                out.extend(self.places_of(r));
                out
            },
            HirExpr::Index(target, member, _) | HirExpr::SafeAccess(target, member, _) => {
                let step = self.ctx.element_key(member);
                let mut places = self.places_of(target);
                for place in &mut places { place.steps.push(step); }
                places
            },
            HirExpr::Identifier(name) => vec![Place::at(match self.frame_index_of(*name) {
                Some(i) => Base::Local(i),
                None => Base::Unknown,
            })],
            HirExpr::Literal(_) | HirExpr::Construct(..) => vec![Place::at(Base::Fresh)],
            // A paren construction is a fresh value; any other call hands back what it chose.
            HirExpr::Call(..) => vec![Place::at(match self.out.constructions.contains(node) {
                true => Base::Fresh,
                false => Base::Unknown,
            })],
            _ => vec![Place::at(Base::Unknown)],
        }
    }

    pub(super) fn place_of(&self, node: &HirId<HirExpr>) -> Option<Place> {
        match &self.places_of(node)[..] {
            [only] => Some(only.clone()),
            _ => None,
        }
    }

    pub(super) fn owns_place(&self, place: &Place) -> bool {
        match (place.base, place.steps.len()) {
            (Base::Fresh, _) => true,
            (Base::Local(_), 0) => true,
            // One element of a mutable container this pass can name is tracked as an extraction.
            (Base::Local(i), 1) => self.holds_mutable(i),
            _ => false,
        }
    }

    pub(super) fn extraction_of(&self, value: &HirId<HirExpr>) -> Option<(usize, Option<ElementKey>)> {
        let place = self.place_of(value)?;
        let (Base::Local(i), [step]) = (place.base, &place.steps[..]) else { return None };
        self.holds_mutable(i).then_some((i, *step))
    }

    pub(super) fn shared_origin(&self, value: &HirId<HirExpr>) -> bool {
        let places = self.places_of(value);
        places.len() > 1 || !places.iter().all(|place| self.owns_place(place))
    }

    pub(super) fn holds_mutable(&self, i: usize) -> bool {
        self.locals[i].alias.mutability == Mutability::Mutable || !self.locals[i].alias.mutable_provenance.is_empty()
    }

    pub(super) fn write_permission(&self, i: usize) -> Mutability {
        if self.capture_writer(i).is_some() || self.transferred_write_ownership(i) {
            return Mutability::Immutable;
        }
        self.locals[i].alias.mutability
    }

    pub(super) fn transferred_at(&self, i: usize) -> Option<TransferSite> {
        self.locals[i].alias.transfer_site.filter(|s| s.transfer == WriteOwnershipTransfer::Transferred)
    }

    pub(super) fn transferred_write_ownership(&self, i: usize) -> bool {
        self.transferred_at(i).is_some()
    }

    /// Takes the write-ownership from every mutable-value holder the given value reaches.
    pub(super) fn transfer_write_ownership_as(&mut self, node: &HirId<HirExpr>, transfer: WriteOwnershipTransfer) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(node, &mut sources);
        let moved: Vec<usize> = sources.iter().map(|(i, _)| *i).collect();
        for (i, blame) in sources {
            // A binding that gave up its writing is a reader of the value.
            if self.locals[i].alias.writing_captor.is_some() {
                continue;
            }
            self.locals[i].alias.transfer_site = Some(TransferSite { node: blame, transfer });
        }
        moved
    }

    pub(super) fn check_can_transfer_write_ownership(&mut self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.local_of(node) else { return Ok(()) };
        let Some(site) = self.transferred_at(i) else { return Ok(()) };
        let name = self.ctx.hir.text(self.locals[i].name);
        Err(anyhow!("{}", Diagnostic::new(
            format!("cannot give away `{name}`, which no longer writes its value"),
            self.ctx.hir.pos(node).clone())
            .with_label(format!("`{name}` is given away here"))
            .with_span(self.ctx.hir.pos(&site.node).clone(), format!("`{name}` gave its write-ownership here"))))
    }

    pub(super) fn settle_unknown_transfer(&mut self, i: usize, _use_node: &HirId<HirExpr>) {
        let Some(site) = self.locals[i].alias.transfer_site else { return };
        let WriteOwnershipTransfer::Unknown(..) = site.transfer else { return };
        self.locals[i].alias.transfer_site = None;
    }

    pub(super) fn capture_enclosing(&mut self, name: Symbol, node: &HirId<HirExpr>) {
        let Some(i) = self.upvalue_index(name) else { return };
        if !self.fn_ctx.writes.is_some_and(|w| w.contains(&name)) {
            return;
        }
        let captor = self.fn_ctx.name;
        let local = &mut self.locals[i];
        if local.func.is_some() || local.alias.mutability != Mutability::Mutable {
            return;
        }
        match captor {
            // A named body takes over the writing. The value stays where it is.
            Some(captor) if local.alias.writing_captor.is_none() => local.alias.writing_captor = Some((captor, *node)),
            None if local.alias.transfer_site.is_none() => local.alias.transfer_site = Some(TransferSite { node: *node, transfer: WriteOwnershipTransfer::Transferred }),
            _ => {},
        }
    }

    /// The write-capture that left a binding only able to read, if one did. Answers with the slot
    /// that lost the writing, the closure that took it, and where it writes.
    pub(super) fn capture_writer(&self, i: usize) -> Option<(usize, Symbol, HirId<HirExpr>)> {
        let mut stack = vec![i];
        let mut seen: HashSet<usize> = HashSet::new();
        while let Some(j) = stack.pop() {
            if !seen.insert(j) {
                continue;
            }
            if let Some((captor, wrote_at)) = self.locals[j].alias.writing_captor {
                return Some((j, captor, wrote_at));
            }
            stack.extend(self.locals[j].alias.mutable_provenance.iter().copied());
        }
        None
    }

    /// Gives back the write-ownership a slot's old value held.
    pub(super) fn reclaim_on_rebind(&mut self, i: usize) {
        // A slot that already gave its write-ownership away has none to hand back.
        if self.locals[i].alias.transfer_site.is_some() {
            return;
        }
        let mut stack = self.locals[i].alias.mutable_provenance.clone();
        let mut seen: HashSet<usize> = HashSet::new();
        while let Some(s) = stack.pop() {
            if !seen.insert(s) {
                continue;
            }
            self.locals[s].alias.transfer_site = None;
            stack.extend(self.locals[s].alias.mutable_provenance.iter().copied());
        }
    }

    pub(super) fn reclaim_scoped_write_ownership(&mut self, mark: usize) {
        for i in (mark..self.locals.len()).rev() {
            if self.locals[i].alias.transfer_site.is_some() {
                continue;
            }
            let mut stack = self.locals[i].alias.mutable_provenance.clone();
            let mut seen: HashSet<usize> = HashSet::new();
            while let Some(s) = stack.pop() {
                if !seen.insert(s) {
                    continue;
                }
                if s < mark {
                    self.locals[s].alias.transfer_site = None;
                } else {
                    stack.extend(self.locals[s].alias.mutable_provenance.iter().copied());
                }
            }
        }
    }

    pub(super) fn reroot_provenance(&mut self, mark: usize) {
        for i in 0..mark.min(self.locals.len()) {
            if self.locals[i].alias.mutable_provenance.iter().all(|&s| s < mark) {
                continue;
            }
            let dying = std::mem::take(&mut self.locals[i].alias.mutable_provenance);
            self.locals[i].alias.mutable_provenance = self.surviving_sources(dying, mark);
        }
    }

    pub(super) fn drop_dead_extractions(&mut self, mark: usize) {
        for i in 0..mark.min(self.locals.len()) {
            let alias = &mut self.locals[i].alias;
            let kept = alias.extracted_from.len();
            alias.extracted_from.retain(|(source, _)| *source < mark);
            alias.shared_origin |= alias.extracted_from.len() != kept;
        }
    }

    /// Follows each source about to die to the sources it came from.
    fn surviving_sources(&self, roots: Vec<usize>, mark: usize) -> Vec<usize> {
        let mut out = Vec::new();
        let mut seen: HashSet<usize> = HashSet::new();
        let mut stack = roots;
        while let Some(s) = stack.pop() {
            if !seen.insert(s) {
                continue;
            }
            match s < mark {
                true => out.push(s),
                false => stack.extend(self.locals[s].alias.mutable_provenance.iter().copied()),
            }
        }
        out
    }

    pub(super) fn holds_borrowed_maybe_mutable(&self, expr: &HirId<HirExpr>) -> bool {
        self.local_of(expr).is_some_and(|i| self.locals[i].alias.borrowed_maybe_mutable)
    }

    /// Whether a value cannot leave the call it arrived in.
    pub(super) fn value_is_confined(&self, expr: &HirId<HirExpr>) -> bool {
        self.local_of(expr).is_some_and(|i| self.locals[i].alias.confined)
    }

    pub(super) fn claim_element_write_ownership(&mut self, i: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let origins = self.locals[i].alias.extracted_from.clone();
        if origins.is_empty() {
            // A value from somewhere unproven has no element to name, but still needs the slot.
            return match self.locals[i].alias.shared_origin {
                true => self.claim_write_ownership_at_runtime(i, node),
                false => Ok(()),
            };
        }

        // A key this pass can name conflicts here, before the program runs. Every origin is checked,
        // because the binding may have come out of any of them.
        for (container, key) in origins {
            let Some(key) = key else { continue };
            if let Some(holder) = (0..self.locals.len()).find(|&j| j != i && self.writes_element(j, container, &key)) {
                return Err(self.second_writer_error(i, holder, container, node));
            }
        }

        self.claim_write_ownership_at_runtime(i, node)
    }

    /// Whether a binding already holds the writer slot for this element of this container.
    pub(super) fn writes_element(&self, local: usize, container: usize, key: &ElementKey) -> bool {
        self.locals[local].alias.first_written_at.is_some()
            && self.locals[local].alias.extracted_from.iter()
                .any(|(c, k)| *c == container && k.is_some_and(|k| self.ctx.is_same_key(&k, key)))
    }

    pub(super) fn record_unshared_store(&mut self, i: usize, target: &HirId<HirExpr>) {
        // Forcing puts the arbitrating store back everywhere, so no proof is recorded at all.
        if self.ctx.force_checks {
            return;
        }
        let local = &mut self.locals[i];
        if local.alias.may_be_shared || local.alias.writing_captor.is_some() {
            return;
        }
        local.alias.unshared_stores.push(*target);
    }

    pub(super) fn settle_unshared_stores(&mut self, mark: usize) {
        let from = mark.min(self.locals.len());
        for local in &mut self.locals[from..] {
            if local.alias.may_be_shared {
                continue;
            }
            self.out.unshared_stores.extend(local.alias.unshared_stores.drain(..));
        }
    }

    pub(super) fn claim_write_ownership_at_runtime(&mut self, local: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.locals[local].alias.holds_write_ownership = true;
        self.locals[local].alias.first_written_at.get_or_insert(*node);
        Ok(())
    }

    pub(super) fn scope_holds_write_ownership(&self, mark: usize, handed_over: usize) -> bool {
        self.locals[mark.min(self.locals.len())..].iter().any(|l| l.alias.holds_write_ownership)
            || self.element_write_ownerships_transferred > handed_over
    }

    pub(super) fn second_writer_error(&self, writer: usize, holder: usize, container: usize, node: &HirId<HirExpr>) -> anyhow::Error {
        let (name, other) = (self.ctx.hir.text(self.locals[writer].name), self.ctx.hir.text(self.locals[holder].name));
        let aggregate = self.ctx.hir.text(self.locals[container].name);
        let held_at = self.locals[holder].alias.first_written_at.expect("the holder of a writer slot wrote it");
        self.error_ctx_help("cannot write an element another name already writes",
            self.ctx.hir.pos(node), format!("`{name}` writes it here"),
            self.ctx.hir.pos(&held_at), format!("`{other}` already writes it here"),
            format!("one element of `{aggregate}` has one writer, and `{other}` is it; read through `{name}` instead of writing, or let `{other}` go out of scope first"))
    }

    pub(super) fn check_container_element_mutability(&mut self, immutable: bool, elem: &ValueState, elem_node: &HirId<HirExpr>, container: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match elem.mutability {
            Mutability::Mutable => Err(self.ctx.mutable_in_immutable_error(elem_node)),
            Mutability::Unknown => { self.record_seal_check(container); Ok(()) },
            Mutability::Immutable if self.holds_borrowed_maybe_mutable(elem_node) => {
                self.record_seal_check(container);
                Ok(())
            },
            Mutability::Immutable => Ok(()),
        }
    }

    pub(super) fn check_construct_field_mutability(&mut self, immutable: bool, value: &ValueState, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match value.mutability {
            Mutability::Mutable => Err(self.ctx.mutable_in_immutable_error(node)),
            Mutability::Unknown => { self.record_guard(node, Guard::Immutable); Ok(()) },
            Mutability::Immutable if self.holds_borrowed_maybe_mutable(node) => {
                self.record_guard(node, Guard::Immutable);
                Ok(())
            },
            Mutability::Immutable => { self.record_elision(node, Guard::Immutable); Ok(()) },
        }
    }

    pub(super) fn capture_write_error(&self, name: Symbol, use_site: &HirId<HirExpr>, owner: usize, captor: Symbol, wrote_at: &HirId<HirExpr>) -> anyhow::Error {
        let text = self.ctx.hir.text(name);
        // The capture may be on a slot this one took its value from, so that slot names the write.
        let owner = self.ctx.hir.text(self.locals[owner].name);
        let captor = self.ctx.hir.text(captor);
        let who = if captor == "lambda" { "a closure".to_string() } else { format!("function `{captor}`") };
        anyhow!("{}", Diagnostic::new(format!("cannot mutate a value whose write-ownership moved to {who}"), self.ctx.hir.pos(use_site).clone())
            .with_label(format!("`{text}` is mutated here"))
            .with_span(self.ctx.hir.pos(wrote_at).clone(), format!("`{owner}` is written here, which takes write-ownership"))
            .with_help(format!("a value has one write-owner; reading `{text}` is still fine, but to write it here, pass it to a `mut` function parameter instead of capturing it, or `copy` it into the closure")))
    }

    pub(super) fn reader_write_error(&self, i: usize, use_site: &HirId<HirExpr>) -> Option<anyhow::Error> {
        if let Some((owner, captor, wrote_at)) = self.capture_writer(i) {
            return Some(self.capture_write_error(self.locals[i].name, use_site, owner, captor, &wrote_at));
        }
        self.transferred_write_error(i, use_site)
    }

    fn transferred_write_error(&self, i: usize, use_site: &HirId<HirExpr>) -> Option<anyhow::Error> {
        let site = self.transferred_at(i)?;
        let name = self.ctx.hir.text(self.locals[i].name);
        Some(anyhow!("{}", Diagnostic::new(
            format!("cannot write `{name}`, which gave its write-ownership away"),
            self.ctx.hir.pos(use_site).clone())
            .with_label(format!("`{name}` is written here"))
            .with_span(self.ctx.hir.pos(&site.node).clone(), format!("`{name}` gave it away here"))
            .with_help(format!("reading `{name}` is still fine; to write the value, go through whatever took it"))))
    }

    fn names_immutable_param(&self, value: &HirId<HirExpr>) -> bool {
        self.local_of(value).is_some_and(|i| {
            self.locals[i].param && self.locals[i].alias.mutability == Mutability::Immutable
        })
    }

    fn names_immutable_param_binder(&self, value: &HirId<HirExpr>) -> bool {
        self.local_of(value).is_some_and(|i| {
            self.locals[i].binder == Some(BinderSource::Param)
                && self.locals[i].alias.mutability == Mutability::Immutable
        })
    }

    pub(super) fn reader_write_error_of(&self, value: &HirId<HirExpr>) -> Option<anyhow::Error> {
        self.reader_write_error(self.local_of(value)?, value)
    }


    pub(super) fn reachable_sources(&self, node: &HirId<HirExpr>, out: &mut Vec<(usize, HirId<HirExpr>)>) {
        match self.ctx.hir.get(node) {
            HirExpr::Identifier(name) => {
                if let Some(i) = self.frame_index_of(*name) {
                    if self.holds_mutable(i) {
                        out.push((i, *node));
                    }
                }
            },
            // A brace also persists its field values into the new instance, so each escapes.
            HirExpr::Construct(_, brace) => for (_, v) in brace { self.reachable_sources(v, out); },
            HirExpr::Call(callee, args) => self.reachable_call_args(callee, args, out),
            _ => for c in self.ctx.hir.ownership_children(node) { self.reachable_sources(&c, out); },
        }
    }

    pub(super) fn reachable_call_args(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], out: &mut Vec<(usize, HirId<HirExpr>)>) {
        if let Some(init) = self.ctx.constructor_init(callee) {
            for (i, a) in args.iter().enumerate() {
                if self.ctx.sigs.param_escapes_at(&init, i) {
                    self.reachable_sources(a, out);
                }
            }
            return;
        }
        let Some(func) = self.resolved_callees.get(callee).copied() else { return };
        for (i, a) in args.iter().enumerate() {
            if self.ctx.sigs.hands_back_itself_at(&func, i) {
                self.reachable_sources(a, out);
            }
        }
        // A method's receiver rides the row position after its declared parameters.
        if let HirExpr::Index(receiver, _, _) = self.ctx.hir.get(callee) {
            if self.ctx.sigs.hands_back_itself_at(&func, args.len()) {
                self.reachable_sources(receiver, out);
            }
        }
        // A result that may be a binding from an outer scope names that binding a second time, so
        // it reaches whatever the caller holds under the same name.
        for &name in self.ctx.sigs.returns_free(&func) {
            if let Some(i) = self.frame_index_of(name).filter(|&i| self.holds_mutable(i)) {
                out.push((i, *callee));
            }
        }
    }

    pub(super) fn transfer_write_ownership(&mut self, node: &HirId<HirExpr>) -> Result<Vec<usize>, anyhow::Error> {
        self.check_can_transfer_write_ownership(node)?;
        Ok(self.transfer_write_ownership_as(node, WriteOwnershipTransfer::Transferred))
    }

    pub(super) fn claim_receiver_write_ownership(&mut self, receiver: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.local_of(receiver) else {
            // A claim is recorded against a frame-local. The receiver is reached through a path,
            // or its base lives outside this frame. In either case, nothing records one for it.
            return Ok(())
        };
        self.claim_element_write_ownership(i, receiver)
    }

    pub(super) fn source_indices(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(value, &mut sources);
        sources.into_iter().map(|(i, _)| i).collect()
    }

    pub(super) fn provenance_of(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut out = self.source_indices(value);
        out.extend(self.captured_sources(value));
        out
    }

    pub(super) fn captured_sources(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let HirExpr::Literal(HirLiteral::Lambda(_)) = self.ctx.hir.get(value) else { return Vec::new() };
        let Some(writes) = self.ctx.sigs.lambda_writes.get(value) else { return Vec::new() };
        writes.iter().filter_map(|name| {
            let i = self.frame_index_of(*name)?;
            self.holds_mutable(i).then_some(i)
        }).collect()
    }

    pub(super) fn store_into_container(&mut self, debt: &Debt, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.ctx.reject_outliving(debt, Site::Container, expr)?;
        self.store_into_container_guard(expr)?;
        self.transfer_write_ownership(expr)?;
        Ok(())
    }

    pub(super) fn store_into_container_guard(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.extraction_of(expr).is_some() || self.may_be_an_element(expr) {
            self.record_guard(expr, Guard::StoreIntoContainer);
            self.element_write_ownerships_transferred += 1;
        }
        Ok(())
    }

    fn may_be_an_element(&self, expr: &HirId<HirExpr>) -> bool {
        self.local_of(expr).is_some_and(|i| self.locals[i].param)
    }

    pub(super) fn upvalue_binding_of(&self, node: &HirId<HirExpr>) -> Option<usize> {
        let HirExpr::Identifier(name) = self.ctx.hir.get(node) else { return None };
        if !matches!(self.ctx.bindings.place_of(node), Some(crate::middle::bind::Place::Upvalue(_))) {
            return None;
        }
        self.upvalue_index(*name)
    }

    pub(super) fn local_of(&self, node: &HirId<HirExpr>) -> Option<usize> {
        match self.place_of(node)? {
            Place { base: Base::Local(i), steps } if steps.is_empty() => Some(i),
            _ => None,
        }
    }

    pub(super) fn escape_via_capture_error(&self, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.upvalue_index(name) else { return Ok(()) };
        if self.locals[i].alias.borrowed && !self.locals[i].alias.confined {
            let text = self.ctx.hir.text(name);
            return Err(self.error_help(
                "a borrowed value cannot be retained, and this closure outlives the call".to_string(), node,
                format!("declare the parameter `*{text}` to retain it, or `*mut {text}` if the closure writes it")));
        }
        let owed = self.locals[i].owed.clone();
        self.ctx.reject_outliving(&Debt::Owed { obligations: owed, definite: false, container: false }, Site::Capture, node)
    }

    pub(super) fn check_opaque_call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], arg_types: &[ValueState]) -> Result<ValueState, anyhow::Error> {
        let mut survive = Vec::new();
        for (i, state) in arg_types.iter().enumerate() {
            if let Some(owed) = self.ctx.obligation_preventing_escape(&state.debt) {
                survive.push((i as u8, Some(owed)));
                continue;
            }
            if state.mutability != Mutability::Mutable {
                continue;
            }
            if self.arg_is_borrowed(&args[i]) {
                survive.push((i as u8, None));
                continue;
            }
            // An owned mutable may be retained by the callee or merely borrowed, and this pass
            // cannot tell which.
            self.transfer_write_ownership_as(&args[i], WriteOwnershipTransfer::Unknown(*callee, i as u8));
        }
        if !survive.is_empty() {
            self.record_survive_barrier(callee, survive);
        }
        self.indirect_call(callee)
    }

    pub(super) fn check_arg_mutability(&self, callee: &HirId<HirExpr>, markers: &[Capability], arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &marker) in markers.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };

            if marker.is_mut() && state.mutability == Mutability::Immutable {
                return Err(self.ctx.param_needs_mut_error(callee, &args[i]));
            }

            if marker.is_retain() && self.arg_is_borrowed(&args[i]) {
                return Err(self.consumes_borrow_error(callee, &args[i]));
            }
        }
        Ok(())
    }

    pub(super) fn transfer_obligations_into_receiver(&mut self, receiver: &HirId<HirExpr>, values: &[ValueState]) {
        let mut obligations = HashSet::new();
        for state in values {
            if let Debt::Owed { obligations: o, .. } = &state.debt {
                obligations.extend(o.iter().copied());
            }
        }
        if obligations.is_empty() {
            return;
        }
        let HirExpr::Identifier(name) = self.ctx.hir.get(receiver) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        self.locals[i].owed.extend(obligations);
        self.locals[i].container = true;
    }

    pub(super) fn consume_move_args(&mut self, markers: &[Capability], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &marker) in markers.iter().enumerate() {
            if marker.is_retain() {
                if let Some(arg) = args.get(i) { self.transfer_write_ownership(arg)?; }
            }
        }
        Ok(())
    }

    pub(super) fn borrowed_param_pos(&self, arg: &HirId<HirExpr>) -> Option<&SourcePosition> {
        let HirExpr::Identifier(name) = self.ctx.hir.get(arg) else { return None };
        self.fn_ctx.params.iter().find(|(sym, _)| sym == name).map(|(_, pos)| pos)
    }

    pub(super) fn arg_is_borrowed(&self, arg: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.ctx.hir.get(arg) else { return false };
        self.frame_index_of(*name).is_some_and(|i| self.locals[i].alias.borrowed)
    }

    pub(super) fn immutable_receiver_error(&self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, reason: &str) -> anyhow::Error {
        if let Some(err) = self.reader_write_error_of(receiver) {
            return err;
        }
        let subject = self.ctx.receiver_subject(receiver);
        let method = self.ctx.callee_display_name(callee);
        let help = if self.names_immutable_param_binder(receiver) {
            format!("{method} {reason}; mark the parameter `mut (..)` to destructure a mutable value, or work through the value it came out of")
        } else if self.names_immutable_param(receiver) {
            format!("{method} {reason}; declare the parameter `mut` to let {subject} be mutated")
        } else {
            format!("{method} {reason}; construct the value with `mut` to call it")
        };
        anyhow!("{}", Diagnostic::new("expected mutable receiver".to_string(), self.ctx.hir.pos(receiver).clone())
            .with_label(format!("{subject} is immutable"))
            .with_help(help))
    }

    pub(super) fn consumes_borrow_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let (a, c) = (self.ctx.arg_display_name(arg), self.ctx.callee_display_name(callee));
        let mut diag = Diagnostic::new(format!("cannot move borrowed value {a}"), self.ctx.hir.pos(arg).clone())
            .with_label(format!("{a} is moved here"));
        if let Some(param_pos) = self.borrowed_param_pos(arg) {
            let fname = self.fn_ctx.name.map_or("this function".to_string(), |s| format!("`{}`", self.ctx.hir.text(s)));
            diag = diag.with_context_span(param_pos.clone(), format!("{fname} only borrows {a} here; it does not own it"));
        }
        diag = diag
            .with_context_span(self.ctx.hir.pos(callee).clone(), format!("{c} consumes {a}"))
            .with_help(format!("take ownership of {a} with `*mut` to move it into {c}"));
        anyhow!("{}", diag)
    }

    pub(super) fn immutable_mutation_error(&self, target: &HirId<HirExpr>, i: usize) -> anyhow::Error {
        if let Some(err) = self.reader_write_error(i, target) {
            return err;
        }
        let name = self.ctx.hir.text(self.locals[i].name);
        if self.locals[i].param {
            return self.error_help(
                format!("cannot mutate `{name}`, a read-only parameter"), target,
                format!("declare the parameter `mut` to let `{name}` be mutated"));
        }
        self.error_labeled("cannot mutate an immutable value".to_string(), target, format!("`{name}` is immutable"))
    }

    pub(super) fn is_readonly(&self, target: &HirId<HirExpr>) -> bool {
        if self.ctx.is_this(target) {
            return !self.checking_factory && self.this_valuestate().mutability == Mutability::Immutable;
        }
        self.root_local_of(target).is_some_and(|i| self.write_permission(i) == Mutability::Immutable)
    }

    /// The binding a write target is rooted in.
    fn root_local_of(&self, target: &HirId<HirExpr>) -> Option<usize> {
        match self.place_of(target)?.base {
            Base::Local(i) => Some(i),
            _ => None,
        }
    }

    pub(super) fn readonly_write_error(&self, target: &HirId<HirExpr>) -> anyhow::Error {
        if !self.ctx.is_this(target) {
            if let Some(err) = self.root_local_of(target).and_then(|i| self.reader_write_error(i, target)) {
                return err;
            }
            return self.error_labeled("cannot mutate an immutable value".to_string(), target,
                "reached through an immutable value");
        }
        let method = self.fn_ctx.name.map_or("this method".to_string(), |s| format!("`{}`", self.ctx.hir.text(s)));
        self.error_help("cannot mutate through a read-only receiver".to_string(), target,
            format!("declare {method}'s receiver `mut this` to let it mutate the instance"))
    }

    pub(super) fn readonly_receiver_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.ctx.qualified_field_display_name(decl, field);
        let method = self.fn_ctx.name.map_or("this method".to_string(), |s| format!("`{}`", self.ctx.hir.text(s)));
        self.error_help(format!("cannot assign `{name}` through a read-only receiver"), lhs,
            format!("declare {method}'s receiver `mut this` to let it mutate the instance"))
    }

    pub(super) fn attach_field_provenance(&mut self, target: &HirId<HirExpr>, rhs: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.ctx.hir.get(target) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        let sources = self.source_indices(rhs);
        self.locals[i].alias.mutable_provenance.extend(sources);
    }

}
