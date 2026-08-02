//! Aliasing: which value a name denotes, who may write it, and when that is given back.
//!
//! The family that answers one-writer questions, wherever they are asked: at a binding, a store,
//! a capture, or an argument.

use crate::middle::diagnose::Diagnose;
use std::collections::HashSet;

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::hir::{Capability, HirExpr, HirId, HirLiteral, HirStmt, Symbol};

use super::{Checker, Flow, Guard, Mutability, Site, Typed};

/// Why a mutable binding was moved.
#[derive(Clone, Copy)]
pub(super) enum MoveCause {
    /// Bound, stored, returned, or passed to a consuming parameter.
    Value,
    /// Passed to a callee this pass cannot resolve, which may or may not have consumed it. Reading
    /// the binding again settles it, by making the call prove the callee only borrowed. Carries the
    /// callee node and argument position that proof needs.
    Opaque(HirId<HirExpr>, u8),
}

/// Where and why a mutable binding was moved out.
#[derive(Clone, Copy)]
pub(super) struct MovedAt {
    pub(super) node: HirId<HirExpr>,
    pub(super) cause: MoveCause,
}

/// Which element within an aggregate a name holds. A dict keys on any value, so a key is any
/// literal, not just an index.
#[derive(Clone, Copy)]
pub(super) enum ElementKey {
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
    /// The capability of the value in the slot.
    pub(super) mutability: Mutability,
    pub(super) borrowed: bool,
    /// Where the value was moved out, or `None` while the binding is live.
    pub(super) move_site: Option<MovedAt>,
    /// The prior slots this binding took its mutable value from.
    pub(super) provenance: Vec<usize>,
    /// The closure that took over writing this binding's value, and where it writes it.
    pub(super) write_owner: Option<(Symbol, HirId<HirExpr>)>,
    /// The aggregate slot and key this binding read its value out of.
    pub(super) extracted_from: Option<(usize, Option<ElementKey>)>,
    /// Whether this binding holds a runtime writer slot.
    pub(super) slot_taken: bool,
    /// Whether the value may already be named somewhere this pass cannot see.
    pub(super) shared_origin: bool,
    /// Where this binding first wrote the element it names.
    pub(super) wrote_at: Option<HirId<HirExpr>>,
}

/// Where a value lives: the thing a path starts from, and each element read out of it. Two places
/// with the same base and the same steps name the same value.
#[derive(Clone)]
pub(super) struct Place {
    pub(super) base: Base,
    /// One element read per entry, outermost first. `None` is an element this pass cannot name.
    pub(super) steps: Vec<Option<ElementKey>>,
}


/// What a place is rooted in.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum Base {
    /// A binding in the current frame.
    Local(usize),
    /// A value made right here, which nothing else can already name.
    Fresh,
    /// A receiver, a call result, or a form this pass does not model. Owned by nobody it can name,
    /// which is what keeps an unmodelled form guarded rather than ignored.
    Unknown,
}


impl Place {
    pub(super) fn at(base: Base) -> Place {
        Place { base, steps: Vec::new() }
    }
}

impl<'a> Checker<'a> {
    /// Every place an expression may denote.
    pub(super) fn denotes(&self, node: &HirId<HirExpr>) -> Vec<Place> {
        match self.hir.get(node) {
            HirExpr::Assert(x) | HirExpr::Propagate(x) | HirExpr::Mut(x) => self.denotes(x),
            HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r) => {
                let mut out = self.denotes(l);
                out.extend(self.denotes(r));
                out
            },
            HirExpr::Index(target, member, _) | HirExpr::SafeAccess(target, member, _) => {
                let step = self.element_key(member);
                let mut places = self.denotes(target);
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

    /// The one place an expression denotes, when it denotes exactly one. Two branches name no one
    /// place, so a rule needing a single answer gets none.
    pub(super) fn place_of(&self, node: &HirId<HirExpr>) -> Option<Place> {
        match &self.denotes(node)[..] {
            [only] => Some(only.clone()),
            _ => None,
        }
    }

    /// The element a member expression names. A literal pins one element. A computed one could be
    /// any of them, so it names none and is left to a runtime check.
    pub(super) fn element_key(&self, member: &HirId<HirExpr>) -> Option<ElementKey> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::Number(n)) => Some(ElementKey::Number(*n)),
            HirExpr::Literal(HirLiteral::Boolean(b)) => Some(ElementKey::Bool(*b)),
            HirExpr::Literal(HirLiteral::Null) => Some(ElementKey::Null),
            HirExpr::Literal(HirLiteral::String(_)) => Some(ElementKey::Name(*member)),
            _ => None,
        }
    }

    /// Whether two reads name the same element. A number keeps its own value rather than an index,
    /// so a fractional key stays distinct from the whole number it sits next to.
    pub(super) fn same_key(&self, a: &ElementKey, b: &ElementKey) -> bool {
        match (a, b) {
            (ElementKey::Number(x), ElementKey::Number(y)) => x == y,
            (ElementKey::Bool(x), ElementKey::Bool(y)) => x == y,
            (ElementKey::Null, ElementKey::Null) => true,
            (ElementKey::Name(x), ElementKey::Name(y)) => self.member_text(x) == self.member_text(y),
            _ => false,
        }
    }

    /// Whether a form yields a value nothing else can already name.
    pub(super) fn owns_place(&self, place: &Place) -> bool {
        match (place.base, place.steps.len()) {
            // A value made here is nobody else's.
            (Base::Fresh, _) => true,
            // A name is move-tracked, so a second one for its value is caught there.
            (Base::Local(_), 0) => true,
            // One element of a mutable container this pass can name is tracked as an extraction.
            (Base::Local(i), 1) => self.holds_mutable(i),
            _ => false,
        }
    }

    /// The aggregate slot and key a value was read out of, when it names one directly. Only a
    /// mutable aggregate matters, since elements of an immutable one cannot be written anyway.
    pub(super) fn extraction_of(&self, value: &HirId<HirExpr>) -> Option<(usize, Option<ElementKey>)> {
        let place = self.place_of(value)?;
        let (Base::Local(i), [step]) = (place.base, &place.steps[..]) else { return None };
        self.holds_mutable(i).then_some((i, *step))
    }

    /// Whether a value's origin leaves it possibly named elsewhere. A call hands back whatever the
    /// callee chose, which may be a value the callee still names. A construction is a fresh value,
    /// so it is the one call shape that is proven exclusive.
    pub(super) fn shared_origin(&self, value: &HirId<HirExpr>) -> bool {
        // With more than one branch there is no single place to claim, so it takes the runtime
        // slot rather than claiming one statically.
        let places = self.denotes(value);
        places.len() > 1 || !places.iter().all(|place| self.owns_place(place))
    }

    /// Whether local `i` holds a mutable value: it owns or borrows one directly, or took one from
    /// other sources. These are the slots move tracking follows.
    pub(super) fn holds_mutable(&self, i: usize) -> bool {
        self.locals[i].alias.mutability == Mutability::Mutable || !self.locals[i].alias.provenance.is_empty()
    }

    /// The mutability a binding hands on. A closure that took over the writing leaves it a reader,
    /// so it yields the value read-only however it was declared.
    pub(super) fn effective_mutability(&self, i: usize) -> Mutability {
        match self.locals[i].alias.write_owner {
            Some(_) => Mutability::Immutable,
            None => self.locals[i].alias.mutability,
        }
    }

    /// Marks every mutable-value holder a moved value reaches as moved out, for the given reason.
    pub(super) fn move_source_because(&mut self, node: &HirId<HirExpr>, cause: MoveCause) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(node, &mut sources);
        let moved: Vec<usize> = sources.iter().map(|(i, _)| *i).collect();
        for (i, blame) in sources {
            // A binding that gave up its writing is a reader of the value.
            if self.locals[i].alias.write_owner.is_some() {
                continue;
            }
            self.locals[i].alias.move_site = Some(MovedAt { node: blame, cause });
        }
        moved
    }

    /// Settles a moved binding at a use. An opaque callee may not have consumed it. The use demands
    /// the runtime proof of that, which makes the binding live again.
    pub(super) fn check_moved(&mut self, i: usize, use_node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(site) = self.locals[i].alias.move_site else { return Ok(()) };
        let MoveCause::Opaque(callee, position) = site.cause else {
            return Err(self.use_after_move_error(self.locals[i].name, use_node, site));
        };
        self.record_reread_barrier(&callee, position, *use_node);
        self.locals[i].alias.move_site = None;
        Ok(())
    }

    /// Moves an enclosing-frame mutable binding when a nested function writes it. A read-only
    /// capture borrows the value, so the enclosing binding stays live.
    pub(super) fn capture_enclosing(&mut self, name: Symbol, node: &HirId<HirExpr>) {
        let Some(i) = self.enclosing_index(name) else { return };
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
            Some(captor) if local.alias.write_owner.is_none() => local.alias.write_owner = Some((captor, *node)),
            None if local.alias.move_site.is_none() => local.alias.move_site = Some(MovedAt { node: *node, cause: MoveCause::Value }),
            _ => {},
        }
    }

    /// The write-capture that left a binding only able to read, if one did. The value can come from
    /// another slot, so the capture may be anywhere in the provenance.
    pub(super) fn capture_writer(&self, i: usize) -> Option<(usize, Symbol, HirId<HirExpr>)> {
        let mut stack = vec![i];
        let mut seen: HashSet<usize> = HashSet::new();
        while let Some(j) = stack.pop() {
            if !seen.insert(j) {
                continue;
            }
            if let Some((captor, wrote_at)) = self.locals[j].alias.write_owner {
                return Some((j, captor, wrote_at));
            }
            stack.extend(self.locals[j].alias.provenance.iter().copied());
        }
        None
    }

    /// When a block-local dies still holding its moved value, hands the value back to a surviving
    /// source. A local moved out on some path holds nothing, so it hands back nothing. The nearest
    /// source that outlives the block becomes the live holder again, along every path, past cycles.
    pub(super) fn revive_scoped_sources(&mut self, mark: usize) {
        for i in (mark..self.locals.len()).rev() {
            if self.locals[i].alias.move_site.is_some() {
                continue;
            }
            let mut stack = self.locals[i].alias.provenance.clone();
            let mut seen: HashSet<usize> = HashSet::new();
            while let Some(s) = stack.pop() {
                if !seen.insert(s) {
                    continue;
                }
                if s < mark {
                    self.locals[s].alias.move_site = None;
                } else {
                    stack.extend(self.locals[s].alias.provenance.iter().copied());
                }
            }
        }
    }

    /// Whether a value may turn out to be a borrow. Only a parameter can receive one, and a `mut`
    /// parameter is already tracked, so an unmarked one is the case left to the runtime.
    pub(super) fn holds_unproven_borrow(&self, expr: &HirId<HirExpr>) -> bool {
        self.local_of(expr).is_some_and(|i| {
            self.locals[i].param && self.locals[i].alias.mutability != Mutability::Mutable
        })
    }

    /// Takes the one writer slot for the element this binding reads, so a second writer for the
    /// same element is rejected. A binding that is not an extraction writes whatever it owns.
    pub(super) fn claim_element_write(&mut self, i: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some((container, key)) = self.locals[i].alias.extracted_from else {
            // A value from somewhere unproven has no element to name, but still needs the slot.
            return match self.locals[i].alias.shared_origin {
                true => self.claim_at_runtime(i, node),
                false => Ok(()),
            };
        };
        // A key this pass can name conflicts here, before the program runs.
        if let Some(key) = key {
            if let Some(holder) = (0..self.locals.len()).find(|&j| j != i && self.writes_element(j, container, &key)) {
                return Err(self.second_writer_error(i, holder, container, node));
            }
        }
        self.claim_at_runtime(i, node)
    }

    /// Whether a binding already holds the writer slot for this element of this aggregate.
    pub(super) fn writes_element(&self, local: usize, container: usize, key: &ElementKey) -> bool {
        self.locals[local].alias.wrote_at.is_some()
            && matches!(self.locals[local].alias.extracted_from, Some((c, Some(k))) if c == container && self.same_key(&k, key))
    }

    /// Takes the runtime writer slot, whether or not this pass could name the element. Leaving it
    /// to the named half alone would let a named and an unnamed key for one element miss each
    /// other, since neither sees the other's record.
    pub(super) fn claim_at_runtime(&mut self, local: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // Every write takes the slot, not just the first. A write the checker sees is not always a
        // write that runs, so keying the take to one of them would leave the others unchecked.
        self.record_guard(node, Guard::WriteThroughName);
        self.locals[local].alias.slot_taken = true;
        self.locals[local].alias.wrote_at.get_or_insert(*node);
        Ok(())
    }

    /// How many of the locals a scope drops hold a runtime writer slot.
    pub(super) fn scope_holds_write_ownership(&self, mark: usize) -> bool {
        self.locals[mark.min(self.locals.len())..].iter().any(|l| l.alias.slot_taken)
    }

    /// The error for a second name writing one element of an aggregate.
    pub(super) fn second_writer_error(&self, writer: usize, holder: usize, container: usize, node: &HirId<HirExpr>) -> anyhow::Error {
        let (name, other) = (self.hir.text(self.locals[writer].name), self.hir.text(self.locals[holder].name));
        let aggregate = self.hir.text(self.locals[container].name);
        let held_at = self.locals[holder].alias.wrote_at.expect("the holder of a writer slot wrote it");
        self.error_ctx_help("cannot write an element another name already writes",
            self.hir.pos(node), format!("`{name}` writes it here"),
            self.hir.pos(&held_at), format!("`{other}` already writes it here"),
            format!("one element of `{aggregate}` has one writer, and `{other}` is it; read through `{name}` instead of writing, or let `{other}` go out of scope first"))
    }

    /// Enforces deep immutability at a container element.
    pub(super) fn check_container_element(&mut self, immutable: bool, elem: &Typed, elem_node: &HirId<HirExpr>, container: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match elem.mutability {
            Mutability::Mutable => Err(self.mutable_in_immutable_error(elem_node)),
            Mutability::Unknown => { self.record_seal_check(container); Ok(()) },
            Mutability::Immutable => Ok(()),
        }
    }

    /// Enforces deep immutability at a construction's field.
    pub(super) fn check_construct_field(&mut self, immutable: bool, value: &Typed, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match value.mutability {
            Mutability::Mutable => Err(self.mutable_in_immutable_error(node)),
            Mutability::Unknown => { self.record_guard(node, Guard::Immutable); Ok(()) },
            Mutability::Immutable => Ok(()),
        }
    }

    pub(super) fn use_after_move_error(&self, name: Symbol, use_site: &HirId<HirExpr>, moved: MovedAt) -> anyhow::Error {
        let text = self.hir.text(name);
        match moved.cause {
            MoveCause::Value => {
                // A loop's re-check reads the value at the very node that moved it, so one caret is
                // clearer than two on the same spot.
                if *use_site == moved.node {
                    return self.loop_move_error(name, moved);
                }
                // Caret both the use and the move so the reader sees where the value went.
                anyhow!("{}", Diagnostic::new("value used after it was moved".to_string(), self.hir.pos(use_site).clone())
                    .with_label(format!("`{text}` used here"))
                    .with_span(self.hir.pos(&moved.node).clone(), format!("`{text}` moved here")))
            },
            // A pending opaque consume is settled at the read, so it never reaches here.
            MoveCause::Opaque(..) => unreachable!("an opaque consume is resolved where it is read"),
        }
    }

    /// The error for writing a value that a capture now writes. Nothing moved but the write-
    /// ownership, so the message says that rather than calling the value gone.
    pub(super) fn capture_write_error(&self, name: Symbol, use_site: &HirId<HirExpr>, owner: usize, captor: Symbol, wrote_at: &HirId<HirExpr>) -> anyhow::Error {
        let text = self.hir.text(name);
        // The capture may be on a slot this one took its value from, so that slot names the write.
        let owner = self.hir.text(self.locals[owner].name);
        let captor = self.hir.text(captor);
        let who = if captor == "lambda" { "a closure".to_string() } else { format!("function `{captor}`") };
        anyhow!("{}", Diagnostic::new(format!("cannot mutate a value whose write-ownership moved to {who}"), self.hir.pos(use_site).clone())
            .with_label(format!("`{text}` is mutated here"))
            .with_span(self.hir.pos(wrote_at).clone(), format!("`{owner}` is written here, which takes write-ownership"))
            .with_help(format!("a value has one write-owner; reading `{text}` is still fine, but to write it here, pass it to a `mut` function parameter instead of capturing it, or `copy` it into the closure")))
    }

    /// The write error for a binding that only reads a value a closure writes, if it is one.
    pub(super) fn reader_write_error(&self, i: usize, use_site: &HirId<HirExpr>) -> Option<anyhow::Error> {
        let (owner, captor, wrote_at) = self.capture_writer(i)?;
        Some(self.capture_write_error(self.locals[i].name, use_site, owner, captor, &wrote_at))
    }

    /// Whether a value expression names a read-only parameter. Such a receiver is fixed by marking
    /// the parameter, not by constructing the value differently.
    pub(super) fn names_readonly_param(&self, value: &HirId<HirExpr>) -> bool {
        self.local_of(value).is_some_and(|i| self.locals[i].param)
    }

    /// The write error for a value expression naming a binding whose value a closure writes.
    pub(super) fn reader_write_error_of(&self, value: &HirId<HirExpr>) -> Option<anyhow::Error> {
        self.reader_write_error(self.local_of(value)?, value)
    }

    /// Moving a value reads it, so a loop body that moves one reads a moved value on the next pass.
    /// The read and the move are the same spot, so a single caret marks it.
    pub(super) fn loop_move_error(&self, name: Symbol, moved: MovedAt) -> anyhow::Error {
        let text = self.hir.text(name);
        anyhow!("{}", Diagnostic::new("value used after it was moved".to_string(), self.hir.pos(&moved.node).clone())
            .with_label(format!("`{text}` is moved here, then read again on the next loop iteration")))
    }

    /// The mutable-value holders a value expression reaches, each with the node to blame.
    pub(super) fn reachable_sources(&self, node: &HirId<HirExpr>, out: &mut Vec<(usize, HirId<HirExpr>)>) {
        match self.hir.get(node) {
            HirExpr::Identifier(name) => {
                if let Some(i) = self.frame_index_of(*name) {
                    if self.holds_mutable(i) {
                        out.push((i, *node));
                    }
                }
            },
            // A brace also persists its field values into the new instance, so each escapes.
            HirExpr::Construct(callee, args, brace) => {
                self.reachable_call_args(callee, args, out);
                for (_, v) in brace { self.reachable_sources(v, out); }
            },
            HirExpr::Call(callee, args) => self.reachable_call_args(callee, args, out),
            _ => for c in self.hir.ownership_children(node) { self.reachable_sources(&c, out); },
        }
    }

    /// The sources a call's result keeps reachable. A constructor keeps the arguments its init
    /// persists. Any other resolved callee keeps the ones it hands back, so binding the result names
    /// them a second time. An unresolved callee answers nothing, which the runtime barrier covers.
    pub(super) fn reachable_call_args(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], out: &mut Vec<(usize, HirId<HirExpr>)>) {
        if let Some(init) = self.constructor_init(callee) {
            for (i, a) in args.iter().enumerate() {
                if self.sigs.param_escapes_at(&init, i) {
                    self.reachable_sources(a, out);
                }
            }
            return;
        }
        let Some(func) = self.resolved_callees.get(callee).copied() else { return };
        for (i, a) in args.iter().enumerate() {
            if self.sigs.hands_back_itself_at(&func, i) {
                self.reachable_sources(a, out);
            }
        }
        // A method's receiver rides the row position after its declared parameters.
        if let HirExpr::Index(receiver, _, _) = self.hir.get(callee) {
            if self.sigs.hands_back_itself_at(&func, args.len()) {
                self.reachable_sources(receiver, out);
            }
        }
        // A result that may be a binding from an outer scope names that binding a second time, so
        // it reaches whatever the caller holds under the same name.
        for &name in self.sigs.returns_free(&func) {
            if let Some(i) = self.frame_index_of(name).filter(|&i| self.holds_mutable(i)) {
                out.push((i, *callee));
            }
        }
    }

    /// Marks every mutable-value holder a moved value reaches as moved out, so a later read is
    /// use-after-move. Returns those holders, so a caller that also records provenance reuses the walk.
    pub(super) fn move_source(&mut self, node: &HirId<HirExpr>) -> Vec<usize> {
        self.move_source_because(node, MoveCause::Value)
    }

    /// Takes the element writer slot for a receiver a mutating call writes.
    pub(super) fn claim_receiver_write(&mut self, receiver: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.local_of(receiver) else { return self.check_path_write(receiver) };
        self.claim_element_write(i, receiver)
    }

    /// `a[0][0] = 1` reaches its element through a path, so no binding denotes that element and
    /// there is nothing to record as its holder. Such a write takes no writer slot.
    pub(super) fn check_path_write(&mut self, target: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.record_guard(target, Guard::WriteThroughPath);
        Ok(())
    }

    /// The mutable-value holders a value reaches.
    pub(super) fn source_indices(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(value, &mut sources);
        sources.into_iter().map(|(i, _)| i).collect()
    }

    /// The source slots feeding a binding's value: the mutable-value holders it reaches, plus the
    /// enclosing sources a captured writing closure moves.
    pub(super) fn provenance_of(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut out = self.source_indices(value);
        out.extend(self.captured_sources(value));
        out
    }

    /// The enclosing locals a closure moves by writing to them. Only mutable-value holders count.
    pub(super) fn captured_sources(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let HirExpr::Literal(HirLiteral::Lambda(_)) = self.hir.get(value) else { return Vec::new() };
        let Some(writes) = self.sigs.lambda_writes.get(value) else { return Vec::new() };
        writes.iter().filter_map(|name| {
            let i = self.frame_index_of(*name)?;
            self.holds_mutable(i).then_some(i)
        }).collect()
    }

    /// Stores a value into a container. The value persists there, so a `no persist` value
    /// is rejected, a borrowed value is rejected since it cannot outlive its lender, and a mutable
    /// value moves in as the container becomes its owner.
    pub(super) fn store_into_container(&mut self, flow: &Flow, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.reject_outliving(flow, Site::Container, expr)?;
        if self.arg_is_borrowed(expr) {
            return Err(self.error_help("cannot persist a borrowed value".to_string(), expr,
                "take it by `*mut` to own it, then it may be persisted"));
        }
        // A parameter may hold a mutable borrowed from the caller, which no signature records. The
        // runtime settles it, and the check rides the value so the error carets the value.
        if self.holds_unproven_borrow(expr) {
            self.record_guard(expr, Guard::Unborrowed);
        }
        self.check_stored_element(expr)?;
        self.move_source(expr);
        Ok(())
    }

    /// Hands one aggregate's element to a second one. The element keeps its place in the first,
    /// which may still read it, and the receiver takes the writer slot so only it may write.
    pub(super) fn check_stored_element(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.extraction_of(expr).is_some() {
            self.record_guard(expr, Guard::StoreIntoContainer);
        }
        Ok(())
    }

    /// The frame slot a node reads, when it reads one at all.
    pub(super) fn local_of(&self, node: &HirId<HirExpr>) -> Option<usize> {
        match self.place_of(node)? {
            Place { base: Base::Local(i), steps } if steps.is_empty() => Some(i),
            _ => None,
        }
    }

    pub(super) fn mutable_in_immutable_error(&self, node: &HirId<HirExpr>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(
            "cannot store a mutable value in an immutable container".to_string(),
            self.hir.pos(node).clone())
            .with_label("this value is mutable")
            .with_help("freeze the value, or mark the container `mut`"))
    }

    /// A closure outlives the frame it captures from, so holding a `no persist` binding in one
    /// persists it exactly like a field or container would.
    pub(super) fn reject_capture_escape(&self, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.enclosing_index(name) else { return Ok(()) };
        let owed = self.locals[i].owed.clone();
        self.reject_outliving(&Flow::Bad { obligations: owed, definite: false, container: false }, Site::Capture, node)
    }

    /// Checks an opaque call. Each mutable argument is either consumed or, when it is a borrow
    /// the caller cannot give away, guarded by a runtime assertion that the callee borrows it.
    pub(super) fn check_opaque_call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], arg_types: &[Typed]) -> Result<Typed, anyhow::Error> {
        let mut survive = Vec::new();
        for (i, typed) in arg_types.iter().enumerate() {
            // A `no persist` value must survive: the opaque callee may not persist it.
            if self.arg_owes_no_persist(&typed.flow) {
                survive.push(i as u8);
                continue;
            }
            if typed.mutability != Mutability::Mutable {
                continue;
            }
            if self.arg_is_borrowed(&args[i]) {
                survive.push(i as u8);
                continue;
            }
            // An owned mutable may be consumed by the callee or merely borrowed, and this pass
            // cannot tell which. Treat the binding as dead for now. If it is never read again both
            // outcomes are fine, and if it is read the reader demands the runtime prove a borrow.
            self.move_source_because(&args[i], MoveCause::Opaque(*callee, i as u8));
        }
        if !survive.is_empty() {
            self.record_survive_barrier(callee, survive.clone());
            // Mark the borrowed args so the runtime panics if an opaque callee tries to persist them.
            self.record_borrow_marks(callee, survive);
        }
        self.indirect_call(callee)
    }

    /// Matches each argument's mutability against its parameter marker.
    pub(super) fn check_arg_mutability(&self, callee: &HirId<HirExpr>, callee_fn: HirId<HirStmt>, markers: &[Capability], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &marker) in markers.iter().enumerate() {
            let Some(typed) = arg_types.get(i) else { break };
            if marker.is_mut() {
                if typed.mutability == Mutability::Immutable {
                    return Err(self.needs_mut_error(callee, &args[i]));
                }
                // A borrow cannot be given away, so it may not feed a consuming parameter.
                if marker.is_move() && self.arg_is_borrowed(&args[i]) {
                    return Err(self.consumes_borrow_error(callee, &args[i]));
                }
            } else if typed.mutability == Mutability::Mutable {
                // A read-only helper borrows the mutable, so it is admitted only where the callee
                // does not persist it. A callee that mutates it is rejected in its own body, since
                // an unmarked parameter is read-only.
                if self.sigs.param_escapes_at(&callee_fn, i) {
                    return Err(self.keeps_argument_error(callee, &args[i]));
                }
            }
        }
        Ok(())
    }

    /// Flows a stored argument's obligations onto the receiver container, so pushing a pending
    /// value makes the array carry that obligation.
    pub(super) fn preserve_into_receiver(&mut self, receiver: &HirId<HirExpr>, arg_types: &[Typed]) {
        let mut obligations = HashSet::new();
        for typed in arg_types {
            if let Flow::Bad { obligations: o, .. } = &typed.flow {
                obligations.extend(o.iter().copied());
            }
        }
        if obligations.is_empty() {
            return;
        }
        let HirExpr::Identifier(name) = self.hir.get(receiver) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        self.locals[i].owed.extend(obligations);
        self.locals[i].container = true;
    }

    /// Moves each argument passed to a `*mut` parameter. A plain `mut` parameter borrows, so
    /// it leaves the argument live.
    pub(super) fn consume_move_args(&mut self, markers: &[Capability], args: &[HirId<HirExpr>]) {
        for (i, &marker) in markers.iter().enumerate() {
            if matches!(marker, Capability::MoveMut) {
                if let Some(arg) = args.get(i) { self.move_source(arg); }
            }
        }
    }

    /// The declaration span of the current function's parameter named by `arg`.
    pub(super) fn borrowed_param_pos(&self, arg: &HirId<HirExpr>) -> Option<&SourcePosition> {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return None };
        self.fn_ctx.params.iter().find(|(sym, _)| sym == name).map(|(_, pos)| pos)
    }

    pub(super) fn arg_is_borrowed(&self, arg: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return false };
        self.frame_index_of(*name).is_some_and(|i| self.locals[i].alias.borrowed)
    }

    /// The error for calling a mutating method on a value the caller may not mutate.
    pub(super) fn immutable_receiver_error(&self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, reason: &str) -> anyhow::Error {
        // A receiver that only reads a captured value is immutable for a reason worth naming.
        if let Some(err) = self.reader_write_error_of(receiver) {
            return err;
        }
        let subject = self.receiver_subject(receiver);
        let method = self.callee_name(callee);
        let help = match self.names_readonly_param(receiver) {
            true => format!("{method} {reason}; declare the parameter `mut` to let {subject} be mutated"),
            false => format!("{method} {reason}; construct the value with `mut` to call it"),
        };
        // The callee span contains the receiver span, so a second caret would sit inside the first.
        anyhow!("{}", Diagnostic::new("expected mutable receiver".to_string(), self.hir.pos(receiver).clone())
            .with_label(format!("{subject} is immutable"))
            .with_help(help))
    }

    /// The error for handing a borrowed value to a parameter that consumes it.
    pub(super) fn consumes_borrow_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let (a, c) = (self.arg_name(arg), self.callee_name(callee));
        let mut diag = Diagnostic::new(format!("cannot move borrowed value {a}"), self.hir.pos(arg).clone())
            .with_label(format!("{a} is moved here"));
        if let Some(param_pos) = self.borrowed_param_pos(arg) {
            let fname = self.fn_ctx.name.map_or("this function".to_string(), |s| format!("`{}`", self.hir.text(s)));
            diag = diag.with_context_span(param_pos.clone(), format!("{fname} only borrows {a} here; it does not own it"));
        }
        diag = diag
            .with_context_span(self.hir.pos(callee).clone(), format!("{c} consumes {a}"))
            .with_help(format!("take ownership of {a} with `*mut` to move it into {c}"));
        anyhow!("{}", diag)
    }

    /// The error for passing a mutable value to a parameter that persists it.
    pub(super) fn keeps_argument_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let subject = self.quoted_subject(arg);
        let c = self.callee_name(callee);
        self.error_ctx(
            format!("{subject} is mutable and this function keeps its argument; freeze or copy it, or take it by '*mut'"),
            self.hir.pos(arg), format!("{subject} is mutable"),
            self.hir.pos(callee), format!("{c} keeps its argument"))
    }

    /// A capability-mismatch error: the parameter wants a `want` argument but got a `got` one.
    pub(super) fn arg_cap_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>, want: &str, got: &str) -> anyhow::Error {
        let (a, c) = (self.arg_name(arg), self.callee_name(callee));
        self.error_ctx(format!("expected {want} argument"), self.hir.pos(arg), format!("{a} is {got}"),
            self.hir.pos(callee), format!("{c} expects {a} to be {want}"))
    }

    /// The error for passing an immutable value to a parameter that requires a mutable one.
    pub(super) fn needs_mut_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        self.arg_cap_error(callee, arg, "mutable", "immutable")
    }

    /// The compile error for mutating an immutable value. A read-only parameter points at the
    /// `mut` fix. Any other immutable value reports that it cannot be mutated.
    pub(super) fn immutable_mutation_error(&self, target: &HirId<HirExpr>, i: usize) -> anyhow::Error {
        // A binding that only reads a value a closure writes is immutable for that reason, so it
        // gets the reason rather than the bare fact.
        if let Some(err) = self.reader_write_error(i, target) {
            return err;
        }
        let name = self.hir.text(self.locals[i].name);
        if self.locals[i].param {
            return self.error_help(
                format!("cannot mutate `{name}`, a read-only parameter"), target,
                format!("declare the parameter `mut` to let `{name}` be mutated"));
        }
        self.error_labeled("cannot mutate an immutable value".to_string(), target, format!("`{name}` is immutable"))
    }

    /// The error for writing a field through a receiver the method did not declare mutable.
    pub(super) fn readonly_receiver_error(&self, type_name: Symbol, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field(type_name, field);
        let method = self.fn_ctx.name.map_or("this method".to_string(), |s| format!("`{}`", self.hir.text(s)));
        self.error_help(format!("cannot assign `{name}` through a read-only receiver"), lhs,
            format!("declare {method}'s receiver `this: mut` to let it mutate the instance"))
    }

    /// The declaration a field needs to become mutable, e.g. `pub mut value` or `mut value`,
    /// keeping the field's current visibility.
    pub(super) fn mut_decl_hint(&self, type_name: Symbol, field: Symbol) -> String {
        let visibility = self.layout_of(type_name).map_or("", |layout| {
            if layout.is_public(field) { "pub " } else if layout.is_inner(field) { "inner " } else { "" }
        });
        format!("{visibility}mut {}", self.hir.text(field))
    }

    /// Records that a local receiving a stored value takes on that value's sources, so the value
    /// flows back to them when the local dies. A non-local receiver is left alone; the value is
    /// still moved either way.
    pub(super) fn attach_field_provenance(&mut self, target: &HirId<HirExpr>, rhs: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        let sources = self.source_indices(rhs);
        self.locals[i].alias.provenance.extend(sources);
    }

    pub(super) fn immutable_field_error(&self, type_name: Symbol, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field(type_name, field);
        self.error_help(format!("Cannot assign immutable field `{name}`"), lhs,
            format!("you can make `{name}` mutable by declaring it as `{};`", self.mut_decl_hint(type_name, field)))
    }
}
