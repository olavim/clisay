use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::frontend::lex::Diagnostic;

use crate::core::gc::Gc;
use crate::middle::hir::TypeId;
use crate::middle::obligations::{ObligationRule, Obligations};
use crate::middle::ir::{Inst, Ir, Label, SourceRole, NULL_WITNESS_ID, SCRIPT_SLOT_WITNESS_SET_POOL_ID};
use crate::middle::bind::{AnchorParam, Bindings, FnKind, Place};
use crate::middle::check::Barriers;
use crate::middle::signatures::CallableId;
use crate::middle::signatures::Signatures;
use crate::middle::hir::Hir;
use crate::middle::hir::HirExpr;
use crate::middle::hir::HirFnDecl;
use crate::middle::hir::HirId;
use crate::middle::hir::HirStmt;
use crate::middle::hir::HirMatcher;

mod expressions;
mod statements;
pub mod matching;
mod functions;
mod types;
mod anchor_roots;

#[derive(Clone, Copy, PartialEq)]
enum TryCatchPosition {
    Try,
    Catch,
    Finally
}

#[derive(Clone, Copy)]
struct DeferDecl {
    stmt: HirId<HirStmt>,
    body: HirId<HirExpr>,
    handler: Label,
}

struct QueuedDefer {
    decl: DeferDecl,
    /// The handlers a throw from the defer's body reaches.
    outer_handlers: Vec<(Label, u16)>,
    handle_binder_slots: Vec<(u8, u8)>,
}

#[derive(Clone, Copy)]
struct TryFrame {
    position: TryCatchPosition,
    finally: Option<HirId<HirExpr>>,
    stmt: HirId<HirStmt>,
    /// How many defers were pending when the `try` began.
    pending_defer_count: usize,
}

impl TryFrame {
    fn has_handler(&self) -> bool {
        match self.position {
            TryCatchPosition::Try => true,
            TryCatchPosition::Catch => self.finally.is_some(),
            TryCatchPosition::Finally => false,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) struct ReturnContract {
    pub witness_set_pool_id: u16,
    pub allows_void: bool,
}

pub(super) struct CompiledFn {
    pub kind: FnKind,
    pub callable: CallableId,
    pub contract: Option<ReturnContract>,
}

#[derive(Clone, Copy)]
pub(super) enum Returning<'a> {
    Nothing,
    Value(&'a HirId<HirExpr>),
}

/// Lowers a resolved HIR to IR.
pub struct Compiler<'a> {
    ir: Ir,
    hir: &'a Hir,
    gc: &'a mut Gc,
    bindings: &'a Bindings,
    barriers: &'a Barriers,
    sigs: &'a Signatures,
    compiled_fns: Vec<CompiledFn>,
    try_frames: Vec<TryFrame>,
    /// The id of each registered object witness.
    witness_ids: FnvHashMap<TypeId, u16>,
    drop_guards: bool,
    force_checks: bool,
    /// The body of the function being compiled.
    current_body: Option<HirId<HirExpr>>,
    body_rebinds: FnvHashMap<HirId<HirExpr>, anchor_roots::BodyRebinds>,
    /// Each live `?? e =>` binder.
    handle_binder_slots: Vec<(u8, u8)>,
    /// The keys a write saved for its walk, each with the frame slot it was saved to.
    saved_write_path_keys: FnvHashMap<HirId<HirExpr>, u8>,
    /// The exact height of the frame's stack. None after a jump, return, or throw.
    frame_stack_height: Option<usize>,
    /// The stack height at each label.
    label_frame_stack_heights: Vec<Option<usize>>,
    next_call_discards_result: bool,
    defers: Vec<DeferDecl>,
    defer_slot: Option<u8>,
    handlers: Vec<(Label, u16)>,
    queued_defers: Vec<QueuedDefer>,
    anchor_params: &'a [AnchorParam],
    current_slot_witness_set_pool_id: u16,
    slot_witness_set_pool_ids: Vec<u16>,
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub(super) fn next_slot<T: 'static>(&self, at: &HirId<T>) -> Result<u8, anyhow::Error> {
        self.operand_count(self.known_frame_stack_height(), "a frame", "slots", at)
    }

    pub(super) fn operand_count<T: 'static>(&self, count: usize, subject: &str, unit: &str, at: &HirId<T>) -> Result<u8, anyhow::Error> {
        u8::try_from(count).map_err(|_| self.error(format!("{subject} may have at most {} {unit}", u8::MAX), at))
    }

    pub fn compile<'b>(hir: &'b Hir, gc: &'b mut Gc, bindings: &'b Bindings, barriers: &'b Barriers, sigs: &'b Signatures, drop_guards: bool, force_checks: bool) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            anchor_params: &[],
            drop_guards,
            force_checks,
            current_body: None,
            body_rebinds: FnvHashMap::default(),
            ir: Ir::new(),
            hir,
            gc,
            bindings,
            barriers,
            sigs,
            compiled_fns: Vec::new(),
            try_frames: Vec::new(),
            witness_ids: FnvHashMap::default(),
            frame_stack_height: Some(0),
            label_frame_stack_heights: Vec::new(),
            next_call_discards_result: false,
            saved_write_path_keys: FnvHashMap::default(),
            defers: Vec::new(),
            defer_slot: None,
            handlers: Vec::new(),
            queued_defers: Vec::new(),
            handle_binder_slots: Vec::new(),
            current_slot_witness_set_pool_id: SCRIPT_SLOT_WITNESS_SET_POOL_ID,
            slot_witness_set_pool_ids: Vec::new(),
        };

        compiler.assign_witness_ids();
        let stmt_id = compiler.hir.get_root();
        let (_, script) = compiler.with_frame(|c| {
            if let Some(body) = c.hir.script_body() {
                c.open_frame_temp_slots(&body);
            }
            c.statement(&stmt_id)?;
            c.emit(Inst::PushNull, &stmt_id);
            c.emit(Inst::Halt, &stmt_id);
            Ok(())
        })?;
        debug_assert_eq!(script, SCRIPT_SLOT_WITNESS_SET_POOL_ID, "the script's frame is the first one opened");
        Ok(compiler.ir)
    }

    fn with_frame<R>(&mut self, body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<(R, u16), anyhow::Error> {
        let caller_height = self.frame_stack_height.take();
        // A binder names a slot of the frame that made it, so a nested frame starts with none.
        let caller_binders = std::mem::take(&mut self.handle_binder_slots);
        let caller_defers = std::mem::take(&mut self.defers);
        let caller_try_frames = std::mem::take(&mut self.try_frames);
        let caller_handlers = self.replace_handlers(Vec::new());
        let caller_queued_defers = std::mem::take(&mut self.queued_defers);
        let caller_params = std::mem::take(&mut self.anchor_params);
        let caller_defer_slot = self.defer_slot.take();
        let caller_body = self.current_body.take();
        let caller_table = self.current_slot_witness_set_pool_id;
        let caller_slot_witness_sets = std::mem::take(&mut self.slot_witness_set_pool_ids);
        self.current_slot_witness_set_pool_id = self.ir.push_slot_witness_set_table()?;
        let table = self.current_slot_witness_set_pool_id;

        // The frame's own code ends in a return or a halt, so its defer handlers follow it.
        let result = body(self).and_then(|r| self.emit_queued_defers().map(|_| r));

        self.frame_stack_height = caller_height;
        self.handle_binder_slots = caller_binders;
        self.anchor_params = caller_params;
        self.defers = caller_defers;
        self.try_frames = caller_try_frames;
        self.replace_handlers(caller_handlers);
        self.queued_defers = caller_queued_defers;
        self.defer_slot = caller_defer_slot;
        self.current_body = caller_body;
        self.current_slot_witness_set_pool_id = caller_table;
        self.slot_witness_set_pool_ids = caller_slot_witness_sets;
        Ok((result?, table))
    }

    fn place(&self, node: &HirId<HirExpr>) -> Place {
        match self.bindings.place(node) {
            Place::Local(slot) => Place::Local(self.real_slot(slot)),
            other => other,
        }
    }

    fn real_slot(&self, slot: u8) -> u8 {
        self.handle_binder_slots.iter()
            .find_map(|&(binder_slot, operand_slot)| (binder_slot == slot).then_some(operand_slot))
            .unwrap_or(slot)
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &HirId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node_id).clone()))
    }

    fn assign_witness_ids(&mut self) {
        for &decl in self.barriers.witness_decls() {
            let next = self.witness_ids.len() as u16 + 1;
            self.witness_ids.entry(decl).or_insert(next);
        }
        // The VM builds some types itself, so it needs the numbering to mark them the same way.
        let ids = self.witness_ids.iter().map(|(decl, id)| (*decl, *id)).collect();
        self.ir.set_witness_ids(ids);
    }

    pub(super) fn type_test_id<T: 'static>(&self, matcher: &HirId<HirMatcher>, node: &HirId<T>) -> Result<TypeId, anyhow::Error> {
        let Some(decl) = self.bindings.type_ref(matcher) else {
            compiler_error!(self, node, "a type test names no declaration");
        };
        match self.hir.get(&decl) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Ok(decl.id),
            _ => compiler_error!(self, node, "a type test names no declaration"),
        }
    }

    pub(super) fn witness_set_pool_id(&mut self, owed: &Obligations) -> Result<u16, anyhow::Error> {
        let witnesses: Vec<TypeId> = self.sigs.object_witnesses()
            .filter(|(ob, _)| owed.contains(ob))
            .map(|(_, id)| id)
            .collect();
        let witness_set = self.accepted_witness_set(&witnesses, owed.contains(&self.sigs.opt));
        self.ir.intern_witness_set(witness_set)
    }

    pub(super) fn accepted_witness_set(&self, decls: &[TypeId], null_allowed: bool) -> Box<[u16]> {
        let mut ids: Vec<u16> = self.witness_id_set(decls).into_vec();
        if null_allowed {
            ids.insert(0, NULL_WITNESS_ID);
        }
        ids.into_boxed_slice()
    }

    pub(super) fn witnesses_rule(&self, rule: ObligationRule, provided: &[TypeId]) -> bool {
        self.sigs.witnesses.iter()
            .any(|(ob, w)| w.type_or_trait().is_some_and(|id| provided.contains(&id)) && rule.holds(&self.sigs.obligation_rules_of(*ob)))
    }

    pub(super) fn witness_id_set(&self, decls: &[TypeId]) -> Box<[u16]> {
        let mut ids: Vec<u16> = decls.iter().filter_map(|decl| self.witness_ids.get(decl)).copied().collect();
        ids.sort_unstable();
        ids.dedup();
        ids.into_boxed_slice()
    }

    fn emit<T: 'static>(&mut self, inst: Inst, node_id: &HirId<T>) {
        self.track_frame_stack_height(&inst, node_id);
        let pos = self.hir.pos(node_id);
        self.ir.emit(inst, pos);
    }

    fn track_frame_stack_height<T: 'static>(&mut self, inst: &Inst, node: &HirId<T>) {
        let Some(height) = self.frame_stack_height else { return };
        let effect = self.ir.stack_effect(inst);
        if let Some((label, pops, pushes)) = effect.jump {
            self.arrive_at(label, height - pops + pushes, node);
        }
        self.frame_stack_height = effect.next.map(|(pops, pushes)| {
            if pops > height {
                self.frame_stack_height_mismatch(format!("{inst:?} pops {pops} at height {height}"), node);
            }
            height.saturating_sub(pops) + pushes
        });
    }

    /// Records the stack height a jump arrives at `label` with.
    fn arrive_at<T: 'static>(&mut self, label: Label, height: usize, node: &HirId<T>) {
        match self.frame_stack_height_at_label(label) {
            Some(known) if known != height => self.frame_stack_height_mismatch(format!("{label:?} reached at {height} and at {known}"), node),
            Some(_) => {},
            None => self.record_frame_stack_height_at_label(label, height),
        }
    }

    fn frame_stack_height_at_label(&self, label: Label) -> Option<usize> {
        self.label_frame_stack_heights.get(label.index()).copied().flatten()
    }

    fn record_frame_stack_height_at_label(&mut self, label: Label, height: usize) {
        if self.label_frame_stack_heights.len() <= label.index() {
            self.label_frame_stack_heights.resize(label.index() + 1, None);
        }
        self.label_frame_stack_heights[label.index()] = Some(height);
    }

    pub(super) fn bind_label(&mut self, label: Label) {
        match (self.frame_stack_height, self.frame_stack_height_at_label(label)) {
            (Some(here), Some(known)) if here != known => {
                let node = self.hir.get_root();
                self.frame_stack_height_mismatch(format!("{label:?} bound at {here}, reached at {known}"), &node);
            },
            (Some(here), None) => self.record_frame_stack_height_at_label(label, here),
            (None, known) => self.frame_stack_height = known,
            _ => {},
        }
        self.ir.bind(label);
    }

    /// Checks the height where the code knows what it has to be. Code right after a jump, return
    /// or throw starts at that height.
    pub(super) fn expect_frame_stack_height<T: 'static>(&mut self, expected: usize, what: &str, node: &HirId<T>) {
        match self.frame_stack_height {
            Some(height) if height != expected => self.frame_stack_height_mismatch(format!("{what}: the stack is {height} high, not {expected}"), node),
            _ => {},
        }
        self.frame_stack_height = Some(expected);
    }

    pub(super) fn push_handler<T: 'static>(&mut self, handler: Label, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let frame_stack_height = self.known_frame_stack_height();
        // A throw reaches the handler with the frame as it is here and the thrown value on top.
        self.arrive_at(handler, frame_stack_height + 1, node);
        let frame_stack_height = u16::try_from(frame_stack_height).map_err(|_| self.error("a frame's stack is too deep".to_string(), node))?;
        self.handlers.push((handler, frame_stack_height));
        self.ir.set_throw_target(self.handlers.last().copied());
        Ok(())
    }

    pub(super) fn pop_handler(&mut self) {
        self.handlers.pop();
        self.ir.set_throw_target(self.handlers.last().copied());
    }

    pub(super) fn replace_handlers(&mut self, handlers: Vec<(Label, u16)>) -> Vec<(Label, u16)> {
        let replaced = std::mem::replace(&mut self.handlers, handlers);
        self.ir.set_throw_target(self.handlers.last().copied());
        replaced
    }

    pub(super) fn known_frame_stack_height(&self) -> usize {
        self.frame_stack_height.expect("the frame stack height is known")
    }

    fn frame_stack_height_mismatch<T: 'static>(&self, what: String, node: &HirId<T>) {
        if cfg!(debug_assertions) {
            let pos = self.hir.pos(node);
            panic!("{}:{}: {what}", pos.source.name, pos.line);
        }
    }

    fn emit_store_inst(&mut self, inst: Inst, node: &HirId<HirExpr>, value: &HirId<HirExpr>) {
        let at = self.ir.next_index();
        self.emit(inst, node);
        let role = match self.hir.get(value) {
            HirExpr::Identifier(_) => SourceRole::StoredName,
            _ => SourceRole::StoredValue,
        };
        let pos = self.hir.pos(value).clone();
        self.ir.map_source(at, role, pos);
    }

    fn emit_conditional_jump<T: 'static>(&mut self, cond: &HirId<HirExpr>, node_id: &HirId<T>) -> Result<Label, anyhow::Error> {
        let target = self.ir.new_label();
        self.expression(cond)?;
        self.emit(Inst::JumpIfFalse(target), node_id);
        Ok(target)
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) -> Result<(), anyhow::Error> {
        let count = self.bindings.cleanup(node_id);
        if count == 0 {
            return Ok(());
        }
        let height = self.bindings.exit_frame_stack_height_at(node_id);
        let first_dead = height.saturating_sub(count as u8);
        self.ir.end_slot_witness_sets_from(self.current_slot_witness_set_pool_id, first_dead);
        self.slot_witness_set_pool_ids.truncate(first_dead as usize);
        self.emit(Inst::PopScope(count, height), node_id);
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}
