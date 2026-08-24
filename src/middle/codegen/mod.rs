use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::frontend::lex::Diagnostic;

use crate::core::gc::Gc;
use crate::middle::hir::TypeId;
use crate::middle::obligations::Obligations;
use crate::middle::ir::{Inst, Ir, Label, SourceRole, NULL_WITNESS_ID};
use crate::middle::bind::{Bindings, FnKind, Place};
use crate::middle::check::Barriers;
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

#[derive(Clone, Copy)]
enum TryCatchPosition {
    Try,
    Catch,
    Finally
}

#[derive(Clone)]
struct TryFrame {
    position: TryCatchPosition,
    finally: Option<HirId<HirExpr>>
}

#[derive(Clone, Copy, PartialEq)]
pub(super) enum PathRoot {
    Local(u8),
    Upvalue(u8),
    Unnamed,
}

#[derive(Clone, Copy)]
pub(super) enum WriteOwnershipHolderPlace {
    Local(u8),
    Upvalue(u8),
    Stack(u8),
}

/// Lowers a resolved HIR to IR.
pub struct Compiler<'a> {
    ir: Ir,
    hir: &'a Hir,
    gc: &'a mut Gc,
    bindings: &'a Bindings,
    barriers: &'a Barriers,
    sigs: &'a Signatures,
    fn_kinds: Vec<FnKind>,
    try_frames: Vec<TryFrame>,
    /// The id of each registered object witness.
    witness_ids: FnvHashMap<TypeId, u16>,
    /// The slot that will hold the container being built.
    receiving_slot: Option<WriteOwnershipHolderPlace>,
    /// The root node of a path whose write barrier compares against it.
    stash_root: Option<HirId<HirExpr>>,
    drop_guards: bool,
    /// Each live `?? e =>` binder.
    handle_binder_slots: Vec<(u8, u8)>,
    frame_slot_count: usize,
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub(super) fn operand_count<T: 'static>(&self, count: usize, subject: &str, unit: &str, at: &HirId<T>) -> Result<u8, anyhow::Error> {
        u8::try_from(count).map_err(|_| self.error(format!("{subject} may have at most {} {unit}", u8::MAX), at))
    }

    pub fn compile<'b>(hir: &'b Hir, gc: &'b mut Gc, bindings: &'b Bindings, barriers: &'b Barriers, sigs: &'b Signatures, drop_guards: bool) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            receiving_slot: None,
            stash_root: None,
            drop_guards,
            ir: Ir::new(),
            hir,
            gc,
            bindings,
            barriers,
            sigs,
            fn_kinds: Vec::new(),
            try_frames: Vec::new(),
            witness_ids: FnvHashMap::default(),
            frame_slot_count: 0,
            handle_binder_slots: Vec::new(),
        };

        compiler.assign_witness_ids();
        let stmt_id = compiler.hir.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
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

    /// What a destination accepts, given the obligations its clause names.
    pub(super) fn accepts_index(&mut self, owed: &Obligations, nullable: bool) -> Result<u16, anyhow::Error> {
        let witnesses: Vec<TypeId> = self.sigs.object_witnesses()
            .filter(|(ob, _)| owed.contains(ob) || !self.sigs.obligation_rules_of(*ob).before_drop)
            .map(|(_, id)| id)
            .collect();
        let accepts = self.accepted_witness_set(&witnesses, nullable || owed.contains(&self.sigs.opt));
        self.ir.add_witness_allow(accepts)
    }

    pub(super) fn accepted_witness_set(&self, decls: &[TypeId], null_allowed: bool) -> Box<[u16]> {
        let mut ids: Vec<u16> = self.witness_id_set(decls).into_vec();
        if null_allowed {
            ids.insert(0, NULL_WITNESS_ID);
        }
        ids.into_boxed_slice()
    }

    /// The ids these declarations are numbered as. `ObjType::witness_ids` lists what a type
    /// provides, and null is not among them.
    pub(super) fn witness_id_set(&self, decls: &[TypeId]) -> Box<[u16]> {
        let mut ids: Vec<u16> = decls.iter().filter_map(|decl| self.witness_ids.get(decl)).copied().collect();
        ids.sort_unstable();
        ids.dedup();
        ids.into_boxed_slice()
    }

    fn finish(mut self) -> Ir {
        self.emit(Inst::PushNull, &self.hir.get_root());
        self.emit(Inst::Halt, &self.hir.get_root());
        self.ir
    }

    fn emit<T: 'static>(&mut self, inst: Inst, node_id: &HirId<T>) {
        match inst {
            Inst::PushNull => self.frame_slot_count += 1,
            Inst::Pop
                | Inst::JumpIfFalseOrPop(_)
                | Inst::JumpIfTrueOrPop(_)
                | Inst::JumpIfNotNullOrPop(_) => self.frame_slot_count = self.frame_slot_count.saturating_sub(1),
            _ => {},
        }
        let pos = self.hir.pos(node_id);
        self.ir.emit(inst, pos);
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
        let slot_count = self.bindings.exit_frame_slot_count_at(node_id);
        self.emit(Inst::PopScope(count, slot_count), node_id);
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}
