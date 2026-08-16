use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::frontend::lex::Diagnostic;

use crate::core::gc::Gc;
use crate::middle::hir::TypeId;
use crate::middle::ir::{Inst, Ir, Label, SourceRole};
use crate::middle::bind::{Bindings, Cleanup, FnKind, Place};
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

/// How a path write names the container it reaches through.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum PathRoot {
    Local(u8),
    Upvalue(u8),
    /// No binding names it, so the barrier compares against the value itself.
    Unnamed,
}

/// Where the container taking an element's write-ownership lives. This is the compile-time half of
/// the runtime's `WriteOwnershipHolder`: a place to read the holder from, rather than the holder.
#[derive(Clone, Copy)]
pub(super) enum WriteOwnershipHolderPlace {
    Local(u8),
    Upvalue(u8),
    /// On the stack, this far below the value being handed over. For a receiver no binding names.
    Stack(u8),
}

/// Lowers a resolved HIR to IR.
pub struct Compiler<'a> {
    ir: Ir,
    hir: &'a Hir,
    gc: &'a mut Gc,
    bindings: &'a Bindings,
    /// Nodes whose value needs a runtime null-barrier, from the check pass. Empty when checking is off.
    barriers: &'a Barriers,
    sigs: &'a Signatures,
    /// The kind of each enclosing function, for factory return handling.
    fn_kinds: Vec<FnKind>,
    try_frames: Vec<TryFrame>,
    /// The id of each registered object witness, by declaration.
    witness_ids: FnvHashMap<TypeId, u16>,
    /// The slot that will hold the container being built, while its parts are compiled. An element
    /// handed to it takes its writer slot in that slot's name.
    receiving_slot: Option<WriteOwnershipHolderPlace>,
    /// The root node of a path whose write barrier compares against it.
    dup_root: Option<HirId<HirExpr>>,
    /// Whether to drop every placed guard.
    floor_only: bool,
    /// Each live `??` binder: the slot bind gave its name, and the slot its operand landed in.
    handle_binder_slots: Vec<(u8, u8)>,
    /// How many slots the frame being emitted holds.
    depth: usize,
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub(super) fn operand_count<T: 'static>(&self, count: usize, subject: &str, unit: &str, at: &HirId<T>) -> Result<u8, anyhow::Error> {
        u8::try_from(count).map_err(|_| self.error(format!("{subject} may have at most {} {unit}", u8::MAX), at))
    }

    pub fn compile<'b>(hir: &'b Hir, gc: &'b mut Gc, bindings: &'b Bindings, barriers: &'b Barriers, sigs: &'b Signatures, floor_only: bool) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            receiving_slot: None,
            dup_root: None,
            floor_only,
            ir: Ir::new(),
            hir,
            gc,
            bindings,
            barriers,
            sigs,
            fn_kinds: Vec::new(),
            try_frames: Vec::new(),
            witness_ids: FnvHashMap::default(),
            depth: 0,
            handle_binder_slots: Vec::new(),
        };

        compiler.assign_witness_ids();
        let stmt_id = compiler.hir.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
    }

    /// Where a name lives. A `??` binder is the one name bind cannot number, because its slot
    /// depends on what the expression around it already pushed.
    fn place(&self, node: &HirId<HirExpr>) -> Place {
        match self.bindings.place(node) {
            Place::Local(slot) => Place::Local(self.real_slot(slot)),
            other => other,
        }
    }

    /// The slot a name really reads. Only a `??` binder's differs from what bind gave it.
    fn real_slot(&self, slot: u8) -> u8 {
        self.handle_binder_slots.iter()
            .find_map(|&(binder_slot, operand_slot)| (binder_slot == slot).then_some(operand_slot))
            .unwrap_or(slot)
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &HirId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node_id).clone()))
    }

    /// Numbers every registered object witness.
    fn assign_witness_ids(&mut self) {
        for &decl in self.barriers.witness_decls() {
            let next = self.witness_ids.len() as u16;
            self.witness_ids.entry(decl).or_insert(next);
        }
        // The VM builds some types itself, so it needs the numbering to mark them the same way.
        let ids = self.witness_ids.iter().map(|(decl, id)| (*decl, *id)).collect();
        self.ir.set_witness_ids(ids);
    }

    /// The runtime identity of the declaration a type test names.
    pub(super) fn type_test_id<T: 'static>(&self, matcher: &HirId<HirMatcher>, node: &HirId<T>) -> Result<TypeId, anyhow::Error> {
        let Some(decl) = self.bindings.type_ref(matcher) else {
            compiler_error!(self, node, "a type test names no declaration");
        };
        match self.hir.get(&decl) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Ok(decl.id),
            _ => compiler_error!(self, node, "a type test names no declaration"),
        }
    }

    /// The witness ids the given declarations are numbered by, sorted.
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
            Inst::PushNull => self.depth += 1,
            Inst::Pop
                | Inst::JumpIfFalseOrPop(_)
                | Inst::JumpIfTrueOrPop(_)
                | Inst::JumpIfNotNullOrPop(_) => self.depth = self.depth.saturating_sub(1),
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

    /// Emits a conditional branch to a fresh (unbound) label and returns it.
    /// The caller should bind the label to the jump's destination.
    fn emit_conditional_jump<T: 'static>(&mut self, cond: &HirId<HirExpr>, node_id: &HirId<T>) -> Result<Label, anyhow::Error> {
        let target = self.ir.new_label();
        self.expression(cond)?;
        self.emit(Inst::JumpIfFalse(target), node_id);
        Ok(target)
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) {
        let cleanups = self.bindings.cleanup(node_id).to_vec();
        // Writer slots go back before the locals holding them are popped. The count is how many
        // values to look at, not how many were taken.
        if self.barriers.releases_write_ownership(node_id) {
            self.emit(Inst::ReleaseWriteOwnership(cleanups.len() as u8), node_id);
        }
        for cleanup in cleanups {
            let inst = match cleanup {
                Cleanup::Pop => Inst::Pop,
                Cleanup::CloseUpvalue(slot) => Inst::CloseUpvalue(slot),
            };
            self.emit(inst, node_id);
        }
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}
