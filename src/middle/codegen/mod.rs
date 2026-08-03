use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::frontend::lex::Diagnostic;

use crate::core::gc::Gc;
use crate::core::objects::ObjType;
use crate::core::objects::ObjString;
use crate::middle::hir::TypeId;
use crate::middle::ir::{Inst, Ir, Label};
use crate::middle::bind::{Bindings, Cleanup, FnKind};
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
    types: FnvHashMap<*mut ObjString, *mut ObjType>,
    /// The id of each registered object witness, by declaration.
    witness_ids: FnvHashMap<TypeId, u16>,
    /// The slot that will hold the container being built, while its parts are compiled. An element
    /// handed to it takes its writer slot in that slot's name.
    receiving_slot: Option<u8>,
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(hir: &'b Hir, gc: &'b mut Gc, bindings: &'b Bindings, barriers: &'b Barriers, sigs: &'b Signatures) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            receiving_slot: None,
            ir: Ir::new(),
            hir,
            gc,
            bindings,
            barriers,
            sigs,
            fn_kinds: Vec::new(),
            try_frames: Vec::new(),
            types: FnvHashMap::default(),
            witness_ids: FnvHashMap::default()
        };

        compiler.assign_witness_ids();
        let stmt_id = compiler.hir.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
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
        let pos = self.hir.pos(node_id);
        self.ir.emit(inst, pos);
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
        // values to look at, not how many were taken, so a scope with two exits is safe either way.
        if self.barriers.releases_write_ownership(node_id) {
            let held = cleanups.iter().filter(|c| matches!(c, Cleanup::Pop)).count();
            self.emit(Inst::ReleaseWriteOwnership(held as u8), node_id);
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
