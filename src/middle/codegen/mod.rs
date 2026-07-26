use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::frontend::lex::Diagnostic;

use crate::core::gc::Gc;
use crate::core::value::Value;
use crate::core::objects::ObjType;
use crate::core::objects::ObjString;
use crate::middle::ir::{Inst, Ir, Label};
use crate::middle::bind::{Bindings, Cleanup, FnKind};
use crate::middle::check::Barriers;
use crate::middle::signatures::Signatures;
use crate::middle::hir::Hir;
use crate::middle::hir::HirExpr;
use crate::middle::hir::HirFnDecl;
use crate::middle::hir::HirId;
use crate::middle::hir::HirStmt;

mod expressions;
mod statements;
mod matching;
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
    types: FnvHashMap<*mut ObjString, *mut ObjType>
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(hir: &'b Hir, gc: &'b mut Gc, bindings: &'b Bindings, barriers: &'b Barriers, sigs: &'b Signatures) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            ir: Ir::new(),
            hir,
            gc,
            bindings,
            barriers,
            sigs,
            fn_kinds: Vec::new(),
            try_frames: Vec::new(),
            types: FnvHashMap::default()
        };

        compiler.record_witness_registry();
        let stmt_id = compiler.hir.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &HirId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node_id).clone()))
    }

    /// Interns the program's object witness names into the `Ir`, so the VM can recognize a
    /// crossing value as a witness at a boundary barrier.
    fn record_witness_registry(&mut self) {
        let names = self.barriers.witness_names().iter()
            .map(|name| Value::from(self.gc.intern(self.hir.text(*name))))
            .collect();
        self.ir.set_witness_names(names);
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
