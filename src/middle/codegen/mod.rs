use anyhow::anyhow;
use fnv::FnvHashMap;

use crate::core::gc::Gc;
use crate::core::objects::ObjClass;
use crate::core::objects::ObjString;
use crate::middle::ir::{Inst, Ir, Label};
use crate::middle::resolve::{Bindings, Cleanup, FnKind};
use crate::ast::AstId;
use crate::ast::Expr;
use crate::ast::FnDecl;
use crate::ast::Stmt;
use crate::ast::Ast;

mod expressions;
mod statements;
mod functions;
mod classes;

#[derive(Clone, Copy)]
enum TryCatchPosition {
    Try,
    Catch,
    Finally
}

#[derive(Clone)]
struct TryFrame {
    position: TryCatchPosition,
    finally: Option<AstId<Expr>>
}

/// Lowers a resolved AST to IR.
pub struct Compiler<'a> {
    ir: Ir,
    ast: &'a Ast,
    gc: &'a mut Gc,
    bindings: &'a Bindings,
    /// The kind of each enclosing function, for initializer return handling.
    fn_kinds: Vec<FnKind>,
    try_frames: Vec<TryFrame>,
    classes: FnvHashMap<*mut ObjString, *mut ObjClass>
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(ast: &'b Ast, gc: &'b mut Gc, bindings: &'b Bindings) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            ir: Ir::new(),
            ast,
            gc,
            bindings,
            fn_kinds: Vec::new(),
            try_frames: Vec::new(),
            classes: FnvHashMap::default()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &AstId<T>) -> anyhow::Error {
        let pos = self.ast.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn finish(mut self) -> Ir {
        self.emit(Inst::PushNull, &self.ast.get_root());
        self.emit(Inst::Return, &self.ast.get_root());
        self.ir
    }

    fn emit<T: 'static>(&mut self, inst: Inst, node_id: &AstId<T>) {
        let pos = self.ast.pos(node_id);
        self.ir.emit(inst, pos);
    }

    /// Emits a conditional branch to a fresh (unbound) label and returns it.
    /// The caller should bind the label to the jump's destination.
    fn emit_conditional_jump<T: 'static>(&mut self, cond: &AstId<Expr>, node_id: &AstId<T>) -> Result<Label, anyhow::Error> {
        let target = self.ir.new_label();
        self.expression(cond)?;
        self.emit(Inst::JumpIfFalse(target), node_id);
        Ok(target)
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &AstId<T>) {
        let cleanups = self.bindings.cleanup(node_id).to_vec();
        for cleanup in cleanups {
            let inst = match cleanup {
                Cleanup::Pop => Inst::Pop,
                Cleanup::CloseUpvalue(slot) => Inst::CloseUpvalue(slot),
            };
            self.emit(inst, node_id);
        }
    }

    fn fn_decl(&self, stmt: &AstId<Stmt>) -> &'a FnDecl {
        let Stmt::Fn(decl) = self.ast.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}
