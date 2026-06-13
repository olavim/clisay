//! The intermediate representation: a flat stream of `Inst`s with symbolic
//! jump `Label`s and a constant pool.

use anyhow::bail;

use crate::core::objects::ObjFn;
use crate::core::value::Value;
use crate::frontend::lex::SourcePosition;

/// A symbolic jump target, resolved to a byte offset at assembly time. Created
/// with [`Ir::new_label`] and pinned to a point in the stream with [`Ir::bind`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);

/// A single IR instruction. Operands are typed and jumps reference a [`Label`]
/// rather than a byte offset.
#[derive(Clone, Copy)]
pub enum Inst {
    // Control flow
    Call(u8),
    /// Fused method call `recv.name(args)`: `Invoke(name_const, arg_count)` resolves
    /// the method on the receiver's class and pushes the frame directly, avoiding
    /// the bound-method allocation that `GetIndex` + `Call` would incur.
    Invoke(u8, u8),
    Jump(Label),
    JumpIfFalse(Label),
    /// Short-circuit jumps for `&&`/`||`: peek the top value; if it is falsy
    /// (resp. truthy) jump to the target leaving it on the stack, otherwise pop
    /// it and fall through. The surviving value is the expression's result.
    JumpIfFalseOrPop(Label),
    JumpIfTrueOrPop(Label),
    JumpIfGe(Label),
    JumpIfGt(Label),
    JumpIfLe(Label),
    JumpIfLt(Label),
    JumpIfEq(Label),
    JumpIfNeq(Label),
    JumpIfGeLocalConst(Label, u8, u8),
    JumpIfGtLocalConst(Label, u8, u8),
    JumpIfLeLocalConst(Label, u8, u8),
    JumpIfLtLocalConst(Label, u8, u8),
    Return,
    Throw,
    PushTry(Label),
    PopTry,

    // Stack / constants
    Pop,
    PushConstant(u8),
    PushNull,
    PushTrue,
    PushFalse,
    PushClosure(u8),
    PushClass(u8),

    // Variables and properties
    GetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    SetLocalPop(u8),
    SetLocalAddLocalLocal(u8, u8, u8), // dst = a + b
    GetUpvalue(u8),
    SetUpvalue(u8),
    SetUpvaluePop(u8),
    CloseUpvalue(u8),
    GetIndex,
    SetIndex,
    GetPropertyId(u8),
    SetPropertyId(u8),
    SetPropertyIdPop(u8),
    Array(u8),

    // Arithmetic
    Add,
    AddLocalConst(u8, u8),   // local + const
    Subtract,
    SubLocalConst(u8, u8),   // local - const
    SubConstLocal(u8, u8),   // const - local
    Multiply,
    Divide,
    Negate,
    Not,
    LeftShift,
    RightShift,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,

    // Logical / comparison
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

/// A whole program's worth of IR: the instruction stream (with a source position
/// per instruction) plus the constant pool. Labels are resolved against `code`
/// positions; `assemble` maps those to byte offsets.
pub struct Ir {
    code: Vec<Inst>,
    positions: Vec<SourcePosition>,
    constants: Vec<Value>,
    /// `labels[id]` is the instruction index a label is bound to, or `None`
    /// until [`bind`](Ir::bind) is called.
    labels: Vec<Option<usize>>,
    /// Function entry points: each `ObjFn`'s `ip_start` is patched to its body
    /// label's byte offset at assembly time (offsets aren't known until then).
    entries: Vec<(*mut ObjFn, Label)>,
}

impl Ir {
    pub fn new() -> Ir {
        Ir {
            code: Vec::new(),
            positions: Vec::new(),
            constants: Vec::new(),
            labels: Vec::new(),
            entries: Vec::new(),
        }
    }

    /// Appends an instruction, attributing it to `pos`.
    pub fn emit(&mut self, inst: Inst, pos: &SourcePosition) {
        self.code.push(inst);
        self.positions.push(pos.clone());
    }

    /// Allocates an unbound label.
    pub fn new_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    /// Pins `label` to the next instruction to be emitted.
    pub fn bind(&mut self, label: Label) {
        self.labels[label.0] = Some(self.code.len());
    }

    /// Records that `func`'s entry point is the (bound) body `label`, so its
    /// `ip_start` can be set to the resolved byte offset during assembly.
    pub fn record_entry(&mut self, func: *mut ObjFn, label: Label) {
        self.entries.push((func, label));
    }

    pub fn entries(&self) -> &[(*mut ObjFn, Label)] {
        &self.entries
    }

    /// Interns a constant, returning its pool index.
    pub fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        self.constants.push(value);
        Ok((self.constants.len() - 1) as u8)
    }

    pub fn code(&self) -> &[Inst] {
        &self.code
    }

    pub fn positions(&self) -> &[SourcePosition] {
        &self.positions
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    /// The instruction index `label` is bound to.
    pub fn label_target(&self, label: Label) -> usize {
        let target = self.labels[label.0].expect("label was never bound");
        debug_assert!(target < self.code.len(), "jump target past end of instruction stream");
        target
    }

    /// Rewrites the instruction stream with a peephole `fuse` function and fixes
    /// up every label to track the new positions.
    pub fn rewrite(self, fuse: impl Fn(&[Inst], usize) -> Option<(Inst, usize)>) -> Ir {
        let mut code = Vec::with_capacity(self.code.len());
        let mut positions = Vec::with_capacity(self.code.len());
        let mut old_to_new = vec![0usize; self.code.len() + 1];

        let mut i = 0;
        while i < self.code.len() {
            let (inst, len) = fuse(&self.code, i).unwrap_or((self.code[i], 1));
            let new_idx = code.len();
            for k in 0..len {
                old_to_new[i + k] = new_idx;
            }
            positions.push(self.positions[i + len - 1].clone());
            code.push(inst);
            i += len;
        }
        old_to_new[self.code.len()] = code.len();

        let labels = self.labels.into_iter()
            .map(|target| target.map(|idx| old_to_new[idx]))
            .collect();

        Ir { code, positions, constants: self.constants, labels, entries: self.entries }
    }
}
