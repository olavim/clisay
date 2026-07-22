//! The intermediate representation: a flat stream of `Inst`s with symbolic
//! jump `Label`s and a constant pool.

use anyhow::bail;
use fnv::FnvHashMap;

use crate::core::objects::ObjFn;
use crate::core::value::Value;
use crate::frontend::lex::SourcePosition;

/// A symbolic jump target, resolved to a byte offset at assembly time.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);

/// A single IR instruction.
#[derive(Clone, Copy)]
pub enum Inst {
    // Control flow
    Call(u8),
    /// Brace construction `C(args) { f: v, ... }`.
    Construct(u16, u8),
    /// Fused method call `recv.name(args)`.
    Invoke(u8, u8),
    Jump(Label),
    JumpIfFalse(Label),
    JumpIfFalseOrPop(Label),
    JumpIfTrueOrPop(Label),
    JumpIfNotNullOrPop(Label),
    JumpIfNull(Label),
    JumpIfClean(Label),
    JumpIfBad(Label),
    JumpIfIs(Label, u8),
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
    Halt,
    Throw,
    PushTry(Label),
    PopTry,
    /// Aborts if the top of the stack is null, else leaves it.
    AssertNonNull,
    /// Guards an unknown value at a destination: throws any registered witness the destination
    /// does not allow.
    BarrierGuard(u16),
    /// Asserts an opaque callee borrows the guarded argument positions. Operands are the argument
    /// count (the callee's stack depth) and an index into the barrier's position list.
    AssertBorrow(u8, u16),
    /// Marks the listed argument positions borrowed for the call that follows. Operands are the
    /// argument count and an index into the position list.
    MarkBorrow(u8, u16),
    /// Releases the last `count` marked borrows.
    ReleaseBorrow(u8),

    // Stack / constants
    Pop,
    /// Pushes a copy of the top of the stack.
    Dup,
    PushConstant(u8),
    PushNull,
    PushTrue,
    PushFalse,
    PushClosure(u8),
    PushType(u8),

    // Variables and properties
    LoadGlobal(u8),
    LoadLocal(u8),
    StoreLocal(u8),
    StoreLocalPop(u8),
    StoreLocalAddLocalLocal(u8, u8, u8), // dst = a + b
    LoadUpvalue(u8),
    StoreUpvalue(u8),
    StoreUpvaluePop(u8),
    CloseUpvalue(u8),
    GetIndex,
    SetIndex,
    GetIndexOrNull(u8),
    /// Dynamic member access by name (`.name`).
    GetProperty,
    SetProperty,
    /// Instance member access by resolved layout id (`this.x`), skipping the name lookup.
    GetField(u8),
    SetField(u8),
    SetFieldPop(u8),
    Array(u8),
    Dict(u8),

    // Arithmetic
    Add,
    AddLocalConst(u8, u8), // local + const
    AddConstLocal(u8, u8), // const + local
    Subtract,
    SubLocalConst(u8, u8), // local - const
    SubConstLocal(u8, u8), // const - local
    IncLocal(u8, u8),      // local = local + const
    DecLocal(u8, u8),      // local = local - const
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
    Is(u8),
    HasMember(u8),
    /// Replaces the top with whether it is a dict or instance, the values a shape can match.
    IsShaped,
    ArrayLen,
    /// Replaces the array on top with a fresh copy of `array[prefix .. len - suffix]`.
    ArrayMiddle(u8, u8),
}

/// A boundary barrier's data: whether the destination permits null, and the constant-pool indices
/// of the witness names it allows.
pub struct BarrierAllow {
    pub null_allowed: bool,
    pub names: Vec<u8>,
}

pub struct Ir {
    code: Vec<Inst>,
    positions: Vec<SourcePosition>,
    constants: Vec<Value>,
    /// Maps an already-pooled constant to its index.
    constant_indices: FnvHashMap<Value, u8>,
    labels: Vec<Option<usize>>,
    fn_entries: Vec<(*mut ObjFn, Label)>,
    /// Brace-construction field-id lists.
    construct_fields: Vec<Vec<u8>>,
    barrier_allows: Vec<BarrierAllow>,
    survive_positions: Vec<Vec<(u8, SourcePosition)>>,
    witness_names: Vec<Value>,
}

impl Ir {
    pub fn new() -> Ir {
        Ir {
            code: Vec::new(),
            positions: Vec::new(),
            constants: Vec::new(),
            constant_indices: FnvHashMap::default(),
            labels: Vec::new(),
            fn_entries: Vec::new(),
            construct_fields: Vec::new(),
            barrier_allows: Vec::new(),
            survive_positions: Vec::new(),
            witness_names: Vec::new(),
        }
    }

    pub fn add_construct_fields(&mut self, fields: Vec<u8>) -> Result<u16, anyhow::Error> {
        if self.construct_fields.len() >= u16::MAX as usize {
            bail!("Too many brace constructions");
        }
        self.construct_fields.push(fields);
        Ok((self.construct_fields.len() - 1) as u16)
    }

    pub fn construct_fields(&self, idx: u16) -> &[u8] {
        &self.construct_fields[idx as usize]
    }

    /// Records a barrier's guarded argument positions.
    pub fn add_survive_positions(&mut self, positions: Vec<(u8, SourcePosition)>) -> Result<u16, anyhow::Error> {
        if self.survive_positions.len() >= u16::MAX as usize {
            bail!("Too many opaque-call barriers");
        }
        self.survive_positions.push(positions);
        Ok((self.survive_positions.len() - 1) as u16)
    }

    pub fn survive_positions(&self, idx: u16) -> &[(u8, SourcePosition)] {
        &self.survive_positions[idx as usize]
    }

    pub fn add_barrier_allow(&mut self, allow: BarrierAllow) -> Result<u16, anyhow::Error> {
        if self.barrier_allows.len() >= u16::MAX as usize {
            bail!("Too many boundary barriers");
        }
        self.barrier_allows.push(allow);
        Ok((self.barrier_allows.len() - 1) as u16)
    }

    pub fn barrier_allow(&self, idx: u16) -> &BarrierAllow {
        &self.barrier_allows[idx as usize]
    }

    /// Records the program's object witness names for the VM's boundary-barrier registry.
    pub fn set_witness_names(&mut self, names: Vec<Value>) {
        self.witness_names = names;
    }

    pub fn witness_names(&self) -> &[Value] {
        &self.witness_names
    }

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

    /// Records that `func`'s entry point is at `label`.
    pub fn record_fn_entry(&mut self, func: *mut ObjFn, label: Label) {
        self.fn_entries.push((func, label));
    }

    pub fn fn_entries(&self) -> &[(*mut ObjFn, Label)] {
        &self.fn_entries
    }

    /// Interns a constant, returning its pool index. Equal values reuse one slot.
    pub fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if let Some(&idx) = self.constant_indices.get(&value) {
            return Ok(idx);
        }
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        let idx = self.constants.len() as u8;
        self.constants.push(value);
        self.constant_indices.insert(value, idx);
        Ok(idx)
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

        Ir {
            code,
            positions,
            constants: self.constants,
            constant_indices: self.constant_indices,
            labels,
            fn_entries: self.fn_entries,
            construct_fields: self.construct_fields,
            barrier_allows: self.barrier_allows,
            survive_positions: self.survive_positions,
            witness_names: self.witness_names
        }
    }
}
