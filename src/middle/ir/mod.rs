//! The intermediate representation.

use anyhow::bail;
use fnv::FnvHashMap;

use crate::core::objects::ObjFn;
use crate::core::value::Value;
use crate::frontend::lex::SourcePosition;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);

/// A single IR instruction.
#[derive(Clone, Copy)]
pub enum Inst {
    // Control flow
    Call(u8),
    Construct(u16, u8),
    Invoke(u8, u8),
    Jump(Label),
    JumpIfFalse(Label),
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
    PushType(u8),

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
    GetProperty,
    SetProperty,
    GetPropertyId(u8),
    SetPropertyId(u8),
    SetPropertyIdPop(u8),
    Array(u8),
    Dict(u8),

    // Arithmetic
    Add,
    AddLocalConst(u8, u8), // local + const
    AddConstLocal(u8, u8), // const + local
    Subtract,
    SubLocalConst(u8, u8), // local - const
    SubConstLocal(u8, u8), // const - local
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
}

pub struct Ir {
    code: Vec<Inst>,
    positions: Vec<SourcePosition>,
    constants: Vec<Value>,
    constant_indices: FnvHashMap<Value, u8>,
    labels: Vec<Option<usize>>,
    /// Function entry points.
    entries: Vec<(*mut ObjFn, Label)>,
    /// Brace-construction field-id lists, referenced by index from `Inst::Construct`.
    /// Kept out of the instruction so `Inst` stays `Copy`.
    construct_fields: Vec<Vec<u8>>,
}

impl Ir {
    pub fn new() -> Ir {
        Ir {
            code: Vec::new(),
            positions: Vec::new(),
            constants: Vec::new(),
            constant_indices: FnvHashMap::default(),
            labels: Vec::new(),
            entries: Vec::new(),
            construct_fields: Vec::new(),
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

    pub fn emit(&mut self, inst: Inst, pos: &SourcePosition) {
        self.code.push(inst);
        self.positions.push(pos.clone());
    }

    pub fn new_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    /// Pins `label` to the next instruction to be emitted.
    pub fn bind(&mut self, label: Label) {
        self.labels[label.0] = Some(self.code.len());
    }

    pub fn record_entry(&mut self, func: *mut ObjFn, label: Label) {
        self.entries.push((func, label));
    }

    pub fn entries(&self) -> &[(*mut ObjFn, Label)] {
        &self.entries
    }

    /// Interns a constant, returning its pool index.
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

        Ir { code, positions, constants: self.constants, constant_indices: self.constant_indices, labels, entries: self.entries, construct_fields: self.construct_fields }
    }
}
