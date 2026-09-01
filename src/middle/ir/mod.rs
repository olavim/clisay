//! The intermediate representation: a flat stream of `Inst`s with symbolic
//! jump `Label`s and a constant pool.

use std::collections::HashSet;
use anyhow::bail;
use fnv::FnvHashMap;

use crate::core::objects::{BuiltinLayout, ObjFn};
use crate::ast::BuiltinType;
use crate::core::objects::TypeId;
use crate::core::value::Value;
use crate::frontend::lex::SourcePosition;

pub const NULL_WITNESS_ID: u16 = 0;
pub const SLOT_ACCEPTS_SCRIPT_FRAME: u16 = 0;
pub const SLOT_ACCEPTS_ANYTHING: u16 = u16::MAX;

/// A `to` for a binding no scope exit closes, so it answers for its slot until its frame ends.
pub const TO_FRAME_END: usize = usize::MAX;

#[derive(Clone, Copy)]
pub struct SlotAccepts {
    pub slot: u8,
    /// The declaration's instruction index.
    pub from: usize,
    /// Where the binding's scope ends.
    pub to: usize,
    pub accepts: u16,
}

fn remap_end(old_to_new: &[usize], to: usize) -> usize {
    match to {
        TO_FRAME_END => TO_FRAME_END,
        to => old_to_new[to],
    }
}

fn intern_row(pool: &mut Vec<Box<[u16]>>, row: Box<[u16]>, what: &str) -> Result<u16, anyhow::Error> {
    if let Some(i) = pool.iter().position(|existing| **existing == *row) {
        return Ok(i as u16);
    }
    if pool.len() >= u16::MAX as usize {
        bail!("Too many distinct {what}");
    }
    pool.push(row);
    Ok((pool.len() - 1) as u16)
}

/// A symbolic jump target, resolved to a byte offset at assembly time.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);

/// A single IR instruction.
#[derive(Clone, Copy)]
pub enum Inst {
    // Control flow
    Call(u8),
    CallMut(u8),
    TailCall(u8),
    /// Brace construction `C { f: v, ... }`.
    Construct(u16, u8),
    Invoke(u8, u8, u8),
    /// `this.name(args)`.
    InvokeThis(u8, u8),
    Jump(Label),
    JumpIfFalse(Label),
    JumpIfFalseOrPop(Label),
    JumpIfTrueOrPop(Label),
    JumpIfNotNullOrPop(Label),
    JumpIfNull(Label),
    JumpIfClean(Label),
    JumpIfBad(Label),
    JumpIfIs(Label, TypeId),
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
    /// A factory's return: deep-freezes the returned instance if the call frame's seal bit is set.
    ReturnFac,
    Halt,
    Throw,
    PushTry(Label),
    PushDeferTry(Label),
    PopTry,
    AssertNonNull,
    AssertImmutable,
    BarrierGuard(u16),
    AssertNoRetain(u8, u16, u16),
    PopScope(u8, u8),

    // Stack / constants
    Pop,
    Dup,
    Dup2,
    PushConstant(u8),
    PushNull,
    PushTrue,
    PushFalse,
    PushClosure(u8),
    PushType(u8),
    /// Builds a type from a template. Its capturing methods bind to the running frame.
    BuildType(u8),

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
    CloseSlotUpvalue(u8),
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
    /// Element count, then whether the literal seals itself immutable.
    Array(u8, u8),
    Dict(u8, u8),
    /// Clears the immutable bit on the object on top of the stack.
    Mut,
    /// Asserts every element of the immutable container on top of the stack is immutable.
    SealCheck,

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
    Is(TypeId),
    HasMember(u8),
    /// Whether a member satisfies what its declaration admits.
    MemberAdmits(u8, u16),
    IsShaped,
    ArrayLen,
    /// Replaces the array on top with a fresh copy of `array[prefix .. len - suffix]`.
    ArrayMiddle(u8, u8),
    /// Replaces the array on top with one element, by an offset from the front or from the back.
    ArrayElem(u8, u8),
}

pub struct Ir {
    /// Each registered object witness name and its id.
    witness_ids: Vec<(TypeId, u16)>,
    builtin_layouts: [Option<BuiltinLayout>; BuiltinType::COUNT],
    /// The witness ids each barrier allows.
    witness_allows: Vec<Box<[u16]>>,
    code: Vec<Inst>,
    positions: Vec<SourcePosition>,
    constants: Vec<Value>,
    /// Maps an already-pooled constant to its index.
    constant_indices: FnvHashMap<Value, u8>,
    labels: Vec<Option<usize>>,
    fn_entries: Vec<(*mut ObjFn, Label)>,
    /// Brace-construction field-id lists.
    construct_fields: Vec<Vec<u8>>,
    survive_positions: Vec<Vec<(u8, SourcePosition)>>,
    /// The obligation a survive barrier's guarded position owes.
    owed_names: Vec<Box<[(u8, Box<str>)]>>,
    /// Instruction indices of the checks that check-forcing put back.
    /// Empty unless check-forcing is on.
    elisions: Vec<usize>,
    param_accepts: Vec<Box<[u16]>>,
    /// One table per frame, the script's first.
    slot_accepts: Vec<Vec<SlotAccepts>>,
    /// Extra source positions an instruction needs, keyed by instruction index and role.
    source_map: FnvHashMap<(usize, SourceRole), SourcePosition>,
}

/// Which part of an instruction an extra source position belongs to.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SourceRole {
    /// The expression whose value a store writes.
    StoredValue,
    /// The same, where that expression is a bare name. Only then can a diagnostic quote it back as
    /// a declaration to change.
    StoredName,
}

impl Ir {
    pub fn new() -> Ir {
        Ir {
            witness_ids: Vec::new(),
            builtin_layouts: std::array::from_fn(|_| None),
            witness_allows: Vec::new(),
            code: Vec::new(),
            positions: Vec::new(),
            constants: Vec::new(),
            constant_indices: FnvHashMap::default(),
            labels: Vec::new(),
            fn_entries: Vec::new(),
            construct_fields: Vec::new(),
            survive_positions: Vec::new(),
            owed_names: Vec::new(),
            elisions: Vec::new(),
            param_accepts: Vec::new(),
            slot_accepts: Vec::new(),
            source_map: FnvHashMap::default(),
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

    /// Records the obligations a survive barrier's guarded positions owe.
    pub fn add_owed_names(&mut self, owed: Box<[(u8, Box<str>)]>) -> Result<u16, anyhow::Error> {
        if let Some(i) = self.owed_names.iter().position(|o| **o == *owed) {
            return Ok(i as u16);
        }
        if self.owed_names.len() >= u16::MAX as usize {
            bail!("Too many opaque-call barriers");
        }
        self.owed_names.push(owed);
        Ok((self.owed_names.len() - 1) as u16)
    }

    pub fn owed_names(&self) -> &[Box<[(u8, Box<str>)]>] {
        &self.owed_names
    }

    /// The index the next emitted instruction will take.
    pub fn next_index(&self) -> usize {
        self.code.len()
    }

    /// Marks every instruction emitted since `from` as a forced check.
    pub fn mark_elisions_from(&mut self, from: usize) {
        self.elisions.extend(from..self.code.len());
    }

    pub fn elisions(&self) -> &[usize] {
        &self.elisions
    }

    pub fn map_source(&mut self, index: usize, role: SourceRole, pos: SourcePosition) {
        self.source_map.insert((index, role), pos);
    }

    pub fn source_map(&self) -> &FnvHashMap<(usize, SourceRole), SourcePosition> {
        &self.source_map
    }

    /// Records the program's object witness names for the VM's boundary-barrier registry.
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
    pub fn set_witness_ids(&mut self, ids: Vec<(TypeId, u16)>) {
        self.witness_ids = ids;
    }

    pub fn set_builtin_layout(&mut self, builtin: BuiltinType, layout: BuiltinLayout) {
        self.builtin_layouts[builtin.index()] = Some(layout);
    }

    pub fn builtin_layouts(&self) -> &[Option<BuiltinLayout>; BuiltinType::COUNT] {
        &self.builtin_layouts
    }

    pub fn into_builtin_layouts(self) -> [Option<BuiltinLayout>; BuiltinType::COUNT] {
        self.builtin_layouts
    }

    pub fn witness_ids(&self) -> &[(TypeId, u16)] {
        &self.witness_ids
    }

    pub fn witness_allows(&self) -> &[Box<[u16]>] {
        &self.witness_allows
    }

    pub fn add_param_accepts(&mut self, accepts: Box<[u16]>) -> Result<u16, anyhow::Error> {
        intern_row(&mut self.param_accepts, accepts, "parameter accept sets")
    }

    pub fn new_slot_accepts_table(&mut self) -> Result<u16, anyhow::Error> {
        let index = self.slot_accepts.len();
        if index >= u16::MAX as usize {
            bail!("Too many frames with their own slots");
        }
        self.slot_accepts.push(Vec::new());
        Ok(index as u16)
    }

    pub fn end_slot_accepts_from(&mut self, table_id: u16, first_dead: u8) {
        let at = self.code.len();
        for entry in self.slot_accepts[table_id as usize].iter_mut() {
            if entry.slot >= first_dead && entry.to == TO_FRAME_END {
                entry.to = at;
            }
        }
    }

    /// Records what a binding's slot accepts, from its declaration onward.
    pub fn record_slot_accepts(&mut self, table: u16, slot: u8, accepts: u16) {
        let from = self.code.len();
        self.slot_accepts[table as usize].push(SlotAccepts { slot, from, to: TO_FRAME_END, accepts });
    }

    pub fn slot_accepts(&self) -> &[Vec<SlotAccepts>] {
        &self.slot_accepts
    }

    pub fn param_accepts(&self) -> &[Box<[u16]>] {
        &self.param_accepts
    }

    /// Pools a barrier's allowed witness ids.
    pub fn add_witness_allow(&mut self, allow: Box<[u16]>) -> Result<u16, anyhow::Error> {
        intern_row(&mut self.witness_allows, allow, "barrier witness sets")
    }

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

        // Fusing a set of instructions leaves one instruction, so every jump into that set ends up
        // pointing at it. That's correct for a jump to the run's first instruction, but not for a jump
        // to a later one.
        let targeted: HashSet<usize> = self.labels.iter().flatten().copied().collect();

        let mut i = 0;
        while i < self.code.len() {
            let fused = fuse(&self.code, i)
                .filter(|(_, len)| !(1..*len).any(|k| targeted.contains(&(i + k))));
            let (inst, len) = fused.unwrap_or((self.code[i], 1));
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
            survive_positions: self.survive_positions,
            owed_names: self.owed_names,
            // A rewrite moves instructions, so each marked check follows its own index.
            elisions: self.elisions.iter().map(|&idx| old_to_new[idx]).collect(),
            source_map: self.source_map.iter().map(|(&(idx, role), pos)| ((old_to_new[idx], role), pos.clone())).collect(),
            witness_ids: self.witness_ids,
            builtin_layouts: self.builtin_layouts,
            witness_allows: self.witness_allows,
            param_accepts: self.param_accepts,
            slot_accepts: self.slot_accepts.into_iter()
                .map(|body| body.into_iter().map(|e| SlotAccepts { from: old_to_new[e.from], to: remap_end(&old_to_new, e.to), ..e }).collect())
                .collect(),
        }
    }
}
