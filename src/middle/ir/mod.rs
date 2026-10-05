//! The intermediate representation.

use std::collections::HashSet;
use anyhow::bail;
use fnv::FnvHashMap;

use crate::core::objects::{BuiltinLayout, ObjFn};
use crate::ast::BuiltinType;
use crate::core::objects::TypeId;
use crate::core::value::Value;
use crate::frontend::lex::SourcePosition;

pub const NULL_WITNESS_ID: u16 = 0;
pub const SCRIPT_SLOT_WITNESS_SET_POOL_ID: u16 = 0;
pub const PARAM_IS_ANCHOR: u16 = 1 << 15;
pub const PARAM_LIST_HAS_ANCHOR: u16 = 1 << 15;
pub const RECEIVER_IS_COPIED: u16 = 1 << 14;
pub const WANTS_ANCHOR_RECEIVER: u16 = 1 << 13;
pub const PARAM_LIST_POOL_ID: u16 = !(PARAM_LIST_HAS_ANCHOR | RECEIVER_IS_COPIED | WANTS_ANCHOR_RECEIVER);

pub const CALL_ARGS_SETTLED: u8 = 1 << 0;
pub const CALL_KINDS_PROVEN: u8 = 1 << 1;
pub const CALL_WANTS_VALUE: u8 = 1 << 2;
pub const CALL_PASSES_ANCHOR_RECEIVER: u8 = 1 << 3;
pub const CALL_TESTS_CALLEE: u8 = 1 << 4;

pub const NO_WITNESS_SET: u16 = u16::MAX;

pub const COPY_IN_WRITTEN: u8 = 1 << 0;
pub const COPY_IN_ADMITS_NO_PERSIST: u8 = 1 << 1;

pub const TO_FRAME_END: usize = usize::MAX;

#[derive(Clone, Copy)]
pub struct SlotWitnessSet {
    pub slot: u8,
    /// The declaration's instruction index.
    pub from: usize,
    /// Where the binding's scope ends.
    pub to: usize,
    pub witness_set_pool_id: u16,
}

fn remap_end(old_to_new: &[usize], to: usize) -> usize {
    match to {
        TO_FRAME_END => TO_FRAME_END,
        to => old_to_new[to],
    }
}

fn intern(pool: &mut Vec<Box<[u16]>>, entry: Box<[u16]>, what: &str) -> Result<u16, anyhow::Error> {
    if let Some(i) = pool.iter().position(|existing| **existing == *entry) {
        return Ok(i as u16);
    }
    if pool.len() >= WANTS_ANCHOR_RECEIVER as usize {
        bail!("Too many distinct {what}");
    }
    pool.push(entry);
    Ok((pool.len() - 1) as u16)
}

/// A symbolic jump target, resolved to a byte offset at assembly time.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Label(usize);

impl Label {
    pub fn index(self) -> usize {
        self.0
    }
}


/// A single IR instruction.
#[derive(Clone, Copy, Debug)]
pub enum Inst {
    Call(u8, u8),
    TailCall(u8, u8),
    Invoke(u8, u8, u8, u8),
    /// `this.name(args)`.
    InvokeThis(u8, u8, u8),
    /// Brace construction `C { f: v, ... }`.
    Construct(u16),
    Jump(Label),
    JumpIfFalse(Label),
    JumpIfFalseOrPop(Label),
    JumpIfTrueOrPop(Label),
    JumpIfCleanOrPop(Label),
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
    ReturnShared,
    ReturnFac,
    Halt,
    Throw,
    AssertNonNull,
    BarrierGuard(u16),
    PopScope(u8, u8),
    Pop,
    /// Discards an expression statement's value, checking that it wasn't must-use.
    DiscardChecked,
    Dup,
    Dup2,
    PushConstant(u8),
    PushNull,
    PushUnassigned,
    PushTrue,
    PushFalse,
    BuildClosure(u8),
    PushType(u8),
    /// Builds a type from a template. Its capturing methods bind to the running frame.
    BuildType(u8),
    LoadGlobal(u8),
    LoadLocal(u8),
    LoadLocalForWrite(u8),
    LoadAnchorForWrite(u8),
    CopyObject,
    ShareLocal(u8),
    StoreLocalFresh(u8),
    StoreLocalFreshPop(u8),
    LoadStepForWrite(u8),
    StoreLocal(u8),
    PushSlotAnchor(u8, u16),
    LoadAnchor(u8),
    StoreAnchor(u8),
    StoreTempPop(u8),
    /// Walks the path from the root on top through the anchor's saved keys, and pushes the anchor.
    FormAnchorPath(u8, u16),
    StoreLocalPop(u8),
    StoreLocalAddLocalLocal(u8, u8, u8), // dst = a + b
    LoadCapture(u8),
    BindClosureCaptures(u8, u8),
    BindTypeCaptures(u8, u8),
    BuildClosureUnbound(u8),
    BuildTypeUnbound(u8),
    GetIndex,
    GetIndexOrNull(u8),
    GetProperty,
    SetIndex,
    SetProperty,
    /// Instance member access by resolved layout id (`this.x`), skipping the name lookup.
    GetField(u8),
    SetField(u8),
    SetFieldPop(u8),
    GetMember(u8),
    SetMember(u8),
    CheckAnchorRoot(u8, u8, u8),
    RecordAnchorRoot(u8),
    Array(u8),
    Dict(u8),
    Add,
    Subtract,
    AddLocalConst(u8, u8), // local + const
    AddConstLocal(u8, u8), // const + local
    SubLocalConst(u8, u8), // local - const
    SubConstLocal(u8, u8), // const - local
    IncLocal(u8, u8),      // local = local + const
    DecLocal(u8, u8),      // local = local - const
    /// Copies the value at an anchor into a frame slot.
    CopyAnchorIn(u8, u8, u16),
    /// Copies a frame slot's value back to an anchor.
    CopyAnchorOut(u8, u8),
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
    IsDict,
    LoadRef,
    LoadRefForWrite,
    StoreRef,
    StoreRefPop,
    /// The dict's entries whose keys are not among the `n` on the stack above it.
    DictRest(u8),
    /// The dict's entries whose keys are not among the `n` on the stack above it, as an array.
    DictRestValues(u8),
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
    witness_set_pool: Vec<Box<[u16]>>,
    code: Vec<Inst>,
    positions: Vec<SourcePosition>,
    constants: Vec<Value>,
    constant_indices: FnvHashMap<Value, u8>,
    labels: Vec<Option<usize>>,
    fn_entries: Vec<(*mut ObjFn, Label)>,
    byte_lists: Vec<Vec<u8>>,
    /// Instruction indices of the checks that check-forcing put back.
    /// Empty unless check-forcing is on.
    forced_checks: Vec<usize>,
    param_list_pool: Vec<Box<[u16]>>,
    slot_witness_set_pool: Vec<Vec<SlotWitnessSet>>,
    anchor_params: Vec<Box<[(u8, u8)]>>,
    throw_targets: Vec<ThrowTarget>,
    /// Extra source positions an instruction needs, keyed by instruction index and role.
    source_map: FnvHashMap<(usize, SourceRole), SourcePosition>,
}

/// From `from` on, a throw goes to `handler`.
#[derive(Clone, Copy)]
pub struct ThrowTarget {
    pub from: Label,
    pub handler: Option<(Label, u16)>,
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
            witness_set_pool: Vec::new(),
            code: Vec::new(),
            positions: Vec::new(),
            constants: Vec::new(),
            constant_indices: FnvHashMap::default(),
            labels: Vec::new(),
            fn_entries: Vec::new(),
            byte_lists: Vec::new(),
            forced_checks: Vec::new(),
            param_list_pool: Vec::new(),
            slot_witness_set_pool: Vec::new(),
            anchor_params: Vec::new(),
            throw_targets: Vec::new(),
            source_map: FnvHashMap::default(),
        }
    }

    pub fn add_byte_list(&mut self, list: Vec<u8>) -> Result<u16, anyhow::Error> {
        if self.byte_lists.len() >= u16::MAX as usize {
            bail!("Too many byte lists");
        }
        self.byte_lists.push(list);
        Ok((self.byte_lists.len() - 1) as u16)
    }

    pub fn byte_list(&self, idx: u16) -> &[u8] {
        &self.byte_lists[idx as usize]
    }

    /// The index the next emitted instruction will take.
    pub fn next_index(&self) -> usize {
        self.code.len()
    }

    /// Marks every instruction emitted since `from` as a forced check.
    pub fn mark_forced_from(&mut self, from: usize) {
        self.forced_checks.extend(from..self.code.len());
    }

    pub fn forced_checks(&self) -> &[usize] {
        &self.forced_checks
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

    /// Interns a constant, returning its pool index.
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

    pub fn intern_param_list(&mut self, param_list: Box<[u16]>) -> Result<u16, anyhow::Error> {
        intern(&mut self.param_list_pool, param_list, "parameter lists")
    }

    pub fn intern_witness_set(&mut self, witness_set: Box<[u16]>) -> Result<u16, anyhow::Error> {
        intern(&mut self.witness_set_pool, witness_set, "barrier witness sets")
    }

    pub fn witness_ids(&self) -> &[(TypeId, u16)] {
        &self.witness_ids
    }

    pub fn witness_set_pool(&self) -> &[Box<[u16]>] {
        &self.witness_set_pool
    }

    pub fn push_slot_witness_set_table(&mut self) -> Result<u16, anyhow::Error> {
        let index = self.slot_witness_set_pool.len();
        if index >= u16::MAX as usize {
            bail!("Too many frames with their own slots");
        }
        self.slot_witness_set_pool.push(Vec::new());
        self.anchor_params.push(Box::new([]));
        Ok(index as u16)
    }

    pub fn end_slot_witness_sets_from(&mut self, table_id: u16, first_dead: u8) {
        let at = self.code.len();
        for entry in self.slot_witness_set_pool[table_id as usize].iter_mut() {
            if entry.slot >= first_dead && entry.to == TO_FRAME_END {
                entry.to = at;
            }
        }
    }

    pub fn push_slot_witness_set(&mut self, slot_witness_set_pool_id: u16, slot: u8, witness_set_pool_id: u16) {
        let from = self.code.len();
        self.slot_witness_set_pool[slot_witness_set_pool_id as usize].push(SlotWitnessSet { slot, from, to: TO_FRAME_END, witness_set_pool_id });
    }

    pub fn slot_witness_set_pool(&self) -> &[Vec<SlotWitnessSet>] {
        &self.slot_witness_set_pool
    }

    pub fn record_anchor_params(&mut self, table: u16, params: Box<[(u8, u8)]>) {
        self.anchor_params[table as usize] = params;
    }

    pub fn anchor_params(&self) -> &[Box<[(u8, u8)]>] {
        &self.anchor_params
    }

    pub fn set_throw_target(&mut self, handler: Option<(Label, u16)>) {
        let here = self.code.len();
        if let Some(last) = self.throw_targets.last_mut().filter(|last| self.labels[last.from.0] == Some(here)) {
            last.handler = handler;
            return;
        }
        let from = self.new_label();
        self.bind(from);
        self.throw_targets.push(ThrowTarget { from, handler });
    }

    pub fn throw_targets(&self) -> &[ThrowTarget] {
        &self.throw_targets
    }

    pub fn param_list_pool(&self) -> &[Box<[u16]>] {
        &self.param_list_pool
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

    /// The instruction a label is bound to, which may be one past the last one.
    pub fn label_position(&self, label: Label) -> usize {
        self.labels[label.0].expect("label was never bound")
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
            byte_lists: self.byte_lists,
            // A rewrite moves instructions, so each marked check follows its own index.
            forced_checks: self.forced_checks.iter().map(|&idx| old_to_new[idx]).collect(),
            source_map: self.source_map.iter().map(|(&(idx, role), pos)| ((old_to_new[idx], role), pos.clone())).collect(),
            witness_ids: self.witness_ids,
            builtin_layouts: self.builtin_layouts,
            witness_set_pool: self.witness_set_pool,
            param_list_pool: self.param_list_pool,
            anchor_params: self.anchor_params,
            throw_targets: self.throw_targets,
            slot_witness_set_pool: self.slot_witness_set_pool.into_iter()
                .map(|body| body.into_iter().map(|e| SlotWitnessSet { from: old_to_new[e.from], to: remap_end(&old_to_new, e.to), ..e }).collect())
                .collect(),
        }
    }
}

/// What an instruction does to the operand stack on each path out of it.
pub struct StackEffect {
    /// Values popped and pushed on the way to the next instruction. None when control never
    /// goes on to it, such as after a jump.
    pub next: Option<(usize, usize)>,
    /// The label a jump may continue at, with the values popped and pushed on that path.
    pub jump: Option<(Label, usize, usize)>,
}

impl StackEffect {
    fn next(pops: usize, pushes: usize) -> StackEffect {
        StackEffect { next: Some((pops, pushes)), jump: None }
    }

    fn end() -> StackEffect {
        StackEffect { next: None, jump: None }
    }

    fn branch(label: Label, pops: usize, jump_pops: usize) -> StackEffect {
        StackEffect { next: Some((pops, 0)), jump: Some((label, jump_pops, 0)) }
    }
}

impl Ir {
    pub fn stack_effect(&self, inst: &Inst) -> StackEffect {
        match *inst {
            Inst::Call(_, arity)
                | Inst::Invoke(_, _, arity, _)
                | Inst::InvokeThis(_, _, arity) => StackEffect::next(arity as usize + 1, 1),
            Inst::Construct(fields) => StackEffect::next(self.byte_list(fields).len() + 1, 1),
            Inst::Jump(label) => StackEffect { next: None, jump: Some((label, 0, 0)) },
            Inst::JumpIfFalse(label) => StackEffect::branch(label, 1, 1),
            Inst::JumpIfFalseOrPop(label)
                | Inst::JumpIfTrueOrPop(label)
                | Inst::JumpIfCleanOrPop(label) => StackEffect::branch(label, 1, 0),
            Inst::JumpIfClean(label)
                | Inst::JumpIfBad(label)
                | Inst::JumpIfIs(label, _) => StackEffect::branch(label, 0, 0),
            Inst::JumpIfGe(label)
                | Inst::JumpIfGt(label)
                | Inst::JumpIfLe(label)
                | Inst::JumpIfLt(label)
                | Inst::JumpIfEq(label)
                | Inst::JumpIfNeq(label) => StackEffect::branch(label, 2, 2),
            Inst::JumpIfGeLocalConst(label, ..)
                | Inst::JumpIfGtLocalConst(label, ..)
                | Inst::JumpIfLeLocalConst(label, ..)
                | Inst::JumpIfLtLocalConst(label, ..) => StackEffect::branch(label, 0, 0),
            Inst::TailCall(..)
                | Inst::Return
                | Inst::ReturnShared
                | Inst::ReturnFac
                | Inst::Halt
                | Inst::Throw => StackEffect::end(),
            Inst::AssertNonNull
                | Inst::BarrierGuard(_) => StackEffect::next(0, 0),
            Inst::PopScope(count, _) => StackEffect::next(count as usize, 0),
            Inst::Pop | Inst::DiscardChecked => StackEffect::next(1, 0),
            Inst::Dup => StackEffect::next(1, 2),
            Inst::Dup2 => StackEffect::next(2, 4),
            Inst::PushConstant(_)
                | Inst::PushNull
                | Inst::PushUnassigned
                | Inst::PushTrue
                | Inst::PushFalse
                | Inst::BuildClosure(_)
                | Inst::PushType(_)
                | Inst::BuildType(_)
                | Inst::LoadGlobal(_)
                | Inst::LoadLocal(_)
                | Inst::LoadLocalForWrite(_)
                | Inst::LoadAnchorForWrite(_)
                | Inst::PushSlotAnchor(..)
                | Inst::LoadAnchor(_)
                | Inst::LoadCapture(_)
                | Inst::BuildClosureUnbound(_)
                | Inst::BuildTypeUnbound(_)
                | Inst::AddLocalConst(..)
                | Inst::AddConstLocal(..)
                | Inst::SubLocalConst(..)
                | Inst::SubConstLocal(..) => StackEffect::next(0, 1),
            Inst::CopyObject
                | Inst::ShareLocal(_)
                | Inst::StoreLocalFresh(_)
                | Inst::StoreLocal(_)
                | Inst::StoreAnchor(_)
                | Inst::StoreLocalAddLocalLocal(..)
                | Inst::BindClosureCaptures(..)
                | Inst::BindTypeCaptures(..)
                | Inst::IncLocal(..)
                | Inst::CopyAnchorOut(..)
                | Inst::DecLocal(..) => StackEffect::next(0, 0),
            Inst::StoreLocalFreshPop(_)
                | Inst::StoreLocalPop(_)
                | Inst::StoreTempPop(_) => StackEffect::next(1, 0),
            Inst::FormAnchorPath(..) => StackEffect::next(1, 1),
            Inst::CopyAnchorIn(_, flags, _) => StackEffect::next(0, 1 + (flags & COPY_IN_WRITTEN) as usize),
            Inst::LoadStepForWrite(_)
                | Inst::GetIndex
                | Inst::GetProperty => StackEffect::next(2, 1),
            Inst::GetIndexOrNull(_)
                | Inst::GetField(_)
                | Inst::GetMember(_) => StackEffect::next(1, 1),
            Inst::SetIndex
                | Inst::SetProperty => StackEffect::next(3, 1),
            Inst::SetField(_)
                | Inst::SetMember(_) => StackEffect::next(2, 1),
            Inst::SetFieldPop(_) => StackEffect::next(2, 0),
            Inst::CheckAnchorRoot(..) => StackEffect::next(0, 0),
            Inst::RecordAnchorRoot(_) => StackEffect::next(1, 0),
            Inst::Array(count) => StackEffect::next(count as usize, 1),
            Inst::Dict(count) => StackEffect::next(2 * count as usize, 1),
            Inst::Add
                | Inst::Subtract
                | Inst::Multiply
                | Inst::Divide
                | Inst::LeftShift
                | Inst::RightShift
                | Inst::BitAnd
                | Inst::BitOr
                | Inst::BitXor
                | Inst::Equal
                | Inst::NotEqual
                | Inst::LessThan
                | Inst::LessThanEqual
                | Inst::GreaterThan
                | Inst::GreaterThanEqual => StackEffect::next(2, 1),
            Inst::Negate
                | Inst::Not
                | Inst::BitNot
                | Inst::Is(_)
                | Inst::HasMember(_)
                | Inst::MemberAdmits(..)
                | Inst::IsShaped
                | Inst::IsDict
                | Inst::ArrayLen
                | Inst::ArrayMiddle(..)
                | Inst::ArrayElem(..)
                | Inst::LoadRef
                | Inst::LoadRefForWrite => StackEffect::next(1, 1),
            Inst::StoreRef => StackEffect::next(2, 1),
            Inst::StoreRefPop => StackEffect::next(2, 0),
            Inst::DictRest(count)
                | Inst::DictRestValues(count) => StackEffect::next(count as usize + 1, 1),
        }
    }
}
