//! Lowers Ir into bytecode.

use anyhow::bail;

use crate::ast::BuiltinType;
use crate::backend::bytecode::chunk::{BytecodeChunk, HandlerRange};
use crate::backend::bytecode::opcode;
use crate::core::objects::TypeMember;
use crate::frontend::lex::SourcePosition;
use crate::middle::ir::{TO_FRAME_END, SlotWitnessSet, Inst, Ir, Label};

pub fn assemble(ir: Ir) -> Result<BytecodeChunk, anyhow::Error> {
    let mut offsets = Vec::with_capacity(ir.code().len());
    let mut size = 0usize;
    for inst in ir.code() {
        offsets.push(size);
        size += encoded_len(inst, &ir);
    }

    if size > u16::MAX as usize {
        bail!("Bytecode too large");
    }

    // A label may sit one past the last instruction.
    offsets.push(size);

    // Finalise function entry points now that byte offsets are known.
    for &(func, body_label) in ir.fn_entries() {
        unsafe { (*func).ip_start = landing_offset(&ir, &offsets, body_label); }
    }

    let mut chunk = BytecodeChunk::new();
    chunk.witness_ids = ir.witness_ids().to_vec();
    if ir.builtin_layouts().iter().any(Option::is_none) {
        bail!("a built-in type reached assembly with no layout");
    }

    let err_layout = ir.builtin_layouts()[BuiltinType::Err.index()].as_ref().expect("every built-in layout is present");
    if err_layout.field_count != 1 || !err_layout.members.iter().any(|(name, m)| name == "value" && matches!(m, TypeMember::Field(0))) {
        bail!("Err's native factory has one field");
    }

    let ref_layout = ir.builtin_layouts()[BuiltinType::Ref.index()].as_ref().expect("every built-in layout is present");
    if ref_layout.field_count != 2 {
        bail!("Ref's native factory sets a value and a lock");
    }

    chunk.witness_set_pool = ir.witness_set_pool().to_vec();
    chunk.param_list_pool = ir.param_list_pool().to_vec();
    chunk.slot_witness_set_pool = ir.slot_witness_set_pool().iter()
        .map(|body| body.iter().map(|e| SlotWitnessSet { from: offsets[e.from], to: if e.to == TO_FRAME_END { TO_FRAME_END } else { offsets[e.to] }, ..*e }).collect())
        .collect();
    chunk.anchor_params = ir.anchor_params().to_vec();

    let at = |label: Label| offsets[ir.label_position(label)] as u16;
    let targets = ir.throw_targets();
    chunk.handlers = targets.iter().zip(targets.iter().skip(1).map(|next| at(next.from)).chain([size as u16]))
        .filter_map(|(target, end)| target.handler.map(|(handler, height)| HandlerRange { start: at(target.from), end, handler: at(handler), frame_stack_height: height }))
        .filter(|range| range.start < range.end)
        .collect();
    debug_assert!(chunk.handlers.windows(2).all(|pair| pair[0].end <= pair[1].start), "handler ranges should be in order");

    chunk.constants = ir.constants().to_vec();

    chunk.forced_check_ends = ir.forced_checks().iter().map(|&idx| offsets[idx + 1] - 1).collect();

    for (&(idx, role), pos) in ir.source_map() {
        let end = offsets[idx + 1];
        for offset in offsets[idx]..end {
            chunk.source_map.insert((offset, role), pos.clone());
        }
    }

    for (i, inst) in ir.code().iter().enumerate() {
        encode(inst, &offsets, &ir, &mut chunk, &ir.positions()[i]);
    }

    chunk.builtin_layouts = ir.into_builtin_layouts();

    Ok(chunk)
}

/// The byte offset a jump to `label`, or a call into it, lands on.
fn landing_offset(ir: &Ir, offsets: &[usize], label: Label) -> usize {
    let at = ir.label_position(label);
    debug_assert!(at < ir.code().len(), "a jump lands on an instruction");
    offsets[at]
}

fn write_u16(chunk: &mut BytecodeChunk, value: u16, pos: &SourcePosition) {
    for byte in value.to_le_bytes() {
        chunk.write(byte, pos);
    }
}

fn write_byte_list(chunk: &mut BytecodeChunk, list: &[u8], pos: &SourcePosition) {
    chunk.write(list.len() as u8, pos);
    for &byte in list {
        chunk.write(byte, pos);
    }
}

/// The encoded byte length of an instruction: its opcode plus operand bytes.
fn encoded_len(inst: &Inst, ir: &Ir) -> usize {
    let op = opcode::opcode_of(inst);
    let mut len = 1;
    for operand in opcode::operands(op) {
        match operand.size() {
            Some(sz) => len += sz,
            None => match *inst {
                Inst::Construct(list) | Inst::FormAnchorPath(_, list) => len += 1 + ir.byte_list(list).len(),
                _ => unreachable!("only Construct and FormAnchorPath have a List operand"),
            },
        }
    }
    len
}

fn encode(inst: &Inst, offsets: &[usize], ir: &Ir, chunk: &mut BytecodeChunk, pos: &SourcePosition) {
    use Inst::*;
    // No poison value in release builds.
    #[cfg(not(debug_assertions))]
    let inst = &match *inst {
        PushUnassigned => PushNull,
        other => other,
    };

    chunk.write(opcode::opcode_of(inst), pos);

    let target_of = |label: Label| landing_offset(ir, offsets, label) as u16;
    let write_jump = |chunk: &mut BytecodeChunk, target: u16| write_u16(chunk, target, pos);

    match *inst {
        Return | ReturnShared | ReturnFac
        | Halt
        | Throw
        | AssertNonNull
        | Pop | DiscardChecked | Dup | Dup2
        | PushNull | PushTrue | PushFalse | PushUnassigned
        | GetIndex
        | GetProperty
        | CopyObject
        | Add | Subtract | Multiply | Divide | Negate | Not
        | LeftShift | RightShift | BitAnd | BitOr | BitXor | BitNot
        | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
        | IsShaped | IsDict | ArrayLen
        | LoadRef | LoadRefForWrite | StoreRef | StoreRefPop
        | SetIndex | SetProperty => {}

        Call(flags, arity) | TailCall(flags, arity) => { chunk.write(flags, pos); chunk.write(arity, pos); }

        BindClosureCaptures(slot, idx) | BindTypeCaptures(slot, idx) => { chunk.write(slot, pos); chunk.write(idx, pos); }

        PushConstant(b) | BuildClosure(b) | PushType(b) | BuildType(b) | BuildClosureUnbound(b) | BuildTypeUnbound(b)
        | LoadGlobal(b)
        | LoadAnchor(b) | StoreAnchor(b) | StoreTempPop(b)
        | LoadCapture(b)
        | LoadLocalForWrite(b) | LoadAnchorForWrite(b) | LoadStepForWrite(b)
        | GetMember(b) | SetMember(b) | RecordAnchorRoot(b)
        | ShareLocal(b) | LoadLocal(b) | StoreLocal(b) | StoreLocalPop(b) | StoreLocalFresh(b) | StoreLocalFreshPop(b)
        | GetField(b) | GetIndexOrNull(b) | HasMember(b) => chunk.write(b, pos),

        PopScope(a, b) => {
            chunk.write(a, pos);
            chunk.write(b, pos);
        }

        Is(id) => write_u16(chunk, id, pos),

        Jump(l)
        | JumpIfFalse(l)
        | JumpIfFalseOrPop(l) | JumpIfTrueOrPop(l)
        | JumpIfCleanOrPop(l) | JumpIfClean(l) | JumpIfBad(l)
        | JumpIfGe(l) | JumpIfGt(l)
        | JumpIfLe(l) | JumpIfLt(l)
        | JumpIfEq(l) | JumpIfNeq(l) => write_jump(chunk, target_of(l)),

        JumpIfGeLocalConst(l, local, c)
        | JumpIfGtLocalConst(l, local, c)
        | JumpIfLeLocalConst(l, local, c)
        | JumpIfLtLocalConst(l, local, c) => {
            write_jump(chunk, target_of(l));
            chunk.write(local, pos);
            chunk.write(c, pos);
        }

        JumpIfIs(l, id) => {
            write_jump(chunk, target_of(l));
            write_u16(chunk, id, pos);
        }

        AddLocalConst(local, c) | SubLocalConst(local, c)
        | IncLocal(local, c) | DecLocal(local, c) | CopyAnchorOut(local, c) => {
            chunk.write(local, pos);
            chunk.write(c, pos);
        }

        PushSlotAnchor(local, witness_set_pool_id) => {
            chunk.write(local, pos);
            write_u16(chunk, witness_set_pool_id, pos);
        }

        CopyAnchorIn(local, flags, witness_set_pool_id) => {
            chunk.write(local, pos);
            chunk.write(flags, pos);
            write_u16(chunk, witness_set_pool_id, pos);
        }

        ArrayMiddle(a, b) | ArrayElem(a, b) => {
            chunk.write(a, pos);
            chunk.write(b, pos);
        }

        InvokeThis(member, flags, arg_count) => {
            chunk.write(member, pos);
            chunk.write(flags, pos);
            chunk.write(arg_count, pos);
        }

        Invoke(member, flags, arg_count, is_dot) => {
            chunk.write(member, pos);
            chunk.write(flags, pos);
            chunk.write(arg_count, pos);
            chunk.write(is_dot, pos);
        }

        Construct(fields_idx) => write_byte_list(chunk, ir.byte_list(fields_idx), pos),

        CheckAnchorRoot(root, root_is_anchor, formed_on) => {
            chunk.write(root, pos);
            chunk.write(root_is_anchor, pos);
            chunk.write(formed_on, pos);
        }

        FormAnchorPath(target, dots) => {
            chunk.write(target, pos);
            write_byte_list(chunk, ir.byte_list(dots), pos);
        }

        BarrierGuard(idx) => {
            write_u16(chunk, idx, pos);
        }

        MemberAdmits(member, idx) => {
            chunk.write(member, pos);
            write_u16(chunk, idx, pos);
        }

        Array(n) | Dict(n) | DictRest(n) | DictRestValues(n) | SetField(n) | SetFieldPop(n) => chunk.write(n, pos),

        SubConstLocal(c, local) | AddConstLocal(c, local) => {
            chunk.write(c, pos);
            chunk.write(local, pos);
        }

        StoreLocalAddLocalLocal(dst, a, b) => {
            chunk.write(dst, pos);
            chunk.write(a, pos);
            chunk.write(b, pos);
        }
    }
}
