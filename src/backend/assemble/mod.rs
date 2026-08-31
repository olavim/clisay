//! Lowers the middle-end `Ir` into bytecode.

use anyhow::bail;

use crate::ast::BuiltinType;
use crate::backend::bytecode::chunk::BytecodeChunk;
use crate::backend::bytecode::opcode;
use crate::core::objects::TypeMember;
use crate::frontend::lex::SourcePosition;
use crate::middle::ir::{TO_FRAME_END, SlotAccepts, Inst, Ir, Label};

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

    // Finalise function entry points now that byte offsets are known.
    for &(func, body_label) in ir.fn_entries() {
        unsafe { (*func).ip_start = offsets[ir.label_target(body_label)]; }
    }

    let mut chunk = BytecodeChunk::new();
    chunk.witness_ids = ir.witness_ids().to_vec();
    if ir.builtin_layouts().iter().any(Option::is_none) {
        bail!("a built-in type reached assembly with no layout");
    }

    // `Err`'s native factory writes field 0 and nothing else, so its declaration has to match.
    let err = ir.builtin_layouts()[BuiltinType::Err.index()].as_ref().expect("every built-in layout is present");
    if err.field_count != 1 || !err.members.iter().any(|(name, m)| name == "value" && matches!(m, TypeMember::Field(0))) {
        bail!("Err's native factory writes one field, so its declaration must have exactly `value` at id 0");
    }

    chunk.witness_allows = ir.witness_allows().to_vec();
    chunk.param_accepts = ir.param_accepts().to_vec();
    chunk.slot_accepts = ir.slot_accepts().iter()
        .map(|body| body.iter().map(|e| SlotAccepts { from: offsets[e.from], to: if e.to == TO_FRAME_END { TO_FRAME_END } else { offsets[e.to] }, ..*e }).collect())
        .collect();
    chunk.owed_names = ir.owed_names().to_vec();
    chunk.constants = ir.constants().to_vec();
    chunk.elisions = ir.elisions().iter().map(|&idx| offsets[idx]).collect();
    for (&(idx, role), pos) in ir.source_map() {
        let end = offsets.get(idx + 1).copied().unwrap_or(size);
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

/// Writes a barrier's guarded positions as a count byte followed by one byte each.
fn write_positions(ir: &Ir, chunk: &mut BytecodeChunk, idx: u16, pos: &SourcePosition) {
    let positions = ir.survive_positions(idx);
    chunk.write(positions.len() as u8, pos);
    for (p, arg_pos) in positions {
        chunk.write(*p, arg_pos);
    }
}

/// Writes a declaration id as two little-endian bytes.
fn write_u16(chunk: &mut BytecodeChunk, value: u16, pos: &SourcePosition) {
    for byte in value.to_le_bytes() {
        chunk.write(byte, pos);
    }
}

/// The encoded byte length of an instruction: its opcode plus operand bytes. A
/// variable-length `List` operand is sized from the instruction's own data.
fn encoded_len(inst: &Inst, ir: &Ir) -> usize {
    let op = opcode::opcode_of(inst);
    let mut len = 1;
    for operand in opcode::operands(op) {
        match operand.size() {
            Some(sz) => len += sz,
            None => match *inst {
                Inst::Construct(fields_idx, _) => len += 1 + ir.construct_fields(fields_idx).len(), // count byte + ids
                Inst::AssertNoRetain(_, _, idx) => len += 1 + ir.survive_positions(idx).len(), // count byte + positions
                _ => unreachable!("only Construct and AssertNoRetain have a List operand"),
            },
        }
    }
    len
}

fn encode(inst: &Inst, offsets: &[usize], ir: &Ir, chunk: &mut BytecodeChunk, pos: &SourcePosition) {
    use Inst::*;

    chunk.write(opcode::opcode_of(inst), pos);

    let target_of = |label: Label| offsets[ir.label_target(label)] as u16;
    let write_jump = |chunk: &mut BytecodeChunk, target: u16| write_u16(chunk, target, pos);

    match *inst {
        Return | ReturnFac
        | Halt
        | Throw
        | PopTry
        | AssertNonNull
        | AssertImmutable
        | StashRoot
        | Pop | Dup | Dup2
        | PushNull | PushTrue | PushFalse
        | GetIndex
        | GetProperty
        | Add | Subtract | Multiply | Divide | Negate | Not
        | LeftShift | RightShift | BitAnd | BitOr | BitXor | BitNot
        | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
        | IsShaped | ArrayLen
        | Mut | SealCheck => {}

        Call(b) | CallMut(b) | TailCall(b)
        | PushConstant(b) | PushClosure(b) | PushType(b) | BuildType(b)
        | LoadGlobal(b) | LoadLocal(b) | StoreLocal(b) | StoreLocalPop(b)
        | CloseUpvalue(b) | CloseSlotUpvalue(b) | LoadUpvalue(b) | StoreUpvalue(b) | StoreUpvaluePop(b)
        | GetField(b)
        | TransferWriteOwnership(b) | TransferWriteOwnershipUp(b) | TransferWriteOwnershipAt(b)
        | ReleaseWriteOwnership(b)
        | HasMember(b) | GetIndexOrNull(b) => chunk.write(b, pos),

        PopScope(count, depth) => {
            chunk.write(count, pos);
            chunk.write(depth, pos);
        }

        Is(id) => write_u16(chunk, id, pos),

        Jump(l)
        | JumpIfFalse(l)
        | JumpIfFalseOrPop(l) | JumpIfTrueOrPop(l)
        | JumpIfNotNullOrPop(l) | JumpIfNull(l) | JumpIfClean(l) | JumpIfBad(l)
        | JumpIfGe(l) | JumpIfGt(l)
        | JumpIfLe(l) | JumpIfLt(l)
        | JumpIfEq(l) | JumpIfNeq(l)
        | PushTry(l) | PushDeferTry(l) => write_jump(chunk, target_of(l)),

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
        | IncLocal(local, c) | DecLocal(local, c) => {
            chunk.write(local, pos);
            chunk.write(c, pos);
        }

        ArrayMiddle(a, b) | ArrayElem(a, b) => {
            chunk.write(a, pos);
            chunk.write(b, pos);
        }

        AssertNoRetain(arg_count, owed_idx, idx) => {
            chunk.write(arg_count, pos);
            write_u16(chunk, owed_idx, pos);
            write_positions(ir, chunk, idx, pos);
        }

        InvokeThis(member, arg_count, kind, operand) => {
            chunk.write(member, pos);
            chunk.write(arg_count, pos);
            chunk.write(kind, pos);
            chunk.write(operand, pos);
        }

        Invoke(member, arg_count, kind, operand, is_dot) => {
            chunk.write(member, pos);
            chunk.write(arg_count, pos);
            chunk.write(kind, pos);
            chunk.write(operand, pos);
            chunk.write(is_dot, pos);
        }

        Construct(fields_idx, seal) => {
            let fields = ir.construct_fields(fields_idx);
            chunk.write(fields.len() as u8, pos);
            for &id in fields {
                chunk.write(id, pos);
            }
            chunk.write(seal, pos);
        }

        BarrierGuard(idx) => {
            write_u16(chunk, idx, pos);
        }

        MemberAdmits(member, idx) => {
            chunk.write(member, pos);
            write_u16(chunk, idx, pos);
        }

        Array(a, b) | Dict(a, b) => {
            chunk.write(a, pos);
            chunk.write(b, pos);
        }

        SetIndex(kind, operand) | SetProperty(kind, operand) => {
            chunk.write(kind, pos);
            chunk.write(operand, pos);
        }

        SetField(member, kind, operand) | SetFieldPop(member, kind, operand) => {
            chunk.write(member, pos);
            chunk.write(kind, pos);
            chunk.write(operand, pos);
        }

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
