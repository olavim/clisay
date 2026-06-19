//! Lowers the middle-end `Ir` into bytecode.

use anyhow::bail;

use crate::backend::bytecode::chunk::BytecodeChunk;
use crate::backend::bytecode::opcode;
use crate::frontend::lex::SourcePosition;
use crate::middle::ir::{Inst, Ir, Label};

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
    for &(func, body_label) in ir.entries() {
        unsafe { (*func).ip_start = offsets[ir.label_target(body_label)]; }
    }

    let mut chunk = BytecodeChunk::new();
    chunk.constants = ir.constants().to_vec();
    for (i, inst) in ir.code().iter().enumerate() {
        encode(inst, &offsets, &ir, &mut chunk, &ir.positions()[i]);
    }

    Ok(chunk)
}

/// The encoded byte length of an instruction: its opcode plus operand bytes. A
/// variable-length `List` operand is sized from the instruction's own data.
fn encoded_len(inst: &Inst, ir: &Ir) -> usize {
    let op = opcode::opcode_of(inst);
    let mut len = 1;
    for operand in opcode::operands(op) {
        match operand.size() {
            Some(sz) => len += sz,
            None => {
                let Inst::Construct(fields_idx, _) = *inst else { unreachable!("only Construct has a List operand") };
                len += 1 + ir.construct_fields(fields_idx).len(); // count byte + ids
            }
        }
    }
    len
}

fn encode(inst: &Inst, offsets: &[usize], ir: &Ir, chunk: &mut BytecodeChunk, pos: &SourcePosition) {
    use Inst::*;

    chunk.write(opcode::opcode_of(inst), pos);

    let target_of = |label: Label| offsets[ir.label_target(label)] as u16;
    let write_jump = |chunk: &mut BytecodeChunk, target: u16| {
        chunk.write(target as u8, pos);
        chunk.write((target >> 8) as u8, pos);
    };

    match *inst {
        Return
        | Throw
        | PopTry
        | Pop
        | PushNull | PushTrue | PushFalse
        | GetIndex | SetIndex
        | GetProperty | SetProperty
        | Add | Subtract | Multiply | Divide | Negate | Not
        | LeftShift | RightShift | BitAnd | BitOr | BitXor | BitNot
        | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual => {}

        Call(b)
        | Array(b)
        | Dict(b)
        | PushConstant(b) | PushClosure(b) | PushClass(b)
        | GetGlobal(b) | GetLocal(b) | SetLocal(b) | SetLocalPop(b)
        | CloseUpvalue(b) | GetUpvalue(b) | SetUpvalue(b) | SetUpvaluePop(b)
        | GetPropertyId(b) | SetPropertyId(b) | SetPropertyIdPop(b)
        | Is(b) => chunk.write(b, pos),

        Jump(l)
        | JumpIfFalse(l)
        | JumpIfFalseOrPop(l) | JumpIfTrueOrPop(l)
        | JumpIfGe(l) | JumpIfGt(l)
        | JumpIfLe(l) | JumpIfLt(l)
        | JumpIfEq(l) | JumpIfNeq(l)
        | PushTry(l) => write_jump(chunk, target_of(l)),

        JumpIfGeLocalConst(l, local, c)
        | JumpIfGtLocalConst(l, local, c)
        | JumpIfLeLocalConst(l, local, c)
        | JumpIfLtLocalConst(l, local, c) => {
            write_jump(chunk, target_of(l));
            chunk.write(local, pos);
            chunk.write(c, pos);
        }

        AddLocalConst(local, c) | SubLocalConst(local, c) => {
            chunk.write(local, pos);
            chunk.write(c, pos);
        }

        Invoke(name, arg_count) => {
            chunk.write(name, pos);
            chunk.write(arg_count, pos);
        }

        Construct(fields_idx, arg_count) => {
            let fields = ir.construct_fields(fields_idx);
            chunk.write(fields.len() as u8, pos);
            for &id in fields {
                chunk.write(id, pos);
            }
            chunk.write(arg_count, pos);
        }

        SubConstLocal(c, local) | AddConstLocal(c, local) => {
            chunk.write(c, pos);
            chunk.write(local, pos);
        }

        SetLocalAddLocalLocal(dst, a, b) => {
            chunk.write(dst, pos);
            chunk.write(a, pos);
            chunk.write(b, pos);
        }
    }
}
