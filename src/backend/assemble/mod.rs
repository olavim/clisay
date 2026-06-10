//! Lowers the middle-end `Ir` into bytecode.

use anyhow::bail;

use crate::backend::bytecode::chunk::BytecodeChunk;
use crate::backend::bytecode::opcode::{self, OpCode, Operand};
use crate::frontend::lex::SourcePosition;
use crate::middle::ir::{Inst, Ir, Label};

pub fn assemble(ir: Ir) -> Result<BytecodeChunk, anyhow::Error> {
    let mut offsets = Vec::with_capacity(ir.code().len());
    let mut size = 0usize;
    for inst in ir.code() {
        offsets.push(size);
        size += encoded_len(opcode_of(inst));
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

/// The encoded byte length of an instruction: its opcode plus operand bytes.
fn encoded_len(op: OpCode) -> usize {
    1 + opcode::operands(op).iter().map(Operand::size).sum::<usize>()
}

fn encode(inst: &Inst, offsets: &[usize], ir: &Ir, chunk: &mut BytecodeChunk, pos: &SourcePosition) {
    use Inst::*;

    chunk.write(opcode_of(inst), pos);

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
        | Add | Subtract | Multiply | Divide | Negate
        | LeftShift | RightShift | BitAnd | BitOr | BitXor | BitNot
        | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
        | And | Or => {}

        Call(b)
        | Array(b)
        | PushConstant(b) | PushClosure(b) | PushClass(b)
        | GetGlobal(b) | SetGlobal(b) | GetLocal(b) | SetLocal(b) | SetLocalPop(b)
        | CloseUpvalue(b) | GetUpvalue(b) | SetUpvalue(b) | SetUpvaluePop(b)
        | GetPropertyId(b) | SetPropertyId(b) | SetPropertyIdPop(b) => chunk.write(b, pos),

        Jump(l)
        | JumpIfFalse(l)
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

        SubConstLocal(c, local) => {
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

/// The opcode each instruction encodes to.
fn opcode_of(inst: &Inst) -> OpCode {
    use Inst::*;
    match inst {
        Call(_) => opcode::CALL,
        Jump(_) => opcode::JUMP,
        JumpIfFalse(_) => opcode::JUMP_IF_FALSE,
        JumpIfGe(_) => opcode::JUMP_IF_GE,
        JumpIfGt(_) => opcode::JUMP_IF_GT,
        JumpIfLe(_) => opcode::JUMP_IF_LE,
        JumpIfLt(_) => opcode::JUMP_IF_LT,
        JumpIfEq(_) => opcode::JUMP_IF_EQ,
        JumpIfNeq(_) => opcode::JUMP_IF_NEQ,
        JumpIfGeLocalConst(..) => opcode::JUMP_IF_GE_LOCAL_CONST,
        JumpIfGtLocalConst(..) => opcode::JUMP_IF_GT_LOCAL_CONST,
        JumpIfLeLocalConst(..) => opcode::JUMP_IF_LE_LOCAL_CONST,
        JumpIfLtLocalConst(..) => opcode::JUMP_IF_LT_LOCAL_CONST,
        Return => opcode::RETURN,
        Throw => opcode::THROW,
        PushTry(_) => opcode::PUSH_TRY,
        PopTry => opcode::POP_TRY,
        Pop => opcode::POP,
        PushConstant(_) => opcode::PUSH_CONSTANT,
        PushNull => opcode::PUSH_NULL,
        PushTrue => opcode::PUSH_TRUE,
        PushFalse => opcode::PUSH_FALSE,
        PushClosure(_) => opcode::PUSH_CLOSURE,
        PushClass(_) => opcode::PUSH_CLASS,
        GetGlobal(_) => opcode::GET_GLOBAL,
        SetGlobal(_) => opcode::SET_GLOBAL,
        GetLocal(_) => opcode::GET_LOCAL,
        SetLocal(_) => opcode::SET_LOCAL,
        SetLocalPop(_) => opcode::SET_LOCAL_POP,
        SetLocalAddLocalLocal(..) => opcode::SET_LOCAL_ADD_LOCAL_LOCAL,
        GetUpvalue(_) => opcode::GET_UPVALUE,
        SetUpvalue(_) => opcode::SET_UPVALUE,
        SetUpvaluePop(_) => opcode::SET_UPVALUE_POP,
        CloseUpvalue(_) => opcode::CLOSE_UPVALUE,
        GetIndex => opcode::GET_INDEX,
        SetIndex => opcode::SET_INDEX,
        GetPropertyId(_) => opcode::GET_PROPERTY_ID,
        SetPropertyId(_) => opcode::SET_PROPERTY_ID,
        SetPropertyIdPop(_) => opcode::SET_PROPERTY_ID_POP,
        Array(_) => opcode::ARRAY,
        Add => opcode::ADD,
        AddLocalConst(..) => opcode::ADD_LOCAL_CONST,
        Subtract => opcode::SUBTRACT,
        SubLocalConst(..) => opcode::SUB_LOCAL_CONST,
        SubConstLocal(..) => opcode::SUB_CONST_LOCAL,
        Multiply => opcode::MULTIPLY,
        Divide => opcode::DIVIDE,
        Negate => opcode::NEGATE,
        LeftShift => opcode::LEFT_SHIFT,
        RightShift => opcode::RIGHT_SHIFT,
        BitAnd => opcode::BIT_AND,
        BitOr => opcode::BIT_OR,
        BitXor => opcode::BIT_XOR,
        BitNot => opcode::BIT_NOT,
        Equal => opcode::EQUAL,
        NotEqual => opcode::NOT_EQUAL,
        LessThan => opcode::LESS_THAN,
        LessThanEqual => opcode::LESS_THAN_EQUAL,
        GreaterThan => opcode::GREATER_THAN,
        GreaterThanEqual => opcode::GREATER_THAN_EQUAL,
        And => opcode::AND,
        Or => opcode::OR,
    }
}
