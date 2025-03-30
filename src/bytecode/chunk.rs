use std::mem;

use anyhow::bail;

use crate::lexer::SourcePosition;

use super::gc::{Gc, GcTraceable};
use super::objects::Value;
use super::OpCode;



#[derive(Clone)]
pub struct BytecodeChunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub code_pos: Vec<SourcePosition>
}

impl BytecodeChunk {
    pub fn new() -> BytecodeChunk {
        return BytecodeChunk { code: Vec::new(), constants: Vec::new(), code_pos: Vec::new() };
    }

    pub fn write(&mut self, op: OpCode, pos: &SourcePosition) {
        self.code.push(op);
        self.code_pos.push(pos.clone());
    }

    pub fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        self.constants.push(value);
        Ok((self.constants.len() - 1) as u8)
    }
}

impl GcTraceable for BytecodeChunk {
    fn fmt(&self, gc: &Gc) -> String {
        let mut string = String::new();

        macro_rules! push_fmt {
            ($($t:tt)*) => {{
                string.push_str(&format!($($t)*));
            }};
        }

        let mut i = 0;
        while i < self.code.len() {
            let op = self.code[i];
            i += 1;

            push_fmt!("{i}: ");

            match op {
                OpCode::Return => push_fmt!("RET"),
                OpCode::Pop => push_fmt!("POP"),
                OpCode::Call(arg_count) => push_fmt!("CALL {arg_count}"),
                OpCode::Jump(lpos, rpos) => push_fmt!("JUMP <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::JumpIfFalse(lpos, rpos) => push_fmt!("JUMP_F <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::CloseUpvalue(idx) => push_fmt!("CLOSE_UPVALUE <{}>", idx),
                OpCode::PushNull => push_fmt!("NULL"),
                OpCode::PushTrue => push_fmt!("TRUE"),
                OpCode::PushFalse => push_fmt!("FALSE"),
                OpCode::PushConstant(const_idx) => {
                    push_fmt!("CONST {}", self.constants[const_idx as usize].fmt(gc));
                },
                OpCode::PushClosure(idx) => {
                    if let Value::Function(func) = self.constants[idx as usize] {
                        push_fmt!("CLOSURE {}", unsafe { (*func).fmt(gc) });
                    }
                },
                OpCode::PushClass(idx) => {
                    if let Value::Class(class) = self.constants[idx as usize] {
                        push_fmt!("CLASS {}", unsafe { (*class).fmt(gc) });
                    }
                },
                OpCode::Add => push_fmt!("ADD"),
                OpCode::Subtract => push_fmt!("SUB"),
                OpCode::Multiply => push_fmt!("MUL"),
                OpCode::Divide => push_fmt!("DIV"),
                OpCode::Negate => push_fmt!("NEG"),
                OpCode::Equal => push_fmt!("EQ"),
                OpCode::NotEqual => push_fmt!("NEQ"),
                OpCode::LessThan => push_fmt!("LT"),
                OpCode::LessThanEqual => push_fmt!("LTE"),
                OpCode::GreaterThan => push_fmt!("GT"),
                OpCode::GreaterThanEqual => push_fmt!("GTE"),
                OpCode::Not => push_fmt!("NOT"),
                OpCode::GetLocal(loc_idx) => push_fmt!("GET_LOCAL <{}>", loc_idx),
                OpCode::SetLocal(loc_idx) => push_fmt!("SET_LOCAL <{}>", loc_idx),
                OpCode::GetUpvalue(loc_idx) => push_fmt!("GET_UPVAL <{}>", loc_idx),
                OpCode::SetUpvalue(loc_idx) => push_fmt!("SET_UPVAL <{}>", loc_idx),
                OpCode::GetProperty(const_idx) => {
                    push_fmt!("GET_PROP <{}>", self.constants[const_idx as usize].fmt(gc))
                },
                OpCode::SetProperty(const_idx) => {
                    push_fmt!("SET_PROP <{}>", self.constants[const_idx as usize].fmt(gc))
                },
                OpCode::GetPropertyId(member_id) => {
                    push_fmt!("GET_PROP_ID <{}>", member_id)
                },
                OpCode::SetPropertyId(member_id) => {
                    push_fmt!("SET_PROP_ID <{}>", member_id)
                },
                OpCode::GetGlobal(const_idx) => {
                    push_fmt!("GET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                },
                OpCode::SetGlobal(const_idx) => {
                    push_fmt!("SET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                }
            }

            push_fmt!("\n");
        }
        
        string
    }

    fn mark_refs(&self, gc: &mut Gc) {
        for constant in &self.constants {
            constant.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<BytecodeChunk>()
            + self.code.capacity() * mem::size_of::<OpCode>()
            + self.constants.capacity() * mem::size_of::<Value>()
            + self.code_pos.capacity() * mem::size_of::<SourcePosition>()
    }
}