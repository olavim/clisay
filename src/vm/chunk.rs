use std::mem;

use anyhow::bail;

use crate::lexer::SourcePosition;
use crate::vm::opcode;

use super::gc::{Gc, GcTraceable};
use super::opcode::OpCode;
use super::value::Value;



#[derive(Clone)]
pub struct BytecodeChunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub code_pos: Vec<SourcePosition>
}

impl BytecodeChunk {
    pub fn new() -> BytecodeChunk {
        BytecodeChunk {
            code: Vec::new(),
            constants: Vec::new(), 
            code_pos: Vec::new()
        }
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
    fn fmt(&self) -> String {
        let mut string = String::new();

        macro_rules! push_fmt {
            ($($t:tt)*) => {{
                string.push_str(&format!($($t)*));
            }};
        }

        let mut pos = 0;

        macro_rules! byte {
            () => {{
                pos += 1;
                self.code[pos - 1]
            }};
        }

        macro_rules! short {
            () => {{
                pos += 2;
                (self.code[pos - 2] as u16) | ((self.code[pos - 1] as u16) << 8)
            }};
        }

        while pos < self.code.len() {
            push_fmt!("{pos}: ");
            let op = byte!();

            match op {
                opcode::RETURN => push_fmt!("RETURN"),
                opcode::THROW => push_fmt!("THROW"),
                opcode::PUSH_TRY => push_fmt!("TRY {}", short!()),
                opcode::POP_TRY => push_fmt!("POP_TRY"),
                opcode::POP => push_fmt!("POP"),
                opcode::CALL => push_fmt!("CALL {}", byte!()),
                opcode::JUMP => push_fmt!("JUMP <{}>", short!()),
                opcode::JUMP_IF_FALSE => push_fmt!("JUMP_F <{}>", short!()),
                opcode::CLOSE_UPVALUE => push_fmt!("CLOSE_UPVALUE <{}>", byte!()),
                opcode::ARRAY => push_fmt!("ARRAY <{}>", byte!()),
                opcode::PUSH_NULL => push_fmt!("NULL"),
                opcode::PUSH_TRUE => push_fmt!("TRUE"),
                opcode::PUSH_FALSE => push_fmt!("FALSE"),
                opcode::PUSH_CONSTANT => {
                    push_fmt!("CONST {}", self.constants[byte!() as usize].fmt());
                },
                opcode::PUSH_CLOSURE => {
                    let func_const = self.constants[byte!() as usize];
                    let func = func_const.as_object().as_function_ptr();
                    push_fmt!("CLOSURE {}", unsafe { (*func).fmt() });
                },
                opcode::PUSH_CLASS => {
                    let class_const = self.constants[byte!() as usize];
                    let class = class_const.as_object().as_class_ptr();
                    push_fmt!("CLASS {}", unsafe { (*class).fmt() });
                },
                opcode::ADD => push_fmt!("ADD"),
                opcode::SUBTRACT => push_fmt!("SUB"),
                opcode::MULTIPLY => push_fmt!("MUL"),
                opcode::DIVIDE => push_fmt!("DIV"),
                opcode::NEGATE => push_fmt!("NEG"),
                opcode::EQUAL => push_fmt!("EQ"),
                opcode::NOT_EQUAL => push_fmt!("NEQ"),
                opcode::LESS_THAN => push_fmt!("LT"),
                opcode::LESS_THAN_EQUAL => push_fmt!("LTE"),
                opcode::GREATER_THAN => push_fmt!("GT"),
                opcode::GREATER_THAN_EQUAL => push_fmt!("GTE"),
                opcode::NOT => push_fmt!("NOT"),
                opcode::LEFT_SHIFT => push_fmt!("LSH"),
                opcode::RIGHT_SHIFT => push_fmt!("RSH"),
                opcode::BIT_AND => push_fmt!("AND"),
                opcode::BIT_OR => push_fmt!("OR"),
                opcode::BIT_XOR => push_fmt!("XOR"),
                opcode::BIT_NOT => push_fmt!("BIT_NOT"),
                opcode::AND => push_fmt!("AND"),
                opcode::OR => push_fmt!("OR"),
                opcode::GET_LOCAL => push_fmt!("GET_LOCAL <{}>", byte!()),
                opcode::SET_LOCAL => push_fmt!("SET_LOCAL <{}>", byte!()),
                opcode::GET_UPVALUE => push_fmt!("GET_UPVAL <{}>", byte!()),
                opcode::SET_UPVALUE => push_fmt!("SET_UPVAL <{}>", byte!()),
                opcode::GET_INDEX => push_fmt!("GET_INDEX"),
                opcode::SET_INDEX => push_fmt!("SET_INDEX"),
                opcode::GET_PROPERTY_ID => {
                    push_fmt!("GET_PROP_ID <{}>", byte!())
                },
                opcode::SET_PROPERTY_ID => {
                    push_fmt!("SET_PROP_ID <{}>", byte!())
                },
                opcode::GET_GLOBAL => {
                    push_fmt!("GET_GLOBAL {}", self.constants[byte!() as usize].fmt());
                },
                opcode::SET_GLOBAL => {
                    push_fmt!("SET_GLOBAL {}", self.constants[byte!() as usize].fmt());
                },
                _ => panic!("Unknown opcode {}", op)
            }

            if pos < self.code.len() {
                push_fmt!("\n");
            }
        }
        
        string
    }

    fn mark(&self, gc: &mut Gc) {
        for constant in &self.constants {
            constant.mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<BytecodeChunk>()
            + self.code.capacity() * mem::size_of::<OpCode>()
            + self.constants.capacity() * mem::size_of::<Value>()
            + self.code_pos.capacity() * mem::size_of::<SourcePosition>()
    }
}