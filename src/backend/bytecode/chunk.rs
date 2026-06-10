use std::mem;

use anyhow::bail;

use crate::lexer::SourcePosition;
use crate::core::gc::{Gc, GcTraceable};
use crate::core::value::Value;

use super::opcode::{self, OpCode, Operand};



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
        let mut pos = 0;

        macro_rules! byte {
            () => {{ pos += 1; self.code[pos - 1] }};
        }
        macro_rules! short {
            () => {{ pos += 2; (self.code[pos - 2] as u16) | ((self.code[pos - 1] as u16) << 8) }};
        }

        // Generic disassembly: print the opcode's mnemonic, then render each
        // operand per the layout declared alongside the opcode in `opcode.rs`.
        // Adding or changing an opcode never requires touching this loop.
        while pos < self.code.len() {
            let op_pos = pos;
            let op = byte!();
            string.push_str(&format!("{op_pos}: {}", opcode::name(op)));

            for operand in opcode::operands(op) {
                let rendered = match operand {
                    Operand::Byte => format!("<{}>", byte!()),
                    Operand::Local => format!("L{}", byte!()),
                    Operand::Const => self.constants[byte!() as usize].fmt(),
                    Operand::Jump => format!("<{}>", short!()),
                };
                string.push(' ');
                string.push_str(&rendered);
            }

            if pos < self.code.len() {
                string.push('\n');
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