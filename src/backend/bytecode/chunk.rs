use std::mem;

use fnv::{FnvHashMap, FnvHashSet};

use crate::frontend::lex::SourcePosition;
use crate::middle::ir::SourceRole;
use crate::core::gc::{Gc, GcTraceable};
use crate::ast::BuiltinType;
use crate::core::objects::TypeId;
use crate::core::objects::BuiltinLayout;
use crate::core::value::Value;

use super::opcode::{self, OpCode, Operand};



#[derive(Clone)]
pub struct BytecodeChunk {
    /// Each registered object witness declaration and its id, for the types the VM builds itself.
    pub witness_ids: Vec<(TypeId, u16)>,
    /// Each built-in's member layout, by [`BuiltinType::index`].
    pub builtin_layouts: [Option<BuiltinLayout>; BuiltinType::COUNT],
    /// The witness ids each barrier allows, by pool index.
    pub witness_allows: Vec<Box<[u16]>>,
    /// The obligation each survive barrier's guarded positions owe, by pool index. A position
    /// guarded because it is borrowed has no entry.
    pub owed_names: Vec<Box<[(u8, Box<str>)]>>,
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    /// One source position per code byte, not per instruction. Every byte of an instruction usually
    /// carries the same span, but an operand byte may carry a narrower one, which is how a read
    /// part-way through an instruction resolves to the operand it just consumed.
    pub code_pos: Vec<SourcePosition>,
    /// Byte offsets of the checks forcing put back. Empty unless forcing is on, which is what keeps
    /// an ordinary run from paying for the lookup.
    pub elisions: FnvHashSet<usize>,
    /// Extra source positions, keyed by byte offset and role.
    pub source_map: FnvHashMap<(usize, SourceRole), SourcePosition>,
}

impl BytecodeChunk {
    pub fn new() -> BytecodeChunk {
        BytecodeChunk {
            elisions: FnvHashSet::default(),
            witness_ids: Vec::new(),
            builtin_layouts: std::array::from_fn(|_| None),
            witness_allows: Vec::new(),
            owed_names: Vec::new(),
            code: Vec::new(),
            constants: Vec::new(),
            code_pos: Vec::new(),
            source_map: FnvHashMap::default(),
        }
    }

    /// An extra position recorded for the instruction `offset` falls inside.
    pub fn source_at(&self, offset: usize, role: SourceRole) -> Option<&SourcePosition> {
        self.source_map.get(&(offset, role))
    }

    pub fn write(&mut self, op: OpCode, pos: &SourcePosition) {
        self.code.push(op);
        self.code_pos.push(pos.clone());
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
                    Operand::Pool => format!("#{}", short!()),
                    Operand::TypeId => format!("<type {}>", short!()),
                    // A count followed by that many raw bytes.
                    Operand::List => {
                        let count = byte!();
                        let items: Vec<String> = (0..count).map(|_| format!("{}", byte!())).collect();
                        format!("[{}]", items.join(", "))
                    }
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
            + self.witness_ids.capacity() * mem::size_of::<(TypeId, u16)>()
    }
}