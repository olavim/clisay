use std::mem;

use fnv::{FnvHashMap, FnvHashSet};

use crate::frontend::lex::SourcePosition;
use crate::middle::ir::{self, SlotWitnessSet, SourceRole};
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
    /// Each witness set: the witness ids a slot or barrier allows.
    pub witness_set_pool: Vec<Box<[u16]>>,
    /// Each callable's parameter list: a `witness_set_pool` id per parameter, with the anchor flag
    /// in the top bit.
    pub param_list_pool: Vec<Box<[u16]>>,
    /// Each frame's slot witness sets, the script's first.
    pub slot_witness_set_pool: Vec<Box<[SlotWitnessSet]>>,
    /// Each frame's anchor parameters as anchor slot and copy slot, by `slot_witness_set_pool` id.
    pub anchor_params: Vec<Box<[(u8, u8)]>>,
    pub handlers: Vec<HandlerRange>,
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    /// One source position per code byte.
    pub code_pos: Vec<SourcePosition>,
    /// The last byte of each check that check-forcing put back.
    pub forced_check_ends: FnvHashSet<usize>,
    /// Extra source positions, keyed by byte offset and role.
    pub source_map: FnvHashMap<(usize, SourceRole), SourcePosition>,
}

/// The bytes `start..end` throw to the handler at `handler`.
#[derive(Clone, Copy)]
pub struct HandlerRange {
    pub start: u16,
    pub end: u16,
    pub handler: u16,
    pub frame_stack_height: u16,
}

impl BytecodeChunk {
    pub fn handler_at(&self, offset: usize) -> Option<HandlerRange> {
        let after = self.handlers.partition_point(|range| range.start as usize <= offset);
        let range = *self.handlers.get(after.checked_sub(1)?)?;
        (offset < range.end as usize).then_some(range)
    }

    pub fn new() -> BytecodeChunk {
        BytecodeChunk {
            forced_check_ends: FnvHashSet::default(),
            witness_ids: Vec::new(),
            builtin_layouts: std::array::from_fn(|_| None),
            witness_set_pool: Vec::new(),
            param_list_pool: Vec::new(),
            slot_witness_set_pool: Vec::new(),
            anchor_params: Vec::new(),
            handlers: Vec::new(),
            code: Vec::new(),
            constants: Vec::new(),
            code_pos: Vec::new(),
            source_map: FnvHashMap::default(),
        }
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
        let mut omitted = 0;

        macro_rules! byte {
            () => {{ pos += 1; self.code[pos - 1] }};
        }
        macro_rules! short {
            () => {{ pos += 2; (self.code[pos - 2] as u16) | ((self.code[pos - 1] as u16) << 8) }};
        }

        while pos < self.code.len() {
            let op_pos = pos;
            let line_start = string.len();
            let op = byte!();
            // The prelude is the same for every program, so a disassembly leaves it out.
            let from_prelude = self.code_pos.get(op_pos)
                .is_some_and(SourcePosition::is_vm_source);
            string.push_str(&format!("{}: {}", op_pos - omitted, opcode::name(op)));

            for operand in opcode::operands(op) {
                let rendered = match operand {
                    Operand::Byte => format!("<{}>", byte!()),
                    Operand::CallFlags => {
                        let flags = byte!();
                        let mut marks: Vec<&str> = Vec::new();
                        if flags & ir::CALL_ARGS_SETTLED != 0 { marks.push("settled"); }
                        else if flags & ir::CALL_KINDS_PROVEN != 0 { marks.push("proven"); }
                        if flags & ir::CALL_WANTS_VALUE == 0 { marks.push("discards"); }
                        if flags & ir::CALL_PASSES_ANCHOR_RECEIVER != 0 { marks.push("anchor"); }
                        if flags & ir::CALL_TESTS_CALLEE != 0 { marks.push("tested"); }
                        format!("<{}>", marks.join(" "))
                    },
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

            if from_prelude {
                string.truncate(line_start);
                omitted += pos - op_pos;
                continue;
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