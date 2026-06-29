pub type OpCode = u8;

/// One operand of an instruction. The VM reads operands directly in its dispatch
/// loop; this metadata exists so the disassembler can render any opcode without a
/// per-opcode arm.
#[derive(Clone, Copy)]
pub enum Operand {
    /// A raw `u8` (upvalue index, member id, arg count, …).
    Byte,
    /// A `u8` local slot.
    Local,
    /// A `u8` index into the constant pool.
    Const,
    /// A `u16` bytecode offset (jump target).
    Jump,
    /// A `u8` count followed by that many raw bytes. Variable length; the
    /// disassembler reads the count then the bytes.
    List,
}

impl Operand {
    /// The fixed number of bytes this operand occupies, or `None` for a
    /// variable-length operand (`List`), which the reader sizes from its count.
    pub fn size(&self) -> Option<usize> {
        match self {
            Operand::Byte | Operand::Local | Operand::Const => Some(1),
            Operand::Jump => Some(2),
            Operand::List => None,
        }
    }
}

macro_rules! opcodes {
    ( $( $inst:ident => $name:ident $( ( $( $operand:ident ),* ) )? ),+ $(,)? ) => {
        opcodes!(@consts 0u8 ; $( $name ),+ );

        /// The opcode's identifier, used as its disassembly mnemonic.
        pub fn name(op: OpCode) -> &'static str {
            match op {
                $( $name => stringify!($name), )+
                _ => "UNKNOWN"
            }
        }

        /// The opcode's operand layout, used to disassemble it generically.
        pub fn operands(op: OpCode) -> &'static [Operand] {
            match op {
                $( $name => &[ $( $( Operand::$operand ),* )? ], )+
                _ => &[]
            }
        }

        pub fn opcode_of(inst: &crate::middle::ir::Inst) -> OpCode {
            match inst {
                $( crate::middle::ir::Inst::$inst { .. } => $name, )+
            }
        }
    };

    (@consts $idx:expr ; $name:ident $(, $rest:ident )* ) => {
        pub const $name: OpCode = $idx;
        opcodes!(@consts $idx + 1u8 ; $( $rest ),* );
    };
    (@consts $idx:expr ; ) => {};
}

opcodes! {
    Call => CALL(Byte),
    Construct => CONSTRUCT(List, Byte),
    Invoke => INVOKE(Const, Byte),
    Jump => JUMP(Jump),
    JumpIfFalse => JUMP_IF_FALSE(Jump),
    JumpIfFalseOrPop => JUMP_IF_FALSE_OR_POP(Jump),
    JumpIfTrueOrPop => JUMP_IF_TRUE_OR_POP(Jump),
    JumpIfNotNullOrPop => JUMP_IF_NOT_NULL_OR_POP(Jump),
    JumpIfNull => JUMP_IF_NULL(Jump),
    JumpIfGe => JUMP_IF_GE(Jump),
    JumpIfGt => JUMP_IF_GT(Jump),
    JumpIfLe => JUMP_IF_LE(Jump),
    JumpIfLt => JUMP_IF_LT(Jump),
    JumpIfEq => JUMP_IF_EQ(Jump),
    JumpIfNeq => JUMP_IF_NEQ(Jump),
    JumpIfGeLocalConst => JUMP_IF_GE_LOCAL_CONST(Jump, Local, Const),
    JumpIfGtLocalConst => JUMP_IF_GT_LOCAL_CONST(Jump, Local, Const),
    JumpIfLeLocalConst => JUMP_IF_LE_LOCAL_CONST(Jump, Local, Const),
    JumpIfLtLocalConst => JUMP_IF_LT_LOCAL_CONST(Jump, Local, Const),
    CloseUpvalue => CLOSE_UPVALUE(Byte),
    Array => ARRAY(Byte),
    Dict => DICT(Byte),
    Return => RETURN,
    Throw => THROW,
    PushTry => PUSH_TRY(Jump),
    PopTry => POP_TRY,
    AssertNonNull => ASSERT_NON_NULL,

    // Explicit stack manipulation
    Pop => POP,
    Dup => DUP,
    PushConstant => PUSH_CONSTANT(Const),
    PushNull => PUSH_NULL,
    PushTrue => PUSH_TRUE,
    PushFalse => PUSH_FALSE,
    PushClosure => PUSH_CLOSURE(Const),
    PushType => PUSH_TYPE(Const),

    // Variable bindings (local/upvalue/global)
    LoadGlobal => LOAD_GLOBAL(Const),
    LoadLocal => LOAD_LOCAL(Local),
    StoreLocal => STORE_LOCAL(Local),
    StoreLocalPop => STORE_LOCAL_POP(Local),
    StoreLocalAddLocalLocal => STORE_LOCAL_ADD_LOCAL_LOCAL(Local, Local, Local),
    LoadUpvalue => LOAD_UPVALUE(Byte),
    StoreUpvalue => STORE_UPVALUE(Byte),
    StoreUpvaluePop => STORE_UPVALUE_POP(Byte),

    // Object access: by key (`[]`), by name (`.`), or by resolved member id
    GetIndex => GET_INDEX,
    SetIndex => SET_INDEX,
    GetIndexOrNull => GET_INDEX_OR_NULL(Const),
    GetProperty => GET_PROPERTY,
    SetProperty => SET_PROPERTY,
    GetField => GET_FIELD(Byte),
    SetField => SET_FIELD(Byte),
    SetFieldPop => SET_FIELD_POP(Byte),

    // Arithmetic
    Add => ADD,
    AddLocalConst => ADD_LOCAL_CONST(Local, Const),
    AddConstLocal => ADD_CONST_LOCAL(Const, Local),
    Subtract => SUBTRACT,
    SubLocalConst => SUB_LOCAL_CONST(Local, Const),
    SubConstLocal => SUB_CONST_LOCAL(Const, Local),
    Multiply => MULTIPLY,
    Divide => DIVIDE,
    Negate => NEGATE,
    Not => NOT,
    LeftShift => LEFT_SHIFT,
    RightShift => RIGHT_SHIFT,
    BitAnd => BIT_AND,
    BitOr => BIT_OR,
    BitXor => BIT_XOR,
    BitNot => BIT_NOT,

    // Logical
    Equal => EQUAL,
    NotEqual => NOT_EQUAL,
    LessThan => LESS_THAN,
    LessThanEqual => LESS_THAN_EQUAL,
    GreaterThan => GREATER_THAN,
    GreaterThanEqual => GREATER_THAN_EQUAL,
    Is => IS(Const),
    HasMember => HAS_MEMBER(Const),
    ArrayLen => ARRAY_LEN,
}