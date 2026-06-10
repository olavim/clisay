use crate::parser::Operator;

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
}

macro_rules! opcodes {
    ( $( $name:ident $( ( $( $operand:ident ),* ) )? ),+ $(,)? ) => {
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
    };

    (@consts $idx:expr ; $name:ident $(, $rest:ident )* ) => {
        pub const $name: OpCode = $idx;
        opcodes!(@consts $idx + 1u8 ; $( $rest ),* );
    };
    (@consts $idx:expr ; ) => {};
}

opcodes! {
    CALL(Byte),
    JUMP(Jump),
    JUMP_IF_FALSE(Jump),
    JUMP_IF_GE(Jump),
    JUMP_IF_GT(Jump),
    JUMP_IF_LE(Jump),
    JUMP_IF_LT(Jump),
    JUMP_IF_EQ(Jump),
    JUMP_IF_NEQ(Jump),
    JUMP_IF_GE_LOCAL_CONST(Jump, Local, Const),
    JUMP_IF_GT_LOCAL_CONST(Jump, Local, Const),
    JUMP_IF_LE_LOCAL_CONST(Jump, Local, Const),
    JUMP_IF_LT_LOCAL_CONST(Jump, Local, Const),
    CLOSE_UPVALUE(Byte),
    ARRAY(Byte),
    RETURN,
    THROW,
    PUSH_TRY(Jump),
    POP_TRY,

    // Explicit stack manipulation
    POP,
    PUSH_CONSTANT(Const),
    PUSH_NULL,
    PUSH_TRUE,
    PUSH_FALSE,
    PUSH_CLOSURE(Const),
    PUSH_CLASS(Const),

    // Gets/sets
    GET_GLOBAL(Const),
    SET_GLOBAL(Const),
    GET_LOCAL(Local),
    SET_LOCAL(Local),
    SET_LOCAL_POP(Local),
    SET_LOCAL_ADD_LOCAL_LOCAL(Local, Local, Local),
    GET_UPVALUE(Byte),
    SET_UPVALUE(Byte),
    SET_UPVALUE_POP(Byte),
    GET_INDEX,
    SET_INDEX,
    GET_PROPERTY_ID(Byte),
    SET_PROPERTY_ID(Byte),
    SET_PROPERTY_ID_POP(Byte),

    // Arithmetic
    ADD,
    ADD_LOCAL_CONST(Local, Const),
    SUBTRACT,
    SUB_LOCAL_CONST(Local, Const),
    SUB_CONST_LOCAL(Const, Local),
    MULTIPLY,
    DIVIDE,
    NEGATE,
    LEFT_SHIFT,
    RIGHT_SHIFT,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    BIT_NOT,

    // Logical
    EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
    NOT,
    AND,
    OR,
}

pub fn from_operator(op: &Operator) -> OpCode {
    return match op {
        Operator::Add => ADD,
        Operator::Subtract => SUBTRACT,
        Operator::Multiply => MULTIPLY,
        Operator::Divide => DIVIDE,
        Operator::LeftShift => LEFT_SHIFT,
        Operator::RightShift => RIGHT_SHIFT,
        Operator::LessThan => LESS_THAN,
        Operator::LessThanEqual => LESS_THAN_EQUAL,
        Operator::GreaterThan => GREATER_THAN,
        Operator::GreaterThanEqual => GREATER_THAN_EQUAL,
        Operator::LogicalEqual => EQUAL,
        Operator::LogicalNotEqual => NOT_EQUAL,
        Operator::LogicalAnd => AND,
        Operator::LogicalOr => OR,
        Operator::BitAnd => BIT_AND,
        Operator::BitOr => BIT_OR,
        Operator::BitXor => BIT_XOR,
        Operator::BitNot => BIT_NOT,
        Operator::Negate => NEGATE,
        _ => unreachable!("Invalid operator")
    };
}