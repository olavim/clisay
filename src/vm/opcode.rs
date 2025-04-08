pub type OpCode = u8;

macro_rules! ops_ {
    // The pattern for a single `eval`
    ($inc:expr, $op:ident) => {
        pub const $op: OpCode = $inc;
    };

    // Decompose multiple `eval`s recursively
    ($inc:expr, $op:ident, $($ops:ident),+) => {
        ops_! { $inc, $op }
        ops_! { $inc + 1, $($ops),+ }
    };
}

macro_rules! ops {
    ($op:ident, $($ops:ident),+) => {
        ops_! { 0, $op }
        ops_! { 1, $($ops),+ }
    };
}

ops! {
    CALL,
    JUMP,
    JUMP_IF_FALSE,
    CLOSE_UPVALUE,
    RETURN,

    // Explicit stack manipulation
    POP,
    PUSH_CONSTANT,
    PUSH_NULL,
    PUSH_TRUE,
    PUSH_FALSE,
    PUSH_CLOSURE,
    PUSH_CLASS,

    // Gets/sets
    GET_GLOBAL,
    SET_GLOBAL,
    GET_LOCAL,
    SET_LOCAL,
    GET_UPVALUE,
    SET_UPVALUE,
    GET_PROPERTY,
    SET_PROPERTY,
    GET_PROPERTY_ID,
    SET_PROPERTY_ID,

    // Arithmetic
    ADD,
    SUBTRACT,
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
    OR
}