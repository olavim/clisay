pub type OpCode = u8;

/// One operand of an instruction.
#[derive(Clone, Copy)]
pub enum Operand {
    /// A raw `u8`.
    Byte,
    /// A call's `u8` flags: what the compiler proved, whether a value is wanted back, and how an
    /// invoke's receiver and member were written.
    CallFlags,
    /// A `u8` local slot.
    Local,
    /// A `u8` index into the constant pool.
    Const,
    /// A `u16` bytecode offset.
    Jump,
    /// A `u8` count followed by that many raw bytes.
    List,
    /// A 16-bit index into a side table.
    Pool,
    /// A 16-bit declaration id.
    TypeId,
}

impl Operand {
    /// The fixed number of bytes this operand occupies, or `None` for a
    /// variable-length operand, which the reader sizes from its count.
    pub fn size(&self) -> Option<usize> {
        match self {
            Operand::Byte | Operand::Local | Operand::Const | Operand::CallFlags => Some(1),
            Operand::Jump => Some(2),
            Operand::Pool | Operand::TypeId => Some(2),
            Operand::List => None,
        }
    }
}

macro_rules! opcodes {
    ( $( $inst:ident => $name:ident $( ( $( $operand:ident ),* ) )? ),+ $(,)? ) => {
        opcodes!(@consts 0u8 ; $( $name ),+ );

        pub fn name(op: OpCode) -> &'static str {
            match op {
                $( $name => stringify!($name), )+
                _ => "UNKNOWN"
            }
        }

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
    (@consts $idx:expr ; ) => {
        /// How many opcodes there are. They are numbered from zero with no gaps.
        pub const COUNT: usize = $idx as usize;
    };
}

opcodes! {
    Call => CALL(CallFlags, Byte),
    Construct => CONSTRUCT(List),
    Invoke => INVOKE(Const, CallFlags, Byte, Byte),
    InvokeThis => INVOKE_THIS(Byte, CallFlags, Byte),
    Jump => JUMP(Jump),
    JumpIfFalse => JUMP_IF_FALSE(Jump),
    JumpIfFalseOrPop => JUMP_IF_FALSE_OR_POP(Jump),
    JumpIfTrueOrPop => JUMP_IF_TRUE_OR_POP(Jump),
    JumpIfCleanOrPop => JUMP_IF_CLEAN_OR_POP(Jump),
    JumpIfClean => JUMP_IF_CLEAN(Jump),
    JumpIfBad => JUMP_IF_BAD(Jump),
    JumpIfIs => JUMP_IF_IS(Jump, TypeId),
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
    Array => ARRAY(Byte),
    Dict => DICT(Byte),
    Return => RETURN,
    ReturnFac => RETURN_FAC,
    ReturnShared => RETURN_SHARED,
    Halt => HALT,
    Throw => THROW,
    TailCall => TAIL_CALL(CallFlags, Byte),
    AssertNonNull => ASSERT_NON_NULL,
    BarrierGuard => BARRIER_GUARD(Pool),
    PopScope => POP_SCOPE(Byte, Byte),
    Pop => POP,
    DiscardChecked => DISCARD_CHECKED,
    Dup => DUP,
    Dup2 => DUP2,
    PushConstant => PUSH_CONSTANT(Const),
    PushUnassigned => PUSH_UNASSIGNED,
    PushNull => PUSH_NULL,
    PushTrue => PUSH_TRUE,
    PushFalse => PUSH_FALSE,
    PushType => PUSH_TYPE(Const),
    LoadGlobal => LOAD_GLOBAL(Const),
    LoadLocal => LOAD_LOCAL(Local),
    StoreLocal => STORE_LOCAL(Local),
    PushSlotAnchor => PUSH_SLOT_ANCHOR(Local, Pool),
    LoadAnchor => LOAD_ANCHOR(Local),
    StoreAnchor => STORE_ANCHOR(Local),
    StoreTempPop => STORE_TEMP_POP(Local),
    StoreLocalPop => STORE_LOCAL_POP(Local),
    StoreLocalAddLocalLocal => STORE_LOCAL_ADD_LOCAL_LOCAL(Local, Local, Local),
    LoadCapture => LOAD_CAPTURE(Byte),
    BuildClosure => BUILD_CLOSURE(Const),
    BuildClosureUnbound => BUILD_CLOSURE_UNBOUND(Const),
    BindClosureCaptures => BIND_CLOSURE_CAPTURES(Local, Const),
    BuildType => BUILD_TYPE(Const),
    BuildTypeUnbound => BUILD_TYPE_UNBOUND(Const),
    BindTypeCaptures => BIND_TYPE_CAPTURES(Local, Const),
    GetIndex => GET_INDEX,
    GetIndexOrNull => GET_INDEX_OR_NULL(Const),
    GetProperty => GET_PROPERTY,
    LoadLocalForWrite => LOAD_LOCAL_FOR_WRITE(Local),
    LoadAnchorForWrite => LOAD_ANCHOR_FOR_WRITE(Local),
    CopyObject => COPY_OBJECT,
    StoreLocalFresh => STORE_LOCAL_FRESH(Local),
    StoreLocalFreshPop => STORE_LOCAL_FRESH_POP(Local),
    LoadStepForWrite => LOAD_STEP_FOR_WRITE(Byte),
    GetField => GET_FIELD(Byte),
    SetField => SET_FIELD(Byte),
    SetFieldPop => SET_FIELD_POP(Byte),
    DictRest => DICT_REST(Byte),
    DictRestValues => DICT_REST_VALUES(Byte),
    Add => ADD,
    AddLocalConst => ADD_LOCAL_CONST(Local, Const),
    AddConstLocal => ADD_CONST_LOCAL(Const, Local),
    Subtract => SUBTRACT,
    SubLocalConst => SUB_LOCAL_CONST(Local, Const),
    SubConstLocal => SUB_CONST_LOCAL(Const, Local),
    IncLocal => INC_LOCAL(Local, Const),
    DecLocal => DEC_LOCAL(Local, Const),
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
    Equal => EQUAL,
    NotEqual => NOT_EQUAL,
    LessThan => LESS_THAN,
    LessThanEqual => LESS_THAN_EQUAL,
    GreaterThan => GREATER_THAN,
    GreaterThanEqual => GREATER_THAN_EQUAL,
    Is => IS(TypeId),
    HasMember => HAS_MEMBER(Const),
    MemberAdmits => MEMBER_ADMITS(Const, Pool),
    IsShaped => IS_SHAPED,
    ArrayLen => ARRAY_LEN,
    ArrayMiddle => ARRAY_MIDDLE(Byte, Byte),
    ArrayElem => ARRAY_ELEM(Byte, Byte),
    ShareLocal => SHARE_LOCAL(Local),
    IsDict => IS_DICT,
    LoadRef => LOAD_REF,
    LoadRefForWrite => LOAD_REF_FOR_WRITE,
    StoreRef => STORE_REF,
    StoreRefPop => STORE_REF_POP,
    SetIndex => SET_INDEX,
    SetProperty => SET_PROPERTY,
    FormAnchorPath => FORM_ANCHOR_PATH(Local, List),
    GetMember => GET_MEMBER(Const),
    SetMember => SET_MEMBER(Const),
    CheckAnchorRoot => CHECK_ANCHOR_ROOT(Local, Byte, Local),
    RecordAnchorRoot => RECORD_ANCHOR_ROOT(Byte),
    CopyAnchorOut => COPY_ANCHOR_OUT(Local, Local),
    CopyAnchorIn => COPY_ANCHOR_IN(Local, Byte, Pool),
}
