//! Operators and the table that drives their parsing: the enum, precedence, associativity, display.

use std::fmt;

use crate::frontend::lex::{TokenStream, TokenType};

/// Builds the `Operator` enum and every per-operator structure from one table. Rows are
/// operator-keyed and grouped by fixity.
macro_rules! operators {
    (
        prefix: { $($pre_op:ident => $pre_tok:ident, $pre_bp:literal;)* }
        infix: { $($in_op:ident => $in_tok:ident, $in_bp:literal;)* }
        infix_right: { $($inr_op:ident => $inr_tok:ident, $inr_bp:literal;)* }
        postfix: { $($post_op:ident => $post_tok:ident, $post_bp:literal;)* }
    ) => {
        #[derive(Clone, Debug)]
        pub enum Operator {
            $($pre_op,)*
            $($in_op,)*
            $($inr_op,)*
            $($post_op,)*
        }

        impl Operator {
            /// Parses a prefix operator at the cursor, advancing only on a match.
            pub fn parse_prefix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
                let op = match stream.peek(0).kind {
                    $(TokenType::$pre_tok => Operator::$pre_op,)*
                    _ => return None,
                };
                if op.has_lower_prefix_precedence(min_precedence) {
                    return None;
                }
                stream.next();
                Some(op)
            }

            /// Parses an infix operator at the cursor, advancing only on a match.
            pub fn parse_infix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
                let op = match stream.peek(0).kind {
                    $(TokenType::$in_tok => Operator::$in_op,)*
                    $(TokenType::$inr_tok => Operator::$inr_op,)*
                    _ => return None,
                };
                if op.has_lower_infix_precedence(min_precedence) {
                    return None;
                }
                stream.next();
                Some(op)
            }

            /// Parses a postfix operator at the cursor, advancing only on a match.
            pub fn parse_postfix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
                let op = match stream.peek(0).kind {
                    $(TokenType::$post_tok => Operator::$post_op,)*
                    _ => return None,
                };
                if op.has_lower_postfix_precedence(min_precedence) {
                    return None;
                }
                stream.next();
                Some(op)
            }

            pub fn prefix_precedence(&self) -> Option<u8> {
                Some(match self {
                    $(Operator::$pre_op => $pre_bp,)*
                    _ => return None,
                })
            }

            pub fn infix_precedence(&self) -> Option<u8> {
                Some(match self {
                    $(Operator::$in_op => $in_bp,)*
                    $(Operator::$inr_op => $inr_bp,)*
                    _ => return None,
                })
            }

            pub fn postfix_precedence(&self) -> Option<u8> {
                Some(match self {
                    $(Operator::$post_op => $post_bp,)*
                    _ => return None,
                })
            }

            /// Prefix operators and the right-associative infix group bind right. The rest bind left.
            fn is_left_associative(&self) -> bool {
                match self {
                    $(Operator::$pre_op)|* => false,
                    $(Operator::$inr_op)|* => false,
                    _ => true,
                }
            }
        }

        impl fmt::Display for Operator {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(Operator::$pre_op => write!(f, "{}", TokenType::$pre_tok),)*
                    $(Operator::$in_op => write!(f, "{}", TokenType::$in_tok),)*
                    $(Operator::$inr_op => write!(f, "{}", TokenType::$inr_tok),)*
                    $(Operator::$post_op => write!(f, "{}", TokenType::$post_tok),)*
                }
            }
        }
    };
}

operators! {
    prefix: {
        Negate => Minus, 13;
        LogicalNot => Exclamation, 13;
        BitNot => Tilde, 13;
        Group => LeftParen, 16;
        Array => LeftBracket, 16;
        Dict => LeftBrace, 16;
    }
    infix: {
        Comma => Comma, 1;
        Coalesce => QuestionQuestion, 3;
        LogicalOr => PipePipe, 4;
        LogicalAnd => AmpAmp, 5;
        LogicalEqual => EqualEqual, 6;
        LogicalNotEqual => NotEqual, 6;
        BitOr => Pipe, 7;
        BitXor => Hat, 8;
        BitAnd => Amp, 9;
        LessThan => LessThan, 10;
        LessThanEqual => LessEqual, 10;
        GreaterThan => GreaterThan, 10;
        GreaterThanEqual => GreaterEqual, 10;
        Is => Is, 10;
        Has => Has, 10;
        LeftShift => LessLess, 11;
        RightShift => GreaterGreater, 11;
        Add => Plus, 12;
        Subtract => Minus, 12;
        Multiply => Multiply, 13;
        Divide => Divide, 13;
        MemberAccess => Dot, 16;
    }
    infix_right: {
        Assign => Equal, 2;
        AddAssign => PlusEqual, 2;
        SubtractAssign => MinusEqual, 2;
        MultiplyAssign => MultiplyEqual, 2;
        DivideAssign => DivideEqual, 2;
        BitAndAssign => AmpEqual, 2;
        BitOrAssign => PipeEqual, 2;
        BitXorAssign => HatEqual, 2;
        LeftShiftAssign => LessLessEqual, 2;
        RightShiftAssign => GreaterGreaterEqual, 2;
        LogicalAndAssign => AmpAmpEqual, 2;
        LogicalOrAssign => PipePipeEqual, 2;
        Arrow => FatArrow, 2;
    }
    postfix: {
        Call => LeftParen, 16;
        Index => LeftBracket, 16;
        SafeMemberAccess => QuestionDot, 16;
        SafeIndex => QuestionBracket, 16;
        Assert => Exclamation, 16;
    }
}

impl Operator {
    fn has_lower_prefix_precedence(&self, precedence: u8) -> bool {
        let prec = self.prefix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
    }

    fn has_lower_infix_precedence(&self, precedence: u8) -> bool {
        let prec = self.infix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
    }

    fn has_lower_postfix_precedence(&self, precedence: u8) -> bool {
        let prec = self.postfix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
    }
}
