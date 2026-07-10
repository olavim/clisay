use std::fmt;
use std::rc::Rc;

use super::{SourceFile, SourcePosition};

thread_local! {
    static EMPTY_FILE: Rc<SourceFile> = Rc::new(SourceFile { name: String::new(), content: Rc::from("") });
}

macro_rules! tokens {
    ($($token:ident => $lexeme:literal),*) => {
        #[derive(Clone, Copy, PartialEq, Debug)]
        pub enum TokenType {
            Identifier,
            NumericLiteral, StringLiteral,
            Comment, Whitespace, Newline, EOF,
            $($token),*
        }

        impl fmt::Display for TokenType {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                return write!(f, "{}", match self {
                    TokenType::NumericLiteral => "<number>",
                    TokenType::StringLiteral => "<string>",
                    TokenType::Identifier => "<identifier>",
                    TokenType::Comment => "<comment>",
                    TokenType::Whitespace => "<whitespace>",
                    TokenType::Newline => "<newline>",
                    TokenType::EOF => "<EOF>",

                    $(TokenType::$token => $lexeme),*
                });
            }
        }

        impl TokenType {
            pub fn from_alphanumeric(lexeme: &str) -> TokenType {
                return match lexeme {
                    $( $lexeme => TokenType::$token, )*
                    _ => TokenType::Identifier
                };
            }

            pub fn from_punctuation(lexeme: &str) -> Option<TokenType> {
                return match lexeme {
                    $( $lexeme => Some(TokenType::$token), )*
                    _ => None
                };
            }
        }
    };
}

/// Identifiers that act as keywords only in specific positions.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ContextualKeyword {
    With,
    Req,
    Gives,
    Pub,
    Inner,
    Mut,
    No,
    Void,
}

impl ContextualKeyword {
    pub fn from_lexeme(lexeme: &str) -> Option<ContextualKeyword> {
        Some(match lexeme {
            "with" => ContextualKeyword::With,
            "req" => ContextualKeyword::Req,
            "gives" => ContextualKeyword::Gives,
            "pub" => ContextualKeyword::Pub,
            "inner" => ContextualKeyword::Inner,
            "mut" => ContextualKeyword::Mut,
            "no" => ContextualKeyword::No,
            "void" => ContextualKeyword::Void,
            _ => return None,
        })
    }
}

impl fmt::Display for ContextualKeyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ContextualKeyword::With => "with",
            ContextualKeyword::Req => "req",
            ContextualKeyword::Gives => "gives",
            ContextualKeyword::Pub => "pub",
            ContextualKeyword::Inner => "inner",
            ContextualKeyword::Mut => "mut",
            ContextualKeyword::No => "no",
            ContextualKeyword::Void => "void",
        })
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub pos: SourcePosition
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: &str) -> Self {
        let pos = SourcePosition { source: EMPTY_FILE.with(Rc::clone), start: 0, end: 0, line: 0 };
        return Token { kind: token_type, lexeme: String::from(lexeme), pos };
    }

    pub fn from_alphanumeric(lexeme: &str) -> Self {
        return Token::new(TokenType::from_alphanumeric(lexeme), lexeme);
    }

    pub fn from_punctuation(lexeme: &str) -> Option<Token> {
        return TokenType::from_punctuation(lexeme).map(|kind| Token::new(kind, lexeme));
    }

    /// The contextual keyword this token spells, if any.
    pub fn contextual(&self) -> Option<ContextualKeyword> {
        match self.kind {
            TokenType::Identifier => ContextualKeyword::from_lexeme(&self.lexeme),
            _ => None,
        }
    }

    pub fn name_word(&self) -> Option<&str> {
        match self.kind {
            TokenType::Identifier => Some(&self.lexeme),
            _ if self.lexeme.starts_with(|c: char| c.is_ascii_alphabetic()) => Some(&self.lexeme),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self.kind {
            TokenType::NumericLiteral => format!("lit({})", self.lexeme),
            TokenType::StringLiteral => format!("lit(\"{}\")", self.lexeme),
            TokenType::Identifier => format!("id({})", self.lexeme),
            _ => format!("{}", self.kind),
        });
    }
}

tokens! {
    Type => "type",
    Trait => "trait",
    Obligation => "obligation",
    This => "this",
    Is => "is",
    Has => "has",

    Return => "return",
    Break => "break",
    Continue => "continue",
    Say => "say",
    Fn => "fn",
    If => "if",
    Else => "else",
    While => "while",
    For => "for",
    Match => "match",
    Throw => "throw",
    Try => "try",
    Catch => "catch",
    Finally => "finally",

    True => "true",
    False => "false",
    Null => "null",

    LeftBrace => "{",
    RightBrace => "}",
    LeftParen => "(",
    RightParen => ")",
    LeftBracket => "[",
    RightBracket => "]",
    Exclamation => "!",
    Question => "?",
    Semicolon => ";",
    Colon => ":",
    Comma => ",",
    Plus => "+",
    Minus => "-",
    Multiply => "*",
    Divide => "/",
    LessThan => "<",
    GreaterThan => ">",
    Equal => "=",
    Amp => "&",
    Pipe => "|",
    Hat => "^",
    Tilde => "~",
    Dot => ".",
    DotDot => "..",
    At => "@",
    EqualEqual => "==",
    NotEqual => "!=",
    LessEqual => "<=",
    GreaterEqual => ">=",
    AmpAmp => "&&",
    PipePipe => "||",
    LessLess => "<<",
    GreaterGreater => ">>",
    FatArrow => "=>",
    PlusEqual => "+=",
    MinusEqual => "-=",
    MultiplyEqual => "*=",
    DivideEqual => "/=",
    AmpEqual => "&=",
    PipeEqual => "|=",
    HatEqual => "^=",
    LessLessEqual => "<<=",
    GreaterGreaterEqual => ">>=",
    AmpAmpEqual => "&&=",
    PipePipeEqual => "||=",
    QuestionQuestion => "??",
    QuestionDot => "?.",
    QuestionBracket => "?["
}
