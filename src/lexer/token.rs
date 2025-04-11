use std::fmt;

use super::SourcePosition;

macro_rules! tokens {
    ($($token:ident => $lexeme:literal),*) => {
        #[derive(Clone, Copy, PartialEq)]
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

#[derive(Clone)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub pos: SourcePosition
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: &str) -> Self {
        let pos = SourcePosition { file: String::from(""), line: 0 };
        return Token { kind: token_type, lexeme: String::from(lexeme), pos };
    }

    pub fn from_alphanumeric(lexeme: &str) -> Self {
        return Token::new(TokenType::from_alphanumeric(lexeme), lexeme);
    }

    pub fn from_punctuation(lexeme: &str) -> Option<Token> {
        return TokenType::from_punctuation(lexeme).map(|kind| Token::new(kind, lexeme));
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
    Class => "class",
    Init => "init",
    This => "this",
    Super => "super",
    Get => "get",
    Set => "set",

    Return => "return",
    Say => "say",
    Fn => "fn",
    If => "if",
    Else => "else",
    While => "while",
    For => "for",

    True => "true",
    False => "false",
    Null => "null",

    LeftBrace => "{",
    RightBrace => "}",
    LeftParen => "(",
    RightParen => ")",
    LeftBracket => "[",
    RightBracket => "]",
    Question => "?",
    Exclamation => "!",
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
    Dot => "."
}
