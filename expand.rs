#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
mod lexer {
    mod token {
        use std::fmt;
        use super::SourcePosition;
        pub struct Token {
            pub kind: TokenType,
            pub lexeme: String,
            pub pos: SourcePosition,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Token {
            #[inline]
            fn clone(&self) -> Token {
                Token {
                    kind: ::core::clone::Clone::clone(&self.kind),
                    lexeme: ::core::clone::Clone::clone(&self.lexeme),
                    pos: ::core::clone::Clone::clone(&self.pos),
                }
            }
        }
        impl Token {
            pub fn new(token_type: TokenType, lexeme: &str) -> Self {
                let pos = SourcePosition {
                    file: String::from(""),
                    line: 0,
                };
                return Token {
                    kind: token_type,
                    lexeme: String::from(lexeme),
                    pos,
                };
            }
            pub fn from_alphanumeric(lexeme: &str) -> Self {
                return Token::new(TokenType::from_alphanumeric(lexeme), lexeme);
            }
            pub fn from_punctuation(lexeme: &str) -> Option<Token> {
                return TokenType::from_punctuation(lexeme)
                    .map(|kind| Token::new(kind, lexeme));
            }
        }
        impl fmt::Display for Token {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                return f
                    .write_fmt(
                        format_args!(
                            "{0}",
                            match self.kind {
                                TokenType::NumericLiteral => {
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("lit({0})", self.lexeme),
                                        );
                                        res
                                    })
                                }
                                TokenType::StringLiteral => {
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("lit(\"{0}\")", self.lexeme),
                                        );
                                        res
                                    })
                                }
                                TokenType::Identifier => {
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("id({0})", self.lexeme),
                                        );
                                        res
                                    })
                                }
                                _ => {
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("{0}", self.kind),
                                        );
                                        res
                                    })
                                }
                            },
                        ),
                    );
            }
        }
        pub enum TokenType {
            Identifier,
            NumericLiteral,
            StringLiteral,
            Comment,
            Whitespace,
            Newline,
            EOF,
            Class,
            Init,
            This,
            Super,
            Return,
            Say,
            Fn,
            If,
            Else,
            While,
            For,
            True,
            False,
            Null,
            LeftBrace,
            RightBrace,
            LeftParen,
            RightParen,
            Question,
            Exclamation,
            Semicolon,
            Colon,
            Comma,
            Plus,
            Minus,
            Multiply,
            Divide,
            LessThan,
            GreaterThan,
            Equal,
            Amp,
            Pipe,
            Hat,
            Tilde,
            Dot,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for TokenType {
            #[inline]
            fn clone(&self) -> TokenType {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for TokenType {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TokenType {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TokenType {
            #[inline]
            fn eq(&self, other: &TokenType) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        impl fmt::Display for TokenType {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                return f
                    .write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                TokenType::NumericLiteral => "<number>",
                                TokenType::StringLiteral => "<string>",
                                TokenType::Identifier => "<identifier>",
                                TokenType::Comment => "<comment>",
                                TokenType::Whitespace => "<whitespace>",
                                TokenType::Newline => "<newline>",
                                TokenType::EOF => "<EOF>",
                                TokenType::Class => "class",
                                TokenType::Init => "init",
                                TokenType::This => "this",
                                TokenType::Super => "super",
                                TokenType::Return => "return",
                                TokenType::Say => "say",
                                TokenType::Fn => "fn",
                                TokenType::If => "if",
                                TokenType::Else => "else",
                                TokenType::While => "while",
                                TokenType::For => "for",
                                TokenType::True => "true",
                                TokenType::False => "false",
                                TokenType::Null => "null",
                                TokenType::LeftBrace => "{",
                                TokenType::RightBrace => "}",
                                TokenType::LeftParen => "(",
                                TokenType::RightParen => ")",
                                TokenType::Question => "?",
                                TokenType::Exclamation => "!",
                                TokenType::Semicolon => ";",
                                TokenType::Colon => ":",
                                TokenType::Comma => ",",
                                TokenType::Plus => "+",
                                TokenType::Minus => "-",
                                TokenType::Multiply => "*",
                                TokenType::Divide => "/",
                                TokenType::LessThan => "<",
                                TokenType::GreaterThan => ">",
                                TokenType::Equal => "=",
                                TokenType::Amp => "&",
                                TokenType::Pipe => "|",
                                TokenType::Hat => "^",
                                TokenType::Tilde => "~",
                                TokenType::Dot => ".",
                            },
                        ),
                    );
            }
        }
        impl TokenType {
            pub fn from_alphanumeric(lexeme: &str) -> TokenType {
                return match lexeme {
                    "class" => TokenType::Class,
                    "init" => TokenType::Init,
                    "this" => TokenType::This,
                    "super" => TokenType::Super,
                    "return" => TokenType::Return,
                    "say" => TokenType::Say,
                    "fn" => TokenType::Fn,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    "for" => TokenType::For,
                    "true" => TokenType::True,
                    "false" => TokenType::False,
                    "null" => TokenType::Null,
                    "{" => TokenType::LeftBrace,
                    "}" => TokenType::RightBrace,
                    "(" => TokenType::LeftParen,
                    ")" => TokenType::RightParen,
                    "?" => TokenType::Question,
                    "!" => TokenType::Exclamation,
                    ";" => TokenType::Semicolon,
                    ":" => TokenType::Colon,
                    "," => TokenType::Comma,
                    "+" => TokenType::Plus,
                    "-" => TokenType::Minus,
                    "*" => TokenType::Multiply,
                    "/" => TokenType::Divide,
                    "<" => TokenType::LessThan,
                    ">" => TokenType::GreaterThan,
                    "=" => TokenType::Equal,
                    "&" => TokenType::Amp,
                    "|" => TokenType::Pipe,
                    "^" => TokenType::Hat,
                    "~" => TokenType::Tilde,
                    "." => TokenType::Dot,
                    _ => TokenType::Identifier,
                };
            }
            pub fn from_punctuation(lexeme: &str) -> Option<TokenType> {
                return match lexeme {
                    "class" => Some(TokenType::Class),
                    "init" => Some(TokenType::Init),
                    "this" => Some(TokenType::This),
                    "super" => Some(TokenType::Super),
                    "return" => Some(TokenType::Return),
                    "say" => Some(TokenType::Say),
                    "fn" => Some(TokenType::Fn),
                    "if" => Some(TokenType::If),
                    "else" => Some(TokenType::Else),
                    "while" => Some(TokenType::While),
                    "for" => Some(TokenType::For),
                    "true" => Some(TokenType::True),
                    "false" => Some(TokenType::False),
                    "null" => Some(TokenType::Null),
                    "{" => Some(TokenType::LeftBrace),
                    "}" => Some(TokenType::RightBrace),
                    "(" => Some(TokenType::LeftParen),
                    ")" => Some(TokenType::RightParen),
                    "?" => Some(TokenType::Question),
                    "!" => Some(TokenType::Exclamation),
                    ";" => Some(TokenType::Semicolon),
                    ":" => Some(TokenType::Colon),
                    "," => Some(TokenType::Comma),
                    "+" => Some(TokenType::Plus),
                    "-" => Some(TokenType::Minus),
                    "*" => Some(TokenType::Multiply),
                    "/" => Some(TokenType::Divide),
                    "<" => Some(TokenType::LessThan),
                    ">" => Some(TokenType::GreaterThan),
                    "=" => Some(TokenType::Equal),
                    "&" => Some(TokenType::Amp),
                    "|" => Some(TokenType::Pipe),
                    "^" => Some(TokenType::Hat),
                    "~" => Some(TokenType::Tilde),
                    "." => Some(TokenType::Dot),
                    _ => None,
                };
            }
        }
    }
    mod token_stream {
        use anyhow::bail;
        use crate::lexer::{Token, TokenType};
        type TokenResult<T> = Result<T, anyhow::Error>;
        pub struct TokenStream<'a> {
            tokens: &'a Vec<Token>,
            pos: usize,
        }
        impl<'a> TokenStream<'a> {
            pub fn new(tokens: &Vec<Token>) -> TokenStream {
                return TokenStream { tokens, pos: 0 };
            }
            pub fn peek(&self, look_ahead: usize) -> &'a Token {
                if self.pos + look_ahead >= self.tokens.len() {
                    return self.tokens.last().unwrap();
                }
                return self.tokens.get(self.pos + look_ahead).unwrap();
            }
            pub fn match_next(&self, token_type: TokenType) -> bool {
                return self.peek(0).kind == token_type;
            }
            pub fn next(&mut self) -> &'a Token {
                if self.pos >= self.tokens.len() {
                    return self.tokens.last().unwrap();
                }
                let token = self.tokens.get(self.pos);
                self.pos += 1;
                return token.unwrap();
            }
            pub fn next_if(&mut self, token_type: TokenType) -> Option<&'a Token> {
                if self.peek(0).kind == token_type {
                    return Some(self.next());
                }
                return None;
            }
            pub fn has_next(&self) -> bool {
                return self.pos < self.tokens.len()
                    && self.peek(0).kind != TokenType::EOF;
            }
            pub fn expect(&mut self, token_type: TokenType) -> TokenResult<&'a Token> {
                let token = self.next();
                if token.kind == token_type {
                    Ok(token)
                } else {
                    return ::anyhow::__private::Err(
                        ::anyhow::Error::msg(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Unexpected token {0} at {1}: Expected {2} but got {3}",
                                        token,
                                        token.pos,
                                        token_type,
                                        token,
                                    ),
                                );
                                res
                            }),
                        ),
                    )
                }
            }
            pub fn advance(&mut self, count: usize) {
                self.pos += count;
            }
        }
    }
    use std::fmt;
    use anyhow::bail;
    use regex::Regex;
    pub use token::{Token, TokenType};
    pub use token_stream::TokenStream;
    const REGEX_STRING: &str = r#""([^"\\]|\\.)*""#;
    const REGEX_NUMERIC: &str = r"(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?";
    const REGEX_ALPHANUMERIC: &str = r"[a-zA-Z_][a-zA-Z0-9_]*";
    const REGEX_COMMENT: &str = r"\/\/[^\n\r]*";
    const REGEX_NEWLINE: &str = r"(\r\n|\r|\n)";
    const REGEX_WHITESPACE: &str = r"[^\S\r\n]+";
    pub struct SourcePosition {
        pub file: String,
        pub line: usize,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SourcePosition {
        #[inline]
        fn clone(&self) -> SourcePosition {
            SourcePosition {
                file: ::core::clone::Clone::clone(&self.file),
                line: ::core::clone::Clone::clone(&self.line),
            }
        }
    }
    impl fmt::Display for SourcePosition {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            return f.write_fmt(format_args!("{0}:{1}", self.file, self.line));
        }
    }
    fn find_at<'a>(regex: &str, input: &'a str, pos: usize) -> Option<usize> {
        return match Regex::new(regex).unwrap().find(&input[pos..]) {
            Some(mat) if mat.start() == 0 => Option::from(pos + mat.len()),
            _ => None,
        };
    }
    fn next_token(input: &str, input_index: usize) -> Result<Token, anyhow::Error> {
        if input_index >= input.len() {
            return Ok(Token::new(TokenType::EOF, ""));
        }
        if let Some(end) = find_at(REGEX_COMMENT, input, input_index) {
            return Ok(Token::new(TokenType::Comment, &input[input_index..end]));
        }
        if let Some(end) = find_at(REGEX_WHITESPACE, input, input_index) {
            return Ok(Token::new(TokenType::Whitespace, &input[input_index..end]));
        }
        if let Some(end) = find_at(REGEX_NEWLINE, input, input_index) {
            return Ok(Token::new(TokenType::Newline, &input[input_index..end]));
        }
        if let Some(end) = find_at(REGEX_ALPHANUMERIC, input, input_index) {
            return Ok(Token::from_alphanumeric(&input[input_index..end]));
        }
        if let Some(end) = find_at(REGEX_NUMERIC, input, input_index) {
            return Ok(Token::new(TokenType::NumericLiteral, &input[input_index..end]));
        }
        if let Some(end) = find_at(REGEX_STRING, input, input_index) {
            return Ok(Token::new(TokenType::StringLiteral, &input[input_index..end]));
        }
        let end = input_index + 1;
        let substr = &input[input_index..end];
        if let Some(token) = Token::from_punctuation(substr) {
            return Ok(token);
        }
        return ::anyhow::__private::Err(
            ::anyhow::Error::msg(
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!(
                            "Unexpected character `{0}` at pos {1}",
                            substr,
                            input_index,
                        ),
                    );
                    res
                }),
            ),
        );
    }
    pub fn tokenize(
        file_name: String,
        input: String,
    ) -> Result<Vec<Token>, anyhow::Error> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut input_index = 0;
        let mut line = 1;
        while tokens.last().map_or(true, |t| t.kind != TokenType::EOF) {
            let mut token = next_token(&input, input_index)?;
            token.pos = SourcePosition {
                file: file_name.clone(),
                line,
            };
            input_index += token.lexeme.len();
            if token.kind == TokenType::Newline {
                line += 1;
            }
            if !match token.kind {
                TokenType::Whitespace | TokenType::Comment | TokenType::Newline => true,
                _ => false,
            } {
                tokens.push(token);
            }
        }
        return Ok(tokens);
    }
}
mod vm {
    mod stack {
        pub struct Stack<T, const N: usize> {
            values: [T; N],
            top: *mut T,
            bottom: *mut T,
        }
        impl<'a, T: Copy, const N: usize> Stack<T, N> {
            pub fn new() -> Self {
                Self {
                    values: [unsafe { std::mem::zeroed() }; N],
                    top: std::ptr::null_mut(),
                    bottom: std::ptr::null_mut(),
                }
            }
            pub fn init(&mut self) {
                self.top = self.values.as_mut_ptr();
                self.bottom = self.values.as_mut_ptr();
            }
            pub fn top(&self) -> *mut T {
                self.top
            }
            pub fn set_top(&mut self, top: *mut T) {
                self.top = top;
            }
            pub fn offset(&self, offset: usize) -> *mut T {
                unsafe { self.top.sub(offset + 1) }
            }
            pub fn push(&mut self, value: T) {
                unsafe {
                    *self.top = value;
                    self.top = self.top.add(1);
                }
            }
            pub fn pop(&mut self) -> T {
                unsafe {
                    self.top = self.top.sub(1);
                    *self.top
                }
            }
            pub fn truncate(&mut self, count: usize) {
                unsafe {
                    self.top = self.top.sub(count);
                }
            }
            pub fn peek(&self, offset: usize) -> T {
                unsafe { *self.top.sub(offset + 1) }
            }
            pub fn set(&mut self, offset: usize, value: T) -> *mut T {
                unsafe {
                    let ptr = self.top.sub(offset + 1);
                    *ptr = value;
                    ptr
                }
            }
            pub fn iter(&'a self) -> StackIter<'a, T, N> {
                StackIter {
                    stack: self,
                    curr: self.bottom,
                }
            }
            pub fn len(&self) -> usize {
                (self.top as isize - self.bottom as isize) as usize
                    / std::mem::size_of::<T>()
            }
        }
        pub struct StackIter<'a, T: Copy, const N: usize> {
            stack: &'a Stack<T, N>,
            curr: *mut T,
        }
        impl<'a, T: Copy, const N: usize> Iterator for StackIter<'a, T, N> {
            type Item = T;
            fn next(&mut self) -> Option<Self::Item> {
                if self.curr >= self.stack.top {
                    return None;
                }
                let value = unsafe { *self.curr };
                self.curr = unsafe { self.curr.add(1) };
                Some(value)
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                let stack_len = self.stack.len();
                let offset = unsafe {
                    self.curr.offset_from(self.stack.bottom) as usize
                };
                let len = stack_len - offset;
                (len, Some(len))
            }
        }
        impl<'a, T: Copy, const N: usize> DoubleEndedIterator for StackIter<'a, T, N> {
            fn next_back(&mut self) -> Option<Self::Item> {
                if self.curr <= self.stack.bottom {
                    return None;
                }
                self.curr = unsafe { self.curr.sub(1) };
                Some(unsafe { *self.curr })
            }
        }
        impl<'a, T: Copy, const N: usize> ExactSizeIterator for StackIter<'a, T, N> {}
        pub struct CachedStack<T, const N: usize> {
            stack: Stack<T, N>,
            pub top: T,
        }
        impl<'a, T: Copy, const N: usize> CachedStack<T, N> {
            pub fn new() -> Self {
                Self {
                    stack: Stack::new(),
                    top: unsafe { std::mem::zeroed() },
                }
            }
            pub fn init(&mut self) {
                self.stack.init();
            }
            pub fn push(&mut self, value: T) {
                self.stack.push(value);
                self.top = value;
            }
            pub fn pop(&mut self) -> T {
                self.stack.truncate(1);
                let value = self.top;
                self.top = self.stack.peek(0);
                value
            }
            pub fn iter(&'a self) -> StackIter<'a, T, N> {
                self.stack.iter()
            }
            pub fn len(&self) -> usize {
                self.stack.len()
            }
        }
    }
    mod opcode {
        pub type OpCode = u8;
        pub const CALL: OpCode = 0;
        pub const JUMP: OpCode = 1;
        pub const JUMP_IF_FALSE: OpCode = 1 + 1;
        pub const CLOSE_UPVALUE: OpCode = 1 + 1 + 1;
        pub const RETURN: OpCode = 1 + 1 + 1 + 1;
        pub const POP: OpCode = 1 + 1 + 1 + 1 + 1;
        pub const PUSH_CONSTANT: OpCode = 1 + 1 + 1 + 1 + 1 + 1;
        pub const PUSH_NULL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const PUSH_TRUE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const PUSH_FALSE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const PUSH_CLOSURE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const PUSH_CLASS: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const GET_GLOBAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const SET_GLOBAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const GET_LOCAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1;
        pub const SET_LOCAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1;
        pub const GET_UPVALUE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1;
        pub const SET_UPVALUE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1;
        pub const GET_PROPERTY: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1;
        pub const SET_PROPERTY: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const GET_PROPERTY_ID: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const SET_PROPERTY_ID: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const ADD: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const SUBTRACT: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const MULTIPLY: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const DIVIDE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const NEGATE: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const LEFT_SHIFT: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const RIGHT_SHIFT: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const BIT_AND: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const BIT_OR: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const BIT_XOR: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const BIT_NOT: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const EQUAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const NOT_EQUAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1;
        pub const LESS_THAN: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1;
        pub const LESS_THAN_EQUAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1;
        pub const GREATER_THAN: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1;
        pub const GREATER_THAN_EQUAL: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        pub const NOT: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1;
        pub const AND: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1;
        pub const OR: OpCode = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
            + 1 + 1 + 1 + 1 + 1 + 1 + 1;
    }
    mod chunk {
        use std::mem;
        use anyhow::bail;
        use crate::lexer::SourcePosition;
        use crate::vm::opcode;
        use super::gc::{Gc, GcTraceable};
        use super::opcode::OpCode;
        use super::value::Value;
        pub struct BytecodeChunk {
            pub code: Vec<OpCode>,
            pub constants: Vec<Value>,
            pub code_pos: Vec<SourcePosition>,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for BytecodeChunk {
            #[inline]
            fn clone(&self) -> BytecodeChunk {
                BytecodeChunk {
                    code: ::core::clone::Clone::clone(&self.code),
                    constants: ::core::clone::Clone::clone(&self.constants),
                    code_pos: ::core::clone::Clone::clone(&self.code_pos),
                }
            }
        }
        impl BytecodeChunk {
            pub fn new() -> BytecodeChunk {
                return BytecodeChunk {
                    code: Vec::new(),
                    constants: Vec::new(),
                    code_pos: Vec::new(),
                };
            }
            pub fn write(&mut self, op: OpCode, pos: &SourcePosition) {
                self.code.push(op);
                self.code_pos.push(pos.clone());
            }
            pub fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
                if self.constants.len() >= u8::MAX as usize {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Too many constants"),
                        );
                        error
                    });
                }
                self.constants.push(value);
                Ok((self.constants.len() - 1) as u8)
            }
        }
        impl GcTraceable for BytecodeChunk {
            fn fmt(&self) -> String {
                let mut string = String::new();
                let mut pos = 0;
                while pos < self.code.len() {
                    {
                        string
                            .push_str(
                                &::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("{0}: ", pos));
                                    res
                                }),
                            );
                    };
                    let op = {
                        pos += 1;
                        self.code[pos - 1]
                    };
                    match op {
                        opcode::RETURN => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("RET"));
                                        res
                                    }),
                                );
                        }
                        opcode::POP => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("POP"));
                                        res
                                    }),
                                );
                        }
                        opcode::CALL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "CALL {0}",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::JUMP => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "JUMP <{0}>",
                                                {
                                                    pos += 2;
                                                    (self.code[pos - 2] as u16)
                                                        | ((self.code[pos - 1] as u16) << 8)
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::JUMP_IF_FALSE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "JUMP_F <{0}>",
                                                {
                                                    pos += 2;
                                                    (self.code[pos - 2] as u16)
                                                        | ((self.code[pos - 1] as u16) << 8)
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::CLOSE_UPVALUE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "CLOSE_UPVALUE <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::PUSH_NULL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("NULL"));
                                        res
                                    }),
                                );
                        }
                        opcode::PUSH_TRUE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("TRUE"));
                                        res
                                    }),
                                );
                        }
                        opcode::PUSH_FALSE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("FALSE"));
                                        res
                                    }),
                                );
                        }
                        opcode::PUSH_CONSTANT => {
                            {
                                string
                                    .push_str(
                                        &::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!(
                                                    "CONST {0}",
                                                    self
                                                        .constants[{
                                                            pos += 1;
                                                            self.code[pos - 1]
                                                        } as usize]
                                                        .fmt(),
                                                ),
                                            );
                                            res
                                        }),
                                    );
                            };
                        }
                        opcode::PUSH_CLOSURE => {
                            let func_const = self
                                .constants[{
                                pos += 1;
                                self.code[pos - 1]
                            } as usize];
                            let func = unsafe { func_const.as_object().function };
                            {
                                string
                                    .push_str(
                                        &::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!("CLOSURE {0}", unsafe { (*func).fmt() }),
                                            );
                                            res
                                        }),
                                    );
                            };
                        }
                        opcode::PUSH_CLASS => {
                            let class_const = self
                                .constants[{
                                pos += 1;
                                self.code[pos - 1]
                            } as usize];
                            let class = unsafe { class_const.as_object().class };
                            {
                                string
                                    .push_str(
                                        &::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!("CLASS {0}", unsafe { (*class).fmt() }),
                                            );
                                            res
                                        }),
                                    );
                            };
                        }
                        opcode::ADD => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("ADD"));
                                        res
                                    }),
                                );
                        }
                        opcode::SUBTRACT => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("SUB"));
                                        res
                                    }),
                                );
                        }
                        opcode::MULTIPLY => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("MUL"));
                                        res
                                    }),
                                );
                        }
                        opcode::DIVIDE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("DIV"));
                                        res
                                    }),
                                );
                        }
                        opcode::NEGATE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("NEG"));
                                        res
                                    }),
                                );
                        }
                        opcode::EQUAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("EQ"));
                                        res
                                    }),
                                );
                        }
                        opcode::NOT_EQUAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("NEQ"));
                                        res
                                    }),
                                );
                        }
                        opcode::LESS_THAN => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("LT"));
                                        res
                                    }),
                                );
                        }
                        opcode::LESS_THAN_EQUAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("LTE"));
                                        res
                                    }),
                                );
                        }
                        opcode::GREATER_THAN => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("GT"));
                                        res
                                    }),
                                );
                        }
                        opcode::GREATER_THAN_EQUAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("GTE"));
                                        res
                                    }),
                                );
                        }
                        opcode::NOT => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("NOT"));
                                        res
                                    }),
                                );
                        }
                        opcode::LEFT_SHIFT => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("LSH"));
                                        res
                                    }),
                                );
                        }
                        opcode::RIGHT_SHIFT => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("RSH"));
                                        res
                                    }),
                                );
                        }
                        opcode::BIT_AND => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("AND"));
                                        res
                                    }),
                                );
                        }
                        opcode::BIT_OR => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("OR"));
                                        res
                                    }),
                                );
                        }
                        opcode::BIT_XOR => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("XOR"));
                                        res
                                    }),
                                );
                        }
                        opcode::BIT_NOT => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("BIT_NOT"));
                                        res
                                    }),
                                );
                        }
                        opcode::AND => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("AND"));
                                        res
                                    }),
                                );
                        }
                        opcode::OR => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(format_args!("OR"));
                                        res
                                    }),
                                );
                        }
                        opcode::GET_LOCAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "GET_LOCAL <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::SET_LOCAL => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "SET_LOCAL <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::GET_UPVALUE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "GET_UPVAL <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::SET_UPVALUE => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "SET_UPVAL <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::GET_PROPERTY => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "GET_PROP <{0}>",
                                                self
                                                    .constants[{
                                                        pos += 1;
                                                        self.code[pos - 1]
                                                    } as usize]
                                                    .fmt(),
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::SET_PROPERTY => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "SET_PROP <{0}>",
                                                self
                                                    .constants[{
                                                        pos += 1;
                                                        self.code[pos - 1]
                                                    } as usize]
                                                    .fmt(),
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::GET_PROPERTY_ID => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "GET_PROP_ID <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::SET_PROPERTY_ID => {
                            string
                                .push_str(
                                    &::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "SET_PROP_ID <{0}>",
                                                {
                                                    pos += 1;
                                                    self.code[pos - 1]
                                                },
                                            ),
                                        );
                                        res
                                    }),
                                );
                        }
                        opcode::GET_GLOBAL => {
                            {
                                string
                                    .push_str(
                                        &::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!(
                                                    "GET_GLOBAL {0}",
                                                    self
                                                        .constants[{
                                                            pos += 1;
                                                            self.code[pos - 1]
                                                        } as usize]
                                                        .fmt(),
                                                ),
                                            );
                                            res
                                        }),
                                    );
                            };
                        }
                        opcode::SET_GLOBAL => {
                            {
                                string
                                    .push_str(
                                        &::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!(
                                                    "SET_GLOBAL {0}",
                                                    self
                                                        .constants[{
                                                            pos += 1;
                                                            self.code[pos - 1]
                                                        } as usize]
                                                        .fmt(),
                                                ),
                                            );
                                            res
                                        }),
                                    );
                            };
                        }
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!("Unknown opcode {0}", op),
                            );
                        }
                    }
                    {
                        string
                            .push_str(
                                &::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("\n"));
                                    res
                                }),
                            );
                    };
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
    }
    mod objects {
        use std::{fmt, mem};
        use nohash_hasher::{IntMap, IntSet};
        use super::gc::{Gc, GcTraceable};
        use super::value::Value;
        use super::vm::Vm;
        pub enum ObjectKind {
            Closure,
            NativeFunction,
            Class,
            BoundMethod,
            String,
            Function,
            Instance,
            Upvalue,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ObjectKind {
            #[inline]
            fn clone(&self) -> ObjectKind {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for ObjectKind {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ObjectKind {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ObjectKind {
            #[inline]
            fn eq(&self, other: &ObjectKind) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        impl fmt::Display for ObjectKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let kind = match self {
                    ObjectKind::String => "string",
                    ObjectKind::Function => "function",
                    ObjectKind::NativeFunction => "function",
                    ObjectKind::BoundMethod => "function",
                    ObjectKind::Closure => "function",
                    ObjectKind::Class => "class",
                    ObjectKind::Instance => "object",
                    ObjectKind::Upvalue => "upvalue",
                };
                f.write_fmt(format_args!("{0}", kind))
            }
        }
        #[repr(C)]
        pub struct ObjectHeader {
            pub kind: ObjectKind,
            pub marked: bool,
        }
        impl ObjectHeader {
            pub fn new(kind: ObjectKind) -> ObjectHeader {
                ObjectHeader {
                    kind,
                    marked: false,
                }
            }
        }
        #[repr(C)]
        pub union Object {
            pub header: *mut ObjectHeader,
            pub string: *mut ObjString,
            pub function: *mut ObjFn,
            pub native_function: *mut ObjNativeFn,
            pub bound_method: *mut ObjBoundMethod,
            pub closure: *mut ObjClosure,
            pub class: *mut ObjClass,
            pub instance: *mut ObjInstance,
            pub upvalue: *mut ObjUpvalue,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Object {
            #[inline]
            fn clone(&self) -> Object {
                let _: ::core::clone::AssertParamIsCopy<Self>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for Object {}
        impl Object {
            #[inline]
            pub fn kind(self) -> ObjectKind {
                unsafe { (*self.header).kind }
            }
            pub fn free(self) -> usize {
                match self.kind() {
                    ObjectKind::String => {
                        let size = unsafe { (*self.string).size() };
                        let _ = unsafe { Box::from_raw(self.string) };
                        size
                    }
                    ObjectKind::Function => {
                        let size = unsafe { (*self.function).size() };
                        let _ = unsafe { Box::from_raw(self.function) };
                        size
                    }
                    ObjectKind::NativeFunction => {
                        let size = unsafe { (*self.native_function).size() };
                        let _ = unsafe { Box::from_raw(self.native_function) };
                        size
                    }
                    ObjectKind::BoundMethod => {
                        let size = unsafe { (*self.bound_method).size() };
                        let _ = unsafe { Box::from_raw(self.bound_method) };
                        size
                    }
                    ObjectKind::Closure => {
                        let size = unsafe { (*self.closure).size() };
                        let _ = unsafe { Box::from_raw(self.closure) };
                        size
                    }
                    ObjectKind::Class => {
                        let size = unsafe { (*self.class).size() };
                        let _ = unsafe { Box::from_raw(self.class) };
                        size
                    }
                    ObjectKind::Instance => {
                        let size = unsafe { (*self.instance).size() };
                        let _ = unsafe { Box::from_raw(self.instance) };
                        size
                    }
                    ObjectKind::Upvalue => {
                        let size = unsafe { (*self.upvalue).size() };
                        let _ = unsafe { Box::from_raw(self.upvalue) };
                        size
                    }
                }
            }
            fn as_traceable(&self) -> &dyn GcTraceable {
                unsafe {
                    match self.kind() {
                        ObjectKind::String => &*self.as_string(),
                        ObjectKind::Function => &*self.as_function(),
                        ObjectKind::NativeFunction => &*self.as_native_function(),
                        ObjectKind::BoundMethod => &*self.as_bound_method(),
                        ObjectKind::Closure => &*self.as_closure(),
                        ObjectKind::Class => &*self.as_class(),
                        ObjectKind::Instance => &*self.as_instance(),
                        ObjectKind::Upvalue => &*self.as_upvalue(),
                    }
                }
            }
            #[inline]
            pub fn as_string(&self) -> *mut ObjString {
                unsafe { self.string }
            }
            #[inline]
            pub fn as_function(&self) -> *mut ObjFn {
                unsafe { self.function }
            }
            #[inline]
            pub fn as_native_function(&self) -> *mut ObjNativeFn {
                unsafe { self.native_function }
            }
            #[inline]
            pub fn as_bound_method(&self) -> *mut ObjBoundMethod {
                unsafe { self.bound_method }
            }
            #[inline]
            pub fn as_closure(&self) -> *mut ObjClosure {
                unsafe { self.closure }
            }
            #[inline]
            pub fn as_class(&self) -> *mut ObjClass {
                unsafe { self.class }
            }
            #[inline]
            pub fn as_instance(&self) -> *mut ObjInstance {
                unsafe { self.instance }
            }
            #[inline]
            pub fn as_upvalue(&self) -> *mut ObjUpvalue {
                unsafe { self.upvalue }
            }
        }
        impl GcTraceable for Object {
            fn fmt(&self) -> String {
                self.as_traceable().fmt()
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(*self);
                self.as_traceable().mark(gc);
            }
            fn size(&self) -> usize {
                self.as_traceable().size()
            }
        }
        impl From<*mut ObjectHeader> for Object {
            fn from(header: *mut ObjectHeader) -> Self {
                Object { header }
            }
        }
        impl From<*mut ObjString> for Object {
            fn from(string: *mut ObjString) -> Self {
                Object { string }
            }
        }
        impl From<*mut ObjInstance> for Object {
            fn from(instance: *mut ObjInstance) -> Self {
                Object { instance }
            }
        }
        impl From<*mut ObjUpvalue> for Object {
            fn from(upvalue: *mut ObjUpvalue) -> Self {
                Object { upvalue }
            }
        }
        impl From<*mut ObjFn> for Object {
            fn from(function: *mut ObjFn) -> Self {
                Object { function }
            }
        }
        impl From<*mut ObjNativeFn> for Object {
            fn from(native_function: *mut ObjNativeFn) -> Self {
                Object { native_function }
            }
        }
        impl From<*mut ObjBoundMethod> for Object {
            fn from(bound_method: *mut ObjBoundMethod) -> Self {
                Object { bound_method }
            }
        }
        impl From<*mut ObjClosure> for Object {
            fn from(closure: *mut ObjClosure) -> Self {
                Object { closure }
            }
        }
        impl From<*mut ObjClass> for Object {
            fn from(class: *mut ObjClass) -> Self {
                Object { class }
            }
        }
        #[repr(C)]
        pub struct ObjString {
            pub header: ObjectHeader,
            pub value: String,
        }
        impl ObjString {
            pub fn new(value: String) -> ObjString {
                ObjString {
                    header: ObjectHeader::new(ObjectKind::String),
                    value,
                }
            }
        }
        impl GcTraceable for ObjString {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(format_args!("{0}", self.value));
                    res
                })
            }
            fn mark(&self, _gc: &mut Gc) {}
            fn size(&self) -> usize {
                mem::size_of::<ObjString>() + self.value.capacity()
            }
        }
        pub struct UpvalueLocation {
            pub is_local: bool,
            pub location: u8,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for UpvalueLocation {
            #[inline]
            fn clone(&self) -> UpvalueLocation {
                let _: ::core::clone::AssertParamIsClone<bool>;
                let _: ::core::clone::AssertParamIsClone<u8>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for UpvalueLocation {}
        #[repr(C)]
        pub struct ObjFn {
            pub header: ObjectHeader,
            pub name: *mut ObjString,
            pub arity: u8,
            pub ip_start: usize,
            pub upvalues: Vec<UpvalueLocation>,
        }
        impl ObjFn {
            pub fn new(
                name: *mut ObjString,
                arity: u8,
                ip_start: usize,
                upvalues: Vec<UpvalueLocation>,
            ) -> ObjFn {
                ObjFn {
                    header: ObjectHeader::new(ObjectKind::Function),
                    name,
                    arity,
                    ip_start,
                    upvalues,
                }
            }
        }
        impl GcTraceable for ObjFn {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("<fn {0}>", unsafe { &(*self.name).value }),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.name);
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjFn>()
                    + self.upvalues.capacity() * mem::size_of::<UpvalueLocation>()
            }
        }
        #[repr(C)]
        pub struct ObjNativeFn {
            pub header: ObjectHeader,
            pub name: *mut ObjString,
            pub arity: u8,
            pub function: fn(vm: &mut Vm),
        }
        impl ObjNativeFn {
            pub fn new(
                name: *mut ObjString,
                arity: u8,
                function: fn(vm: &mut Vm),
            ) -> ObjNativeFn {
                ObjNativeFn {
                    header: ObjectHeader::new(ObjectKind::NativeFunction),
                    name,
                    arity,
                    function,
                }
            }
        }
        impl GcTraceable for ObjNativeFn {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("<native fn {0}>", unsafe { &(*self.name).value }),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.name);
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjNativeFn>()
            }
        }
        #[repr(C)]
        pub struct ObjClosure {
            pub header: ObjectHeader,
            pub function: *mut ObjFn,
            pub upvalues: Vec<*mut ObjUpvalue>,
        }
        impl ObjClosure {
            pub fn new(function: *mut ObjFn) -> ObjClosure {
                return ObjClosure {
                    header: ObjectHeader::new(ObjectKind::Closure),
                    function,
                    upvalues: Vec::new(),
                };
            }
        }
        impl GcTraceable for ObjClosure {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!(
                            "<closure {0}>",
                            unsafe { &(*(*self.function).name).value },
                        ),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.function);
                for upvalue in &self.upvalues {
                    gc.mark_object(*upvalue);
                }
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjClosure>()
                    + self.upvalues.capacity() * mem::size_of::<*mut ObjUpvalue>()
            }
        }
        #[repr(C)]
        pub struct ObjBoundMethod {
            pub header: ObjectHeader,
            pub instance: *mut ObjInstance,
            pub closure: *mut ObjClosure,
        }
        impl ObjBoundMethod {
            pub fn new(
                instance: *mut ObjInstance,
                closure: *mut ObjClosure,
            ) -> ObjBoundMethod {
                ObjBoundMethod {
                    header: ObjectHeader::new(ObjectKind::BoundMethod),
                    instance,
                    closure,
                }
            }
        }
        impl GcTraceable for ObjBoundMethod {
            fn fmt(&self) -> String {
                let closure = unsafe { &*self.closure };
                let function = unsafe { &*closure.function };
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!(
                            "<bound method {0}>",
                            unsafe { &(*function.name).value },
                        ),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.instance);
                gc.mark_object(self.closure);
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjBoundMethod>()
            }
        }
        type MemberId = u8;
        pub enum ClassMember {
            Field(MemberId),
            Method(MemberId),
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ClassMember {
            #[inline]
            fn clone(&self) -> ClassMember {
                let _: ::core::clone::AssertParamIsClone<MemberId>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for ClassMember {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ClassMember {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ClassMember {
            #[inline]
            fn eq(&self, other: &ClassMember) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
                    && match (self, other) {
                        (ClassMember::Field(__self_0), ClassMember::Field(__arg1_0)) => {
                            __self_0 == __arg1_0
                        }
                        (
                            ClassMember::Method(__self_0),
                            ClassMember::Method(__arg1_0),
                        ) => __self_0 == __arg1_0,
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
            }
        }
        #[repr(C)]
        pub struct ObjClass {
            pub header: ObjectHeader,
            pub name: *mut ObjString,
            members: IntMap<usize, ClassMember>,
            pub fields: IntSet<MemberId>,
            methods: IntMap<MemberId, *mut ObjFn>,
            next_member_id: MemberId,
        }
        impl ObjClass {
            pub fn new(name: *mut ObjString, superclass: Option<&ObjClass>) -> ObjClass {
                let mut class = ObjClass {
                    header: ObjectHeader::new(ObjectKind::Class),
                    name,
                    members: IntMap::default(),
                    fields: IntSet::default(),
                    methods: IntMap::default(),
                    next_member_id: 1,
                };
                if let Some(superclass) = superclass {
                    class.inherit(superclass);
                }
                class
            }
            fn inherit(&mut self, superclass: &ObjClass) {
                self.next_member_id = superclass.next_member_id;
                self.members = superclass.members.clone();
                self.fields = superclass.fields.clone();
                self.methods = superclass.methods.clone();
                self.methods.remove(&0);
            }
            pub fn declare_field(&mut self, name: *mut ObjString) {
                self.members
                    .insert(name as usize, ClassMember::Field(self.next_member_id));
                self.fields.insert(self.next_member_id);
                self.next_member_id += 1;
            }
            pub fn declare_method(&mut self, name: *mut ObjString) {
                self.members
                    .insert(name as usize, ClassMember::Method(self.next_member_id));
                self.next_member_id += 1;
            }
            pub fn define_method(&mut self, name: *mut ObjString, method: *mut ObjFn) {
                let ClassMember::Method(id) = self.resolve(name).unwrap() else {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "internal error: entered unreachable code: {0}",
                                format_args!("Cannot define field as method"),
                            ),
                        );
                    };
                };
                self.methods.insert(id, method);
            }
            pub fn resolve(&self, name: *mut ObjString) -> Option<ClassMember> {
                self.members.get(&(name as usize)).copied()
            }
            pub fn resolve_id(&self, name: *mut ObjString) -> Option<MemberId> {
                match self.members.get(&(name as usize)) {
                    Some(ClassMember::Field(id)) | Some(ClassMember::Method(id)) => {
                        Some(*id)
                    }
                    None => None,
                }
            }
            pub fn get_method(&self, id: MemberId) -> Option<*mut ObjFn> {
                self.methods.get(&id).copied()
            }
        }
        impl GcTraceable for ObjClass {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("<class {0}>", unsafe { &(*self.name).value }),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.name);
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjClass>()
                    + self.members.capacity()
                        * (mem::size_of::<*mut String>() + mem::size_of::<ClassMember>())
            }
        }
        #[repr(C)]
        pub struct ObjInstance {
            pub header: ObjectHeader,
            pub class: *mut ObjClass,
            pub values: IntMap<MemberId, Value>,
        }
        impl ObjInstance {
            pub fn new(class: *mut ObjClass) -> ObjInstance {
                let mut values = IntMap::default();
                for field in unsafe { &(*class).fields } {
                    values.insert(*field, Value::NULL);
                }
                ObjInstance {
                    header: ObjectHeader::new(ObjectKind::Instance),
                    class,
                    values,
                }
            }
            pub fn get(&self, id: MemberId) -> Value {
                match self.values.get(&id) {
                    Some(value) => *value,
                    None => {
                        let class = unsafe { &*self.class };
                        let func = class.get_method(id).unwrap();
                        Value::from(func)
                    }
                }
            }
            pub fn set(&mut self, id: MemberId, value: Value) {
                self.values.insert(id, value);
            }
        }
        impl GcTraceable for ObjInstance {
            fn fmt(&self) -> String {
                let class = unsafe { &*self.class };
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("<instance {0}>", class.fmt()),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                gc.mark_object(self.class);
                for (_, value) in &self.values {
                    value.mark(gc);
                }
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjClass>()
                    + self.values.capacity() * mem::size_of::<(MemberId, Value)>()
            }
        }
        #[repr(C)]
        pub struct ObjUpvalue {
            pub header: ObjectHeader,
            pub location: *mut Value,
            pub closed: Value,
        }
        impl ObjUpvalue {
            pub fn new(location: *mut Value) -> ObjUpvalue {
                ObjUpvalue {
                    header: ObjectHeader::new(ObjectKind::Upvalue),
                    location,
                    closed: Value::NULL,
                }
            }
            pub fn close(&mut self) {
                self.closed = unsafe { *self.location };
                self.location = &raw mut self.closed;
            }
        }
        impl GcTraceable for ObjUpvalue {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("<up {0}>", unsafe { &*self.location }),
                    );
                    res
                })
            }
            fn mark(&self, gc: &mut Gc) {
                unsafe { &*self.location }.mark(gc);
            }
            fn size(&self) -> usize {
                mem::size_of::<ObjUpvalue>()
            }
        }
    }
    mod vm {
        use std::collections::HashMap;
        use std::hash::BuildHasherDefault;
        use anyhow::bail;
        use rustc_hash::FxHasher;
        use crate::vm::objects::{ObjBoundMethod, ObjInstance};
        use crate::vm::value::ValueKind;
        use crate::lexer::{tokenize, TokenStream};
        use super::chunk::BytecodeChunk;
        use super::opcode::{self, OpCode};
        use super::parser::Parser;
        use super::stack::{CachedStack, Stack};
        use super::value::Value;
        use super::gc::{Gc, GcTraceable};
        use super::objects::{
            ClassMember, ObjClass, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue,
            Object, ObjectKind,
        };
        use super::compiler::Compiler;
        const MAX_STACK: usize = 16384;
        const MAX_FRAMES: usize = 256;
        pub struct CallFrame {
            closure: *mut ObjClosure,
            return_ip: *const OpCode,
            stack_start: *mut Value,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for CallFrame {
            #[inline]
            fn clone(&self) -> CallFrame {
                let _: ::core::clone::AssertParamIsClone<*mut ObjClosure>;
                let _: ::core::clone::AssertParamIsClone<*const OpCode>;
                let _: ::core::clone::AssertParamIsClone<*mut Value>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for CallFrame {}
        pub struct Vm<'out> {
            gc: Gc,
            ip: *const OpCode,
            chunk: BytecodeChunk,
            globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
            stack: Stack<Value, MAX_STACK>,
            frames: CachedStack<CallFrame, MAX_FRAMES>,
            open_upvalues: Vec<*mut ObjUpvalue>,
            out: &'out mut Vec<String>,
        }
        impl<'out> Vm<'out> {
            pub fn run(
                file_name: &str,
                src: &str,
            ) -> Result<Vec<String>, anyhow::Error> {
                let mut gc = Gc::new();
                let tokens = tokenize(String::from(file_name), String::from(src))?;
                let ast = Parser::parse(&mut gc, &mut TokenStream::new(&tokens))?;
                let chunk = Compiler::compile(&ast, &mut gc)?;
                let mut out = Vec::new();
                let mut vm = Vm {
                    gc,
                    ip: chunk.code.as_ptr(),
                    chunk,
                    globals: HashMap::default(),
                    stack: Stack::new(),
                    frames: CachedStack::new(),
                    open_upvalues: Vec::new(),
                    out: &mut out,
                };
                vm.stack.init();
                vm.frames.init();
                vm.frames
                    .push(CallFrame {
                        closure: std::ptr::null_mut(),
                        return_ip: std::ptr::null(),
                        stack_start: vm.stack.top(),
                    });
                vm.define_native(
                    "print",
                    1,
                    |vm| {
                        let value = vm.stack.peek(0);
                        let value_str = match value.kind() {
                            ValueKind::Null => String::from("null"),
                            ValueKind::Number => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!("{0}", value.as_number()),
                                    );
                                    res
                                })
                            }
                            ValueKind::Boolean => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!("{0}", value.as_bool()),
                                    );
                                    res
                                })
                            }
                            ValueKind::Object(_) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!("{0}", value.as_object().fmt()),
                                    );
                                    res
                                })
                            }
                        };
                        vm.out.push(value_str.clone());
                        {
                            ::std::io::_print(format_args!("{0}\n", value_str));
                        };
                        vm.stack.push(Value::NULL);
                    },
                );
                vm.define_native(
                    "time",
                    0,
                    |vm| {
                        let time = std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as f64;
                        vm.stack.push(Value::from(time));
                    },
                );
                vm.define_native(
                    "gcHeapSize",
                    0,
                    |vm| {
                        vm.stack.push(Value::from(vm.gc.bytes_allocated as f64));
                    },
                );
                vm.define_native(
                    "gcCollect",
                    0,
                    |vm| {
                        vm.start_gc();
                        vm.stack.push(Value::NULL);
                    },
                );
                vm.interpret()?;
                Ok(out)
            }
            fn stringify_frame(&self, frame: &CallFrame) -> String {
                let name = unsafe { &(*(*(*frame.closure).function).name).value };
                let pos = unsafe {
                    let idx = frame.return_ip.offset_from(self.chunk.code.as_ptr());
                    &self.chunk.code_pos[idx as usize]
                };
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!("\tat {0} ({1})", name, pos),
                    );
                    res
                })
            }
            fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
                let trace = self
                    .frames
                    .iter()
                    .skip(1)
                    .rev()
                    .map(|frame| self.stringify_frame(&frame))
                    .collect::<Vec<String>>()
                    .join("\n");
                let pos = unsafe {
                    let idx = self.ip.offset_from(self.chunk.code.as_ptr()) as usize - 1;
                    &self.chunk.code_pos[idx]
                };
                return ::anyhow::__private::Err({
                    use ::anyhow::__private::kind::*;
                    let error = match ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!("{0}\nat {1}\n{2}", message.into(), pos, trace),
                        );
                        res
                    }) {
                        error => (&error).anyhow_kind().new(error),
                    };
                    error
                });
            }
            fn intern(&mut self, name: impl Into<String>) -> *mut ObjString {
                if self.gc.should_collect() {
                    self.start_gc();
                }
                self.gc.intern(name)
            }
            fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
            where
                *mut T: Into<Object>,
            {
                if self.gc.should_collect() {
                    self.start_gc();
                }
                self.gc.alloc(obj)
            }
            fn define_native(
                &mut self,
                name: impl Into<String>,
                arity: u8,
                function: fn(&mut Vm),
            ) {
                let name_ref = self.gc.intern(name.into());
                let native = ObjNativeFn::new(name_ref, arity, function);
                let value = Value::from(self.gc.alloc(native));
                self.globals.insert(name_ref, value);
            }
            fn start_gc(&mut self) {
                self.chunk.mark(&mut self.gc);
                for (&name, value) in &self.globals {
                    self.gc.mark_object(name);
                    value.mark(&mut self.gc);
                }
                for &upvalue in &self.open_upvalues {
                    self.gc.mark_object(upvalue);
                }
                for value in self.stack.iter() {
                    value.mark(&mut self.gc);
                }
                self.gc.collect();
            }
            fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
                unsafe {
                    *(*self.frames.top.closure).upvalues.get_unchecked(idx as usize)
                }
            }
            fn capture_upvalue(&mut self, location: *mut Value) -> *mut ObjUpvalue {
                match self
                    .open_upvalues
                    .iter()
                    .find(|&&upvalue| unsafe { (*upvalue).location } == location)
                {
                    Some(&upvalue) => upvalue,
                    None => {
                        let upvalue = self.alloc(ObjUpvalue::new(location));
                        self.open_upvalues.push(upvalue);
                        upvalue
                    }
                }
            }
            fn close_upvalues(&mut self, after: *const Value) {
                for idx in (0..self.open_upvalues.len()).rev() {
                    unsafe {
                        let upvalue = *self.open_upvalues.get_unchecked(idx);
                        if after <= (*upvalue).location {
                            (*upvalue).close();
                            self.open_upvalues.swap_remove(idx);
                        }
                    }
                }
            }
            fn create_closure(&mut self, function: *mut ObjFn) -> *mut ObjClosure {
                let mut closure = ObjClosure::new(function);
                let upvalue_count = unsafe { (*function).upvalues.len() };
                for i in 0..upvalue_count {
                    let fn_upval = unsafe { &(*function).upvalues[i] };
                    let upvalue = if fn_upval.is_local {
                        self.capture_upvalue(unsafe {
                            self.frames.top.stack_start.add(fn_upval.location as usize)
                        })
                    } else {
                        self.get_upvalue(fn_upval.location as usize)
                    };
                    closure.upvalues.push(upvalue);
                }
                self.alloc(closure)
            }
            fn get_property(
                &mut self,
                instance_ref: *mut ObjInstance,
                prop: *mut ObjString,
            ) -> Option<Value> {
                let instance = unsafe { &*instance_ref };
                let class = unsafe { &*instance.class };
                match class.resolve(prop) {
                    Some(ClassMember::Field(id)) | Some(ClassMember::Method(id)) => {
                        Some(self.get_property_by_id(instance_ref, id))
                    }
                    None => None,
                }
            }
            fn get_property_by_id(
                &mut self,
                instance_ref: *mut ObjInstance,
                id: u8,
            ) -> Value {
                let value = unsafe { (*instance_ref).get(id) };
                match value.kind() {
                    ValueKind::Object(ObjectKind::Function) => {
                        let closure = self
                            .create_closure(unsafe { value.as_object().function });
                        let method = self
                            .alloc(ObjBoundMethod::new(instance_ref, closure));
                        Value::from(method)
                    }
                    _ => value,
                }
            }
            fn push_frame(
                &mut self,
                closure: *mut ObjClosure,
                stack_start: *mut Value,
                func: &ObjFn,
            ) {
                self.frames
                    .push(CallFrame {
                        closure,
                        return_ip: self.ip,
                        stack_start,
                    });
                self.ip = unsafe {
                    self.chunk.code.as_ptr().offset(func.ip_start as isize)
                };
            }
            fn call_native(
                &mut self,
                arg_count: usize,
                ptr: *mut ObjNativeFn,
            ) -> Result<(), anyhow::Error> {
                let func = unsafe { &*ptr };
                if arg_count != func.arity as usize {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "{0} expects {1} arguments, but got {2}",
                                        unsafe { &*func.name }.value,
                                        func.arity,
                                        arg_count,
                                    ),
                                );
                                res
                            }),
                        );
                }
                (func.function)(self);
                let result = self.stack.peek(0);
                self.stack.truncate(arg_count + 2);
                self.stack.push(result);
                Ok(())
            }
            fn call_closure(
                &mut self,
                arg_count: usize,
                closure: *mut ObjClosure,
            ) -> Result<(), anyhow::Error> {
                let func = unsafe { &*(*closure).function };
                if arg_count != func.arity as usize {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "{0} expects {1} arguments, but got {2}",
                                        unsafe { &*func.name }.value,
                                        func.arity,
                                        arg_count,
                                    ),
                                );
                                res
                            }),
                        );
                }
                self.push_frame(closure, self.stack.offset(arg_count), func);
                Ok(())
            }
            fn call_bound_method(
                &mut self,
                arg_count: usize,
                ptr: *mut ObjBoundMethod,
            ) -> Result<(), anyhow::Error> {
                let bound_method = unsafe { &*ptr };
                let func = unsafe { &*(*bound_method.closure).function };
                if arg_count != func.arity as usize {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "{0} expects {1} arguments, but got {2}",
                                        unsafe { &*func.name }.value,
                                        func.arity,
                                        arg_count,
                                    ),
                                );
                                res
                            }),
                        );
                }
                let stack_start = self
                    .stack
                    .set(arg_count, Value::from(bound_method.instance));
                self.push_frame(bound_method.closure, stack_start, func);
                Ok(())
            }
            fn call_class(
                &mut self,
                arg_count: usize,
                class_ptr: *mut ObjClass,
            ) -> Result<(), anyhow::Error> {
                let class = unsafe { &*class_ptr };
                let init_id = class.resolve_id(self.intern("init")).unwrap();
                let init_ref = class.get_method(init_id).unwrap();
                let init = unsafe { &*init_ref };
                if arg_count != init.arity as usize {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "{0} expects {1} arguments, but got {2}",
                                        unsafe { &*init.name }.value,
                                        init.arity,
                                        arg_count,
                                    ),
                                );
                                res
                            }),
                        );
                }
                let closure = self.create_closure(init_ref);
                self.stack.push(Value::from(closure));
                let instance = self.alloc(ObjInstance::new(class_ptr));
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure, stack_start, init);
                Ok(())
            }
            fn read_byte(&mut self) -> OpCode {
                let op = unsafe { *self.ip };
                self.ip = unsafe { self.ip.add(1) };
                op
            }
            fn interpret(&mut self) -> Result<(), anyhow::Error> {
                loop {
                    let op = self.read_byte();
                    match op {
                        opcode::RETURN => {
                            if !self.op_return() {
                                return Ok(());
                            }
                        }
                        opcode::POP => self.op_pop(),
                        opcode::CLOSE_UPVALUE => self.op_close_upvalue(),
                        opcode::CALL => self.op_call()?,
                        opcode::JUMP => self.op_jump(),
                        opcode::JUMP_IF_FALSE => self.op_jump_if_false(),
                        opcode::PUSH_CONSTANT => self.op_push_constant(),
                        opcode::PUSH_NULL => self.op_push_null(),
                        opcode::PUSH_TRUE => self.op_push_true(),
                        opcode::PUSH_FALSE => self.op_push_false(),
                        opcode::PUSH_CLOSURE => self.op_push_closure()?,
                        opcode::PUSH_CLASS => self.op_push_class(),
                        opcode::GET_GLOBAL => self.op_get_global()?,
                        opcode::SET_GLOBAL => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                        opcode::GET_LOCAL => self.op_get_local(),
                        opcode::SET_LOCAL => self.op_set_local(),
                        opcode::GET_UPVALUE => self.op_get_upvalue(),
                        opcode::SET_UPVALUE => self.op_set_upvalue(),
                        opcode::GET_PROPERTY => self.op_get_property()?,
                        opcode::SET_PROPERTY => self.op_set_property()?,
                        opcode::GET_PROPERTY_ID => self.op_get_property_by_id()?,
                        opcode::SET_PROPERTY_ID => self.op_set_property_by_id()?,
                        opcode::ADD => self.op_add()?,
                        opcode::SUBTRACT => self.op_subtract()?,
                        opcode::MULTIPLY => self.op_multiply()?,
                        opcode::DIVIDE => self.op_divide()?,
                        opcode::EQUAL => self.op_equal()?,
                        opcode::NOT_EQUAL => self.op_not_equal()?,
                        opcode::LESS_THAN => self.op_less_than()?,
                        opcode::LESS_THAN_EQUAL => self.op_less_than_equal()?,
                        opcode::GREATER_THAN => self.op_greater_than()?,
                        opcode::GREATER_THAN_EQUAL => self.op_greater_than_equal()?,
                        opcode::NEGATE => self.op_negate()?,
                        opcode::NOT => self.op_not()?,
                        opcode::AND => self.op_and()?,
                        opcode::OR => self.op_or()?,
                        opcode::LEFT_SHIFT => self.op_left_shift()?,
                        opcode::RIGHT_SHIFT => self.op_right_shift()?,
                        opcode::BIT_AND => self.op_bit_and()?,
                        opcode::BIT_OR => self.op_bit_or()?,
                        opcode::BIT_XOR => self.op_bit_xor()?,
                        opcode::BIT_NOT => self.op_bit_not()?,
                        _ => unsafe { std::hint::unreachable_unchecked() }
                    }
                }
            }
            fn op_return(&mut self) -> bool {
                if self.frames.len() == 1 {
                    return false;
                }
                let frame = self.frames.pop();
                self.ip = frame.return_ip;
                self.close_upvalues(frame.stack_start);
                let value = self.stack.pop();
                self.stack.set_top(frame.stack_start);
                self.stack.push(value);
                true
            }
            fn op_pop(&mut self) {
                self.stack.truncate(1);
            }
            fn op_close_upvalue(&mut self) {
                let location = self.read_byte() as usize;
                let p = unsafe { self.frames.top.stack_start.add(location) };
                self.close_upvalues(p);
                self.stack.truncate(1);
            }
            fn op_call(&mut self) -> Result<(), anyhow::Error> {
                let arg_count = self.read_byte() as usize;
                let value = self.stack.peek(arg_count);
                if !value.is_callable() {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("{0} is not callable", value.fmt()),
                                );
                                res
                            }),
                        );
                }
                let object = self.stack.peek(arg_count).as_object();
                let kind = object.kind();
                if kind == ObjectKind::Closure {
                    return self.call_closure(arg_count, object.as_closure());
                } else if kind == ObjectKind::NativeFunction {
                    return self.call_native(arg_count, object.as_native_function());
                } else if kind == ObjectKind::Class {
                    return self.call_class(arg_count, object.as_class());
                } else if kind == ObjectKind::BoundMethod {
                    return self.call_bound_method(arg_count, object.as_bound_method());
                }
                unsafe { std::hint::unreachable_unchecked() }
            }
            fn op_jump(&mut self) {
                let offset = ((self.read_byte() as u16)
                    | ((self.read_byte() as u16) << 8)) as isize;
                self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
            }
            fn op_jump_if_false(&mut self) {
                let offset = ((self.read_byte() as u16)
                    | ((self.read_byte() as u16) << 8)) as isize;
                let value = self.stack.pop();
                if value.is_bool() && !value.as_bool() {
                    self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
                }
            }
            fn op_push_constant(&mut self) {
                let const_idx = self.read_byte() as usize;
                let value = self.chunk.constants[const_idx];
                self.stack.push(value);
            }
            fn op_push_null(&mut self) {
                self.stack.push(Value::NULL)
            }
            fn op_push_true(&mut self) {
                self.stack.push(Value::TRUE)
            }
            fn op_push_false(&mut self) {
                self.stack.push(Value::FALSE)
            }
            fn op_push_closure(&mut self) -> Result<(), anyhow::Error> {
                let const_idx = self.read_byte() as usize;
                let value = self.chunk.constants[const_idx];
                let fn_ref = unsafe { value.as_object().function };
                let closure = self.create_closure(fn_ref);
                self.stack.push(Value::from(closure));
                Ok(())
            }
            fn op_push_class(&mut self) {
                let const_idx = self.read_byte() as usize;
                let value = self.chunk.constants[const_idx];
                self.stack.push(value);
            }
            fn op_get_global(&mut self) -> Result<(), anyhow::Error> {
                let const_idx = self.read_byte() as usize;
                let constant = &self.chunk.constants[const_idx];
                let string = constant.as_object().as_string();
                let value = self.globals[&string];
                self.stack.push(value);
                Ok(())
            }
            fn op_get_local(&mut self) {
                let idx = self.read_byte() as usize;
                let value = unsafe { *self.frames.top.stack_start.add(idx) };
                self.stack.push(value);
            }
            fn op_set_local(&mut self) {
                let idx = self.read_byte() as usize;
                unsafe {
                    *self.frames.top.stack_start.add(idx) = self.stack.peek(0);
                };
            }
            fn op_get_upvalue(&mut self) {
                let idx = self.read_byte() as usize;
                let upvalue = self.get_upvalue(idx);
                self.stack.push(unsafe { *(*upvalue).location });
            }
            fn op_set_upvalue(&mut self) {
                let idx = self.read_byte() as usize;
                let upvalue = self.get_upvalue(idx);
                unsafe { *(*upvalue).location = self.stack.peek(0) };
            }
            fn op_get_property(&mut self) -> Result<(), anyhow::Error> {
                let const_idx = self.read_byte() as usize;
                let value = self.stack.pop();
                if !match value.kind() {
                    ValueKind::Object(ObjectKind::Instance) => true,
                    _ => false,
                } {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("Invalid property access: {0}", value.fmt()),
                                );
                                res
                            }),
                        );
                }
                let object = value.as_object();
                let instance_ref = unsafe { object.instance };
                let prop = self.chunk.constants[const_idx].as_object().as_string();
                if let Some(value) = self.get_property(instance_ref, prop) {
                    self.stack.push(value);
                    Ok(())
                } else {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Property not found"),
                        );
                        error
                    })
                }
            }
            fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
                let const_idx = self.read_byte() as usize;
                let value = self.stack.pop();
                if !match value.kind() {
                    ValueKind::Object(ObjectKind::Instance) => true,
                    _ => false,
                } {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("Invalid property access: {0}", value.fmt()),
                                );
                                res
                            }),
                        );
                }
                let object = value.as_object();
                let instance_ref = unsafe { object.instance };
                let prop = self.chunk.constants[const_idx].as_object().as_string();
                let instance = unsafe { &mut *instance_ref };
                let class = unsafe { &*instance.class };
                match class.resolve(prop) {
                    Some(ClassMember::Field(id)) => {
                        let value = self.stack.peek(0);
                        instance.values.insert(id, value);
                        Ok(())
                    }
                    Some(ClassMember::Method(_)) => {
                        return self
                            .error(
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!(
                                            "Cannot assign to instance method \'{0}\'",
                                            unsafe { &*prop }.value,
                                        ),
                                    );
                                    res
                                }),
                            );
                    }
                    None => {
                        return ::anyhow::__private::Err({
                            let error = ::anyhow::__private::format_err(
                                format_args!("No such property"),
                            );
                            error
                        });
                    }
                }
            }
            fn op_get_property_by_id(&mut self) -> Result<(), anyhow::Error> {
                let member_id = self.read_byte();
                let value = self.stack.pop();
                if !match value.kind() {
                    ValueKind::Object(ObjectKind::Instance) => true,
                    _ => false,
                } {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("Invalid property access: {0}", value.fmt()),
                                );
                                res
                            }),
                        );
                }
                let object = value.as_object();
                let instance_ref = unsafe { object.instance };
                let value = self.get_property_by_id(instance_ref, member_id);
                self.stack.push(value);
                Ok(())
            }
            fn op_set_property_by_id(&mut self) -> Result<(), anyhow::Error> {
                let member_id = self.read_byte();
                let value = self.stack.pop();
                if !match value.kind() {
                    ValueKind::Object(ObjectKind::Instance) => true,
                    _ => false,
                } {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("Invalid property access: {0}", value.fmt()),
                                );
                                res
                            }),
                        );
                }
                let object = value.as_object();
                let instance_ref = unsafe { object.instance };
                let value = self.stack.peek(0);
                let instance = unsafe { &mut *instance_ref };
                instance.set(member_id, value);
                Ok(())
            }
            fn op_add(&mut self) -> Result<(), anyhow::Error> {
                let b = self.stack.pop();
                let a = self.stack.pop();
                let result = match (a.kind(), b.kind()) {
                    (ValueKind::Number, ValueKind::Number) => {
                        Value::from(a.as_number() + b.as_number())
                    }
                    (
                        ValueKind::Object(ObjectKind::String),
                        ValueKind::Object(ObjectKind::String),
                    ) => {
                        let sa = unsafe { &*a.as_object().as_string() }.value.as_str();
                        let sb = unsafe { &*b.as_object().as_string() }.value.as_str();
                        let s = [sa, sb].concat();
                        Value::from(self.intern(s))
                    }
                    _ => {
                        return self
                            .error(
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!(
                                            "Operator \'+\' cannot be applied to operands {0} and {1}",
                                            a,
                                            b,
                                        ),
                                    );
                                    res
                                }),
                            );
                    }
                };
                self.stack.push(result);
                Ok(())
            }
            fn op_subtract(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a - b), "-")
            }
            fn op_multiply(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a * b), "*")
            }
            fn op_divide(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a / b), "/")
            }
            fn op_equal(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op(|a, b| Value::from(a == b))
            }
            fn op_not_equal(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op(|a, b| Value::from(a != b))
            }
            fn op_less_than(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a < b), "<")
            }
            fn op_less_than_equal(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a <= b), "<=")
            }
            fn op_greater_than(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a > b), ">")
            }
            fn op_greater_than_equal(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|a, b| Value::from(a >= b), ">=")
            }
            fn op_negate(&mut self) -> Result<(), anyhow::Error> {
                let value = self.stack.pop();
                if !value.is_number() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Invalid operand"),
                        );
                        error
                    });
                }
                self.stack.push(Value::from(-value.as_number()));
                Ok(())
            }
            fn op_not(&mut self) -> Result<(), anyhow::Error> {
                let value = self.stack.pop();
                if !value.is_bool() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Invalid operand"),
                        );
                        error
                    });
                }
                self.stack.push(Value::from(!value.as_bool()));
                Ok(())
            }
            fn op_and(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op(|a, b| Value::from(a.as_bool() && b.as_bool()))
            }
            fn op_or(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op(|a, b| Value::from(a.as_bool() || b.as_bool()))
            }
            fn op_left_shift(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(
                    |a, b| Value::from(((a as i64) << (b as i64)) as f64),
                    "<<",
                )
            }
            fn op_right_shift(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(
                    |a, b| Value::from(((a as i64) >> (b as i64)) as f64),
                    ">>",
                )
            }
            fn op_bit_and(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(
                    |a, b| Value::from(((a as i64) & (b as i64)) as f64),
                    "&&",
                )
            }
            fn op_bit_or(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(
                    |a, b| Value::from(((a as i64) | (b as i64)) as f64),
                    "||",
                )
            }
            fn op_bit_xor(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(
                    |a, b| Value::from(((a as i64) ^ (b as i64)) as f64),
                    "^",
                )
            }
            fn op_bit_not(&mut self) -> Result<(), anyhow::Error> {
                let value = self.stack.pop();
                if !value.is_number() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Invalid operand"),
                        );
                        error
                    });
                }
                self.stack.push(Value::from(!(value.as_number() as i64) as f64));
                Ok(())
            }
            fn binary_op_number<F: Fn(f64, f64) -> Value>(
                &mut self,
                func: F,
                token: impl Into<String>,
            ) -> Result<(), anyhow::Error> {
                let b = self.stack.pop();
                let a = self.stack.pop();
                if !a.is_number() || !b.is_number() {
                    return self
                        .error(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Operator \'{0}\' cannot be applied to operands {1} and {2}",
                                        token.into(),
                                        a,
                                        b,
                                    ),
                                );
                                res
                            }),
                        );
                }
                self.stack.push(func(a.as_number(), b.as_number()));
                Ok(())
            }
            fn binary_op<F: Fn(Value, Value) -> Value>(
                &mut self,
                func: F,
            ) -> Result<(), anyhow::Error> {
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(func(a, b));
                Ok(())
            }
        }
    }
    mod gc {
        use std::mem;
        use fnv::FnvHashMap;
        use super::objects::{ObjString, Object};
        pub trait GcTraceable {
            fn fmt(&self) -> String;
            fn mark(&self, gc: &mut Gc);
            fn size(&self) -> usize;
        }
        impl GcTraceable for String {
            fn fmt(&self) -> String {
                ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(format_args!("\"{0}\"", self));
                    res
                })
            }
            fn mark(&self, _gc: &mut Gc) {}
            fn size(&self) -> usize {
                mem::size_of::<String>() + self.capacity()
            }
        }
        pub struct Gc {
            refs: Vec<Object>,
            strings: FnvHashMap<String, *mut ObjString>,
            reachable_refs: Vec<Object>,
            pub bytes_allocated: usize,
            next_gc: usize,
        }
        impl Gc {
            pub fn new() -> Gc {
                Gc {
                    refs: Vec::new(),
                    strings: FnvHashMap::default(),
                    reachable_refs: Vec::new(),
                    bytes_allocated: 0,
                    next_gc: 1024 * 1024,
                }
            }
            pub fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
            where
                *mut T: Into<Object>,
            {
                self.bytes_allocated += obj.size();
                let obj_ptr = Box::into_raw(Box::new(obj));
                let obj = obj_ptr.into();
                self.refs.push(obj);
                obj_ptr
            }
            pub fn intern(&mut self, name: impl Into<String>) -> *mut ObjString {
                let name = name.into();
                if let Some(&obj) = self.strings.get(&name) {
                    return obj;
                }
                let gc_ref = self.alloc(ObjString::new(name.clone()));
                self.strings.insert(name, gc_ref);
                gc_ref
            }
            pub fn mark_object<T: Into<Object>>(&mut self, obj: T) {
                let obj: Object = obj.into();
                unsafe {
                    if !(*obj.header).marked {
                        (*obj.header).marked = true;
                        self.reachable_refs.push(obj);
                    }
                }
            }
            pub fn collect(&mut self) {
                self.mark_reachable();
                self.sweep_strings();
                self.sweep_objects();
            }
            fn mark_reachable(&mut self) {
                while let Some(obj) = self.reachable_refs.pop() {
                    obj.mark(self);
                }
            }
            fn sweep_strings(&mut self) {
                self.strings
                    .retain(|_, &mut obj_ptr| unsafe { (*obj_ptr).header.marked });
            }
            fn sweep_objects(&mut self) {
                for i in (0..self.refs.len()).rev() {
                    let obj = &self.refs[i];
                    unsafe {
                        if (*obj.header).marked {
                            (*obj.header).marked = false;
                        } else {
                            self.free(i);
                        }
                    }
                }
            }
            fn free(&mut self, idx: usize) {
                self.bytes_allocated -= self.refs[idx].free();
                self.refs.swap_remove(idx);
            }
            pub fn should_collect(&self) -> bool {
                self.bytes_allocated > self.next_gc
            }
        }
    }
    mod operator {
        use std::fmt;
        use crate::lexer::{TokenStream, TokenType};
        use super::opcode;
        pub enum Operator {
            Add,
            Subtract,
            Multiply,
            Divide,
            LeftShift,
            RightShift,
            LessThan,
            LessThanEqual,
            GreaterThan,
            GreaterThanEqual,
            LogicalEqual,
            LogicalNotEqual,
            LogicalAnd,
            LogicalOr,
            BitAnd,
            BitOr,
            BitXor,
            Ternary,
            MemberAccess,
            Assign(Option<Box<Operator>>),
            Negate,
            LogicalNot,
            BitNot,
            Group,
            Call,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Operator {
            #[inline]
            fn clone(&self) -> Operator {
                match self {
                    Operator::Add => Operator::Add,
                    Operator::Subtract => Operator::Subtract,
                    Operator::Multiply => Operator::Multiply,
                    Operator::Divide => Operator::Divide,
                    Operator::LeftShift => Operator::LeftShift,
                    Operator::RightShift => Operator::RightShift,
                    Operator::LessThan => Operator::LessThan,
                    Operator::LessThanEqual => Operator::LessThanEqual,
                    Operator::GreaterThan => Operator::GreaterThan,
                    Operator::GreaterThanEqual => Operator::GreaterThanEqual,
                    Operator::LogicalEqual => Operator::LogicalEqual,
                    Operator::LogicalNotEqual => Operator::LogicalNotEqual,
                    Operator::LogicalAnd => Operator::LogicalAnd,
                    Operator::LogicalOr => Operator::LogicalOr,
                    Operator::BitAnd => Operator::BitAnd,
                    Operator::BitOr => Operator::BitOr,
                    Operator::BitXor => Operator::BitXor,
                    Operator::Ternary => Operator::Ternary,
                    Operator::MemberAccess => Operator::MemberAccess,
                    Operator::Assign(__self_0) => {
                        Operator::Assign(::core::clone::Clone::clone(__self_0))
                    }
                    Operator::Negate => Operator::Negate,
                    Operator::LogicalNot => Operator::LogicalNot,
                    Operator::BitNot => Operator::BitNot,
                    Operator::Group => Operator::Group,
                    Operator::Call => Operator::Call,
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Operator {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {
                    Operator::Add => ::core::fmt::Formatter::write_str(f, "Add"),
                    Operator::Subtract => {
                        ::core::fmt::Formatter::write_str(f, "Subtract")
                    }
                    Operator::Multiply => {
                        ::core::fmt::Formatter::write_str(f, "Multiply")
                    }
                    Operator::Divide => ::core::fmt::Formatter::write_str(f, "Divide"),
                    Operator::LeftShift => {
                        ::core::fmt::Formatter::write_str(f, "LeftShift")
                    }
                    Operator::RightShift => {
                        ::core::fmt::Formatter::write_str(f, "RightShift")
                    }
                    Operator::LessThan => {
                        ::core::fmt::Formatter::write_str(f, "LessThan")
                    }
                    Operator::LessThanEqual => {
                        ::core::fmt::Formatter::write_str(f, "LessThanEqual")
                    }
                    Operator::GreaterThan => {
                        ::core::fmt::Formatter::write_str(f, "GreaterThan")
                    }
                    Operator::GreaterThanEqual => {
                        ::core::fmt::Formatter::write_str(f, "GreaterThanEqual")
                    }
                    Operator::LogicalEqual => {
                        ::core::fmt::Formatter::write_str(f, "LogicalEqual")
                    }
                    Operator::LogicalNotEqual => {
                        ::core::fmt::Formatter::write_str(f, "LogicalNotEqual")
                    }
                    Operator::LogicalAnd => {
                        ::core::fmt::Formatter::write_str(f, "LogicalAnd")
                    }
                    Operator::LogicalOr => {
                        ::core::fmt::Formatter::write_str(f, "LogicalOr")
                    }
                    Operator::BitAnd => ::core::fmt::Formatter::write_str(f, "BitAnd"),
                    Operator::BitOr => ::core::fmt::Formatter::write_str(f, "BitOr"),
                    Operator::BitXor => ::core::fmt::Formatter::write_str(f, "BitXor"),
                    Operator::Ternary => ::core::fmt::Formatter::write_str(f, "Ternary"),
                    Operator::MemberAccess => {
                        ::core::fmt::Formatter::write_str(f, "MemberAccess")
                    }
                    Operator::Assign(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "Assign",
                            &__self_0,
                        )
                    }
                    Operator::Negate => ::core::fmt::Formatter::write_str(f, "Negate"),
                    Operator::LogicalNot => {
                        ::core::fmt::Formatter::write_str(f, "LogicalNot")
                    }
                    Operator::BitNot => ::core::fmt::Formatter::write_str(f, "BitNot"),
                    Operator::Group => ::core::fmt::Formatter::write_str(f, "Group"),
                    Operator::Call => ::core::fmt::Formatter::write_str(f, "Call"),
                }
            }
        }
        impl Operator {
            pub fn assign(op: Operator) -> Operator {
                return Operator::Assign(Some(Box::new(op)));
            }
            /// Parses a prefix operator on top of the token stream.
            /// The token stream is advanced only if a valid prefix operator is found.
            ///
            /// ## Arguments
            /// * `stream` - The token stream to parse
            /// * `min_precedence` - The minimum precedence of the operator to parse
            pub fn parse_prefix(
                stream: &mut TokenStream,
                min_precedence: u8,
            ) -> Option<Operator> {
                let op = match &stream.peek(0).kind {
                    TokenType::LeftParen => Operator::Group,
                    TokenType::Minus => Operator::Negate,
                    TokenType::Exclamation => Operator::LogicalNot,
                    TokenType::Tilde => Operator::BitNot,
                    _ => return None,
                };
                if op.has_lower_prefix_precedence(min_precedence) {
                    return None;
                }
                stream.next();
                return Some(op.clone());
            }
            /// Parses an infix operator on top of the token stream.
            /// The token stream is advanced only if a valid infix operator is found.
            ///
            /// ## Arguments
            /// * `stream` - The token stream to parse
            /// * `min_precedence` - The minimum precedence of the operator to parse
            pub fn parse_infix(
                stream: &mut TokenStream,
                min_precedence: u8,
            ) -> Option<Operator> {
                let mut current_op: Option<Operator> = None;
                let mut peek = 0;
                let op = loop {
                    current_op = match (&current_op, &stream.peek(peek).kind) {
                        (None, TokenType::LessThan) => Some(Operator::LessThan),
                        (None, TokenType::GreaterThan) => Some(Operator::GreaterThan),
                        (None, TokenType::Plus) => Some(Operator::Add),
                        (None, TokenType::Minus) => Some(Operator::Subtract),
                        (None, TokenType::Multiply) => Some(Operator::Multiply),
                        (None, TokenType::Divide) => Some(Operator::Divide),
                        (None, TokenType::Amp) => Some(Operator::BitAnd),
                        (None, TokenType::Pipe) => Some(Operator::BitOr),
                        (None, TokenType::Hat) => Some(Operator::BitXor),
                        (None, TokenType::Equal) => Some(Operator::Assign(None)),
                        (None, TokenType::Exclamation) => Some(Operator::LogicalNot),
                        (None, TokenType::Question) => Some(Operator::Ternary),
                        (None, TokenType::Dot) => Some(Operator::MemberAccess),
                        (Some(Operator::LessThan), TokenType::LessThan) => {
                            Some(Operator::LeftShift)
                        }
                        (Some(Operator::GreaterThan), TokenType::GreaterThan) => {
                            Some(Operator::RightShift)
                        }
                        (Some(Operator::BitAnd), TokenType::Amp) => {
                            Some(Operator::LogicalAnd)
                        }
                        (Some(Operator::BitOr), TokenType::Pipe) => {
                            Some(Operator::LogicalOr)
                        }
                        (Some(Operator::LogicalNot), TokenType::Equal) => {
                            Some(Operator::LogicalNotEqual)
                        }
                        (
                            Some(
                                assign_op @ (Operator::LogicalOr
                                | Operator::LogicalAnd
                                | Operator::Add
                                | Operator::Subtract
                                | Operator::Multiply
                                | Operator::Divide
                                | Operator::BitAnd
                                | Operator::BitOr
                                | Operator::BitXor
                                | Operator::LeftShift
                                | Operator::RightShift),
                            ),
                            TokenType::Equal,
                        ) => Some(Operator::assign(assign_op.clone())),
                        (Some(Operator::Assign(None)), TokenType::Equal) => {
                            Some(Operator::LogicalEqual)
                        }
                        (Some(Operator::LessThan), TokenType::Equal) => {
                            Some(Operator::LessThanEqual)
                        }
                        (Some(Operator::GreaterThan), TokenType::Equal) => {
                            Some(Operator::GreaterThanEqual)
                        }
                        (Some(op), _) if op.infix_precedence().is_some() => break op,
                        (_, _) => return None,
                    };
                    peek += 1;
                };
                if op.has_lower_infix_precedence(min_precedence) {
                    return None;
                }
                stream.advance(peek);
                return Some(op.clone());
            }
            /// Parses a postfix operator on top of the token stream.
            /// The token stream is advanced only if a valid postfix operator is found.
            ///
            /// ## Arguments
            /// * `stream` - The token stream to parse
            /// * `min_precedence` - The minimum precedence of the operator to parse
            pub fn parse_postfix(
                stream: &mut TokenStream,
                min_precedence: u8,
            ) -> Option<Operator> {
                let op = match &stream.peek(0).kind {
                    TokenType::LeftParen => Operator::Call,
                    _ => return None,
                };
                if op.has_lower_postfix_precedence(min_precedence) {
                    return None;
                }
                stream.next();
                return Some(op.clone());
            }
            fn has_lower_prefix_precedence(&self, precedence: u8) -> bool {
                let prec = self.prefix_precedence().unwrap();
                return prec < precedence
                    || (self.is_left_associative() && prec == precedence);
            }
            fn has_lower_infix_precedence(&self, precedence: u8) -> bool {
                let prec = self.infix_precedence().unwrap();
                return prec < precedence
                    || (self.is_left_associative() && prec == precedence);
            }
            fn has_lower_postfix_precedence(&self, precedence: u8) -> bool {
                let prec = self.postfix_precedence().unwrap();
                return prec < precedence
                    || (self.is_left_associative() && prec == precedence);
            }
            pub fn infix_precedence(&self) -> Option<u8> {
                let bp = match self {
                    Operator::Assign(_) => 1,
                    Operator::Ternary => 2,
                    Operator::LogicalOr => 3,
                    Operator::LogicalAnd => 4,
                    Operator::LogicalEqual | Operator::LogicalNotEqual => 5,
                    Operator::BitOr => 6,
                    Operator::BitXor => 7,
                    Operator::BitAnd => 8,
                    Operator::LessThan
                    | Operator::LessThanEqual
                    | Operator::GreaterThan
                    | Operator::GreaterThanEqual => 9,
                    Operator::LeftShift | Operator::RightShift => 10,
                    Operator::Add | Operator::Subtract => 11,
                    Operator::Multiply | Operator::Divide => 12,
                    Operator::MemberAccess => 16,
                    _ => return None,
                };
                Some(bp)
            }
            pub fn prefix_precedence(&self) -> Option<u8> {
                let bp = match self {
                    Operator::Negate | Operator::LogicalNot | Operator::BitNot => 13,
                    Operator::Group => 16,
                    _ => return None,
                };
                Some(bp)
            }
            pub fn postfix_precedence(&self) -> Option<u8> {
                let bp = match self {
                    Operator::Call => 16,
                    _ => return None,
                };
                Some(bp)
            }
            fn is_left_associative(&self) -> bool {
                return match self {
                    Operator::Assign(_) => false,
                    Operator::Ternary => false,
                    Operator::Negate => false,
                    Operator::LogicalNot => false,
                    Operator::BitNot => false,
                    Operator::Group => false,
                    _ => true,
                };
            }
            pub fn as_opcode(&self) -> opcode::OpCode {
                return match self {
                    Operator::Add => opcode::ADD,
                    Operator::Subtract => opcode::SUBTRACT,
                    Operator::Multiply => opcode::MULTIPLY,
                    Operator::Divide => opcode::DIVIDE,
                    Operator::LeftShift => opcode::LEFT_SHIFT,
                    Operator::RightShift => opcode::RIGHT_SHIFT,
                    Operator::LessThan => opcode::LESS_THAN,
                    Operator::LessThanEqual => opcode::LESS_THAN_EQUAL,
                    Operator::GreaterThan => opcode::GREATER_THAN,
                    Operator::GreaterThanEqual => opcode::GREATER_THAN_EQUAL,
                    Operator::LogicalEqual => opcode::EQUAL,
                    Operator::LogicalNotEqual => opcode::NOT_EQUAL,
                    Operator::LogicalAnd => opcode::AND,
                    Operator::LogicalOr => opcode::OR,
                    Operator::BitAnd => opcode::BIT_AND,
                    Operator::BitOr => opcode::BIT_OR,
                    Operator::BitXor => opcode::BIT_XOR,
                    _ => {
                        ::core::panicking::panic_fmt(format_args!("Invalid operator"));
                    }
                };
            }
        }
        impl fmt::Display for Operator {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                return f
                    .write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                Operator::Add => "+",
                                Operator::Subtract => "-",
                                Operator::Multiply => "*",
                                Operator::Divide => "/",
                                Operator::LeftShift => "<<",
                                Operator::RightShift => ">>",
                                Operator::LessThan => "<",
                                Operator::LessThanEqual => "<=",
                                Operator::GreaterThan => ">",
                                Operator::GreaterThanEqual => ">=",
                                Operator::LogicalEqual => "==",
                                Operator::LogicalNotEqual => "!=",
                                Operator::LogicalAnd => "&&",
                                Operator::LogicalOr => "||",
                                Operator::BitAnd => "&",
                                Operator::BitOr => "|",
                                Operator::BitXor => "^",
                                Operator::Ternary => "?:",
                                Operator::Negate => "-",
                                Operator::LogicalNot => "!",
                                Operator::BitNot => "~",
                                Operator::Group => "<group>",
                                Operator::Call => "<call>",
                                Operator::MemberAccess => ".",
                                Operator::Assign(None) => "=",
                                Operator::Assign(Some(op)) => {
                                    match **op {
                                        Operator::Add => "+=",
                                        Operator::Subtract => "-=",
                                        Operator::LeftShift => "<<=",
                                        Operator::RightShift => ">>=",
                                        Operator::Multiply => "*=",
                                        Operator::Divide => "/=",
                                        Operator::BitAnd => "&=",
                                        Operator::BitOr => "|=",
                                        Operator::BitXor => "^=",
                                        Operator::LogicalAnd => "&&=",
                                        Operator::LogicalOr => "||=",
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Invalid assignment operator"),
                                            );
                                        }
                                    }
                                }
                            },
                        ),
                    );
            }
        }
    }
    mod value {
        use std::{fmt, mem};
        use super::gc::{Gc, GcTraceable};
        use super::objects::{Object, ObjectHeader, ObjectKind};
        pub enum ValueKind {
            Null,
            Number,
            Boolean,
            Object(ObjectKind),
        }
        impl fmt::Display for ValueKind {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_fmt(
                    format_args!(
                        "{0}",
                        match self {
                            ValueKind::Null => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("null"));
                                    res
                                })
                            }
                            ValueKind::Number => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("number"));
                                    res
                                })
                            }
                            ValueKind::Boolean => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("boolean"));
                                    res
                                })
                            }
                            ValueKind::Object(kind) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("{0}", kind));
                                    res
                                })
                            }
                        },
                    ),
                )
            }
        }
        /// NaN boxed value
        pub struct Value(u64);
        #[automatically_derived]
        impl ::core::clone::Clone for Value {
            #[inline]
            fn clone(&self) -> Value {
                let _: ::core::clone::AssertParamIsClone<u64>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for Value {}
        #[automatically_derived]
        impl ::core::cmp::Eq for Value {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<u64>;
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Value {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Value {
            #[inline]
            fn eq(&self, other: &Value) -> bool {
                self.0 == other.0
            }
        }
        impl Value {
            /**
     * 0x7FF8000000000000 is the QNaN representation of a 64-bit float.
     * A value represents a number if its QNaN bits are not set.
     *
     * We also reserve an additional bit to differentiate between NaNs and
     * other value types in Clisay, like booleans and objects.
     *
     * If these bits are set, the value does not represent an f64.
     */
            const NAN_MASK: u64 = 0x7FFC000000000000;
            const SIGN: u64 = 1 << 63;
            /**
     * The sign and QNaN bits are set for object values.
     * This takes 14 bits, leaving room for a 50-bit pointer.
     *
     * Technically 64-bit architectures have 64-bit pointers, but in practice
     * common architectures only use the first 48 bits.
     */
            const OBJECT_MASK: u64 = Self::SIGN | Self::NAN_MASK;
            const PTR_MASK: u64 = 0x0000FFFFFFFFFFFF;
            const CALLABLE_MASK: u64 = Self::OBJECT_MASK | (1 << 48);
            pub const NULL: Self = Self(Self::NAN_MASK | 0b01);
            pub const TRUE: Self = Self(Self::NAN_MASK | 0b10);
            pub const FALSE: Self = Self(Self::NAN_MASK | 0b11);
            pub fn kind(self) -> ValueKind {
                if self.is_null() {
                    ValueKind::Null
                } else if self.is_bool() {
                    ValueKind::Boolean
                } else if self.is_number() {
                    ValueKind::Number
                } else if self.is_object() {
                    ValueKind::Object(self.as_object().kind())
                } else {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "internal error: entered unreachable code: {0}",
                                format_args!("Invalid value type"),
                            ),
                        );
                    }
                }
            }
            pub fn is_number(self) -> bool {
                (self.0 & Self::NAN_MASK) != Self::NAN_MASK
            }
            pub fn is_bool(self) -> bool {
                Self(self.0 | 0b01) == Self::FALSE
            }
            pub fn is_null(self) -> bool {
                self == Self::NULL
            }
            pub fn is_callable(self) -> bool {
                (self.0 & Self::CALLABLE_MASK) == Self::CALLABLE_MASK
            }
            pub fn is_object(self) -> bool {
                (self.0 & Self::OBJECT_MASK) == Self::OBJECT_MASK
            }
            pub fn as_number(self) -> f64 {
                f64::from_bits(self.0)
            }
            pub fn as_bool(self) -> bool {
                self == Self::TRUE
            }
            pub fn as_object(self) -> Object {
                Object {
                    header: (self.0 & Self::PTR_MASK) as *mut ObjectHeader,
                }
            }
        }
        impl From<f64> for Value {
            fn from(value: f64) -> Self {
                Self(value.to_bits())
            }
        }
        impl From<bool> for Value {
            fn from(value: bool) -> Self {
                if value { Self::TRUE } else { Self::FALSE }
            }
        }
        impl<T: Into<Object>> From<T> for Value {
            fn from(object: T) -> Self {
                let object: Object = object.into();
                let mask = match object.kind() {
                    ObjectKind::BoundMethod
                    | ObjectKind::NativeFunction
                    | ObjectKind::Closure
                    | ObjectKind::Class => Self::CALLABLE_MASK,
                    _ => Self::OBJECT_MASK,
                };
                Self((unsafe { object.header } as u64) | mask)
            }
        }
        impl GcTraceable for Value {
            fn fmt(&self) -> String {
                match self.kind() {
                    ValueKind::Null => {
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(format_args!("null"));
                            res
                        })
                    }
                    ValueKind::Number => {
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!("{0}", self.as_number()),
                            );
                            res
                        })
                    }
                    ValueKind::Boolean => {
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!("{0}", self.as_bool()),
                            );
                            res
                        })
                    }
                    ValueKind::Object(_) => self.as_object().fmt(),
                }
            }
            fn mark(&self, gc: &mut Gc) {
                if self.is_object() {
                    self.as_object().mark(gc);
                }
            }
            fn size(&self) -> usize {
                mem::size_of::<Value>()
            }
        }
        impl fmt::Display for Value {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_fmt(
                    format_args!(
                        "{0}",
                        match self.kind() {
                            ValueKind::Null => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("null"));
                                    res
                                })
                            }
                            ValueKind::Number => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("number"));
                                    res
                                })
                            }
                            ValueKind::Boolean => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("boolean"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::BoundMethod) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("function"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::Function) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("function"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::NativeFunction) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("function"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::Closure) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("function"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::Class) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("class"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::Instance) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("instance"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::String) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("string"));
                                    res
                                })
                            }
                            ValueKind::Object(ObjectKind::Upvalue) => {
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(format_args!("upvalue"));
                                    res
                                })
                            }
                        },
                    ),
                )
            }
        }
    }
    mod parser {
        use std::collections::HashSet;
        use std::{any::Any, marker::PhantomData};
        use anyhow::bail;
        use crate::lexer::{SourcePosition, TokenStream, TokenType};
        use super::gc::Gc;
        use super::objects::ObjString;
        use super::operator::Operator;
        pub enum Literal {
            Null,
            Boolean(bool),
            Number(f64),
            String(*mut ObjString),
        }
        pub trait ASTKind {
            fn as_any(&self) -> &dyn Any;
            fn as_any_mut(&mut self) -> &mut dyn Any;
        }
        pub enum Expr {
            Literal(Literal),
            Identifier(*mut ObjString),
            Unary(Operator, ASTId<Expr>),
            Binary(Operator, ASTId<Expr>, ASTId<Expr>),
            Ternary(ASTId<Expr>, ASTId<Expr>, ASTId<Expr>),
            Call(ASTId<Expr>, Vec<ASTId<Expr>>),
            This,
            Super,
        }
        impl ASTKind for Expr {
            fn as_any(&self) -> &dyn Any {
                self
            }
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }
        }
        pub struct FieldInit {
            pub name: *mut ObjString,
            pub value: Option<ASTId<Expr>>,
        }
        pub struct FnDecl {
            pub name: *mut ObjString,
            pub params: Vec<*mut ObjString>,
            pub body: ASTId<Stmt>,
        }
        pub struct ClassDecl {
            pub name: *mut ObjString,
            pub superclass: Option<*mut ObjString>,
            pub init: ASTId<Stmt>,
            pub fields: HashSet<*mut ObjString>,
            pub methods: Vec<ASTId<Stmt>>,
        }
        pub enum Stmt {
            Block(Vec<ASTId<Stmt>>),
            Expression(ASTId<Expr>),
            Return(Option<ASTId<Expr>>),
            Say(FieldInit),
            Fn(Box<FnDecl>),
            Class(Box<ClassDecl>),
            If(ASTId<Expr>, ASTId<Stmt>, Option<ASTId<Stmt>>),
            While(ASTId<Expr>, ASTId<Stmt>),
        }
        impl ASTKind for Stmt {
            fn as_any(&self) -> &dyn Any {
                self
            }
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }
        }
        pub struct ASTNode {
            pub pos: SourcePosition,
            pub value: Box<dyn ASTKind>,
        }
        pub struct ASTId<T> {
            id: usize,
            _marker: PhantomData<T>,
        }
        impl Copy for ASTId<Stmt> {}
        impl Clone for ASTId<Stmt> {
            fn clone(&self) -> ASTId<Stmt> {
                *self
            }
        }
        impl Copy for ASTId<Expr> {}
        impl Clone for ASTId<Expr> {
            fn clone(&self) -> ASTId<Expr> {
                *self
            }
        }
        pub struct AST {
            nodes: Vec<ASTNode>,
        }
        impl AST {
            pub fn get<T: 'static>(&self, id: &ASTId<T>) -> &T {
                self.nodes[id.id].value.as_any().downcast_ref::<T>().unwrap()
            }
            pub fn get_mut<T: 'static>(&mut self, id: &ASTId<T>) -> &mut T {
                self.nodes[id.id].value.as_any_mut().downcast_mut::<T>().unwrap()
            }
            pub fn pos<T: 'static>(&self, id: &ASTId<T>) -> &SourcePosition {
                &self.nodes[id.id].pos
            }
            pub fn get_root(&self) -> ASTId<Stmt> {
                ASTId {
                    id: self.nodes.len() - 1,
                    _marker: PhantomData,
                }
            }
            fn add_stmt(&mut self, kind: Stmt, pos: SourcePosition) -> ASTId<Stmt> {
                let node = ASTNode {
                    value: Box::new(kind),
                    pos,
                };
                self.nodes.push(node);
                return ASTId {
                    id: self.nodes.len() - 1,
                    _marker: PhantomData,
                };
            }
            fn add_expr(&mut self, kind: Expr, pos: SourcePosition) -> ASTId<Expr> {
                let node = ASTNode {
                    value: Box::new(kind),
                    pos,
                };
                self.nodes.push(node);
                return ASTId {
                    id: self.nodes.len() - 1,
                    _marker: PhantomData,
                };
            }
        }
        pub struct Parser<'parser, 'vm> {
            gc: &'vm mut Gc,
            tokens: &'vm mut TokenStream<'vm>,
            ast: &'parser mut AST,
            current_class: Option<*mut ObjString>,
        }
        impl<'parser, 'vm> Parser<'parser, 'vm> {
            pub fn parse(
                gc: &'vm mut Gc,
                tokens: &'vm mut TokenStream<'vm>,
            ) -> Result<AST, anyhow::Error> {
                let mut ast = AST { nodes: Vec::new() };
                let mut parser = Parser {
                    gc,
                    tokens,
                    ast: &mut ast,
                    current_class: None,
                };
                let pos = parser.tokens.peek(0).pos.clone();
                let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
                while parser.tokens.has_next() {
                    stmts.push(parser.parse_stmt()?);
                }
                ast.add_stmt(Stmt::Block(stmts), pos);
                Ok(ast)
            }
            fn parse_identifier(&mut self) -> Result<*mut ObjString, anyhow::Error> {
                let token = self.tokens.expect(TokenType::Identifier)?;
                Ok(self.gc.intern(token.lexeme.clone()))
            }
            fn parse_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                match self.tokens.peek(0).kind {
                    TokenType::Say => self.parse_say(),
                    TokenType::Fn => self.parse_fn(),
                    TokenType::Class => self.parse_class(),
                    TokenType::If => self.parse_if(),
                    TokenType::While => self.parse_while(),
                    TokenType::Return => self.parse_return(),
                    TokenType::LeftBrace => self.parse_block(),
                    _ => self.parse_expr_stmt(),
                }
            }
            fn parse_say(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::Say)?.pos.clone();
                let name = self.parse_identifier()?;
                let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.tokens.expect(TokenType::Semicolon)?;
                let field_init = FieldInit { name, value: expr };
                Ok(self.ast.add_stmt(Stmt::Say(field_init), pos))
            }
            fn parse_fn(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
                let name = self.parse_identifier()?;
                let params = self.parse_params()?;
                let body = self.parse_block()?;
                let fn_decl = Box::new(FnDecl { name, params, body });
                Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
            }
            fn parse_init(
                &mut self,
                has_superclass: bool,
            ) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::Init)?.pos.clone();
                let name = self
                    .gc
                    .intern(
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "{0}.init",
                                    unsafe { &*self.current_class.unwrap() }.value,
                                ),
                            );
                            res
                        }),
                    );
                let params = self.parse_params()?;
                self.tokens.expect(TokenType::LeftBrace)?;
                let mut stmts = if has_superclass {
                    match (self.tokens.peek(0).kind, self.tokens.peek(1).kind) {
                        (TokenType::Super, TokenType::LeftParen) => {
                            <[_]>::into_vec(
                                #[rustc_box]
                                ::alloc::boxed::Box::new([self.parse_stmt()?]),
                            )
                        }
                        _ => {
                            <[_]>::into_vec(
                                #[rustc_box]
                                ::alloc::boxed::Box::new([
                                    self.virtual_super_call(pos.clone())?,
                                ]),
                            )
                        }
                    }
                } else {
                    Vec::new()
                };
                stmts.extend(self.parse_stmts()?);
                self.tokens.expect(TokenType::RightBrace)?;
                let body = self.ast.add_stmt(Stmt::Block(stmts), pos.clone());
                let fn_decl = Box::new(FnDecl { name, params, body });
                Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
            }
            fn parse_params(&mut self) -> Result<Vec<*mut ObjString>, anyhow::Error> {
                self.tokens.expect(TokenType::LeftParen)?;
                let mut params: Vec<*mut ObjString> = Vec::new();
                while !self.tokens.match_next(TokenType::RightParen) {
                    if params.len() > 0 {
                        self.tokens.expect(TokenType::Comma)?;
                    }
                    params.push(self.parse_identifier()?);
                }
                self.tokens.expect(TokenType::RightParen)?;
                Ok(params)
            }
            fn virtual_super_call(
                &mut self,
                pos: SourcePosition,
            ) -> Result<ASTId<Stmt>, anyhow::Error> {
                let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
                let expr = self
                    .ast
                    .add_expr(Expr::Call(super_expr, Vec::new()), pos.clone());
                Ok(self.ast.add_stmt(Stmt::Expression(expr), pos.clone()))
            }
            fn parse_class(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::Class)?.pos.clone();
                let name = self.parse_identifier()?;
                let prev_class = self.current_class.replace(name);
                let superclass = match self.tokens.next_if(TokenType::Colon) {
                    Some(_) => Some(self.parse_identifier()?),
                    None => None,
                };
                self.tokens.expect(TokenType::LeftBrace)?;
                let mut fields: HashSet<*mut ObjString> = HashSet::default();
                let mut field_stmts: Vec<ASTId<Stmt>> = Vec::new();
                let mut method_stmts: Vec<ASTId<Stmt>> = Vec::new();
                let mut init = None;
                while !self.tokens.match_next(TokenType::RightBrace) {
                    if self.tokens.match_next(TokenType::Init) {
                        init = Some(self.parse_init(superclass.is_some())?);
                    } else if self.tokens.match_next(TokenType::Fn) {
                        method_stmts.push(self.parse_fn()?);
                    } else {
                        let name = self.parse_identifier()?;
                        let value = if let Some(_) = self
                            .tokens
                            .next_if(TokenType::Equal)
                        {
                            Some(self.parse_expr()?)
                        } else {
                            None
                        };
                        self.tokens.expect(TokenType::Semicolon)?;
                        fields.insert(name);
                        if let Some(value) = value {
                            let id = self
                                .ast
                                .add_expr(Expr::Identifier(name), pos.clone());
                            let assign = self
                                .ast
                                .add_expr(
                                    Expr::Binary(Operator::Assign(None), id, value),
                                    pos.clone(),
                                );
                            field_stmts
                                .push(
                                    self.ast.add_stmt(Stmt::Expression(assign), pos.clone()),
                                );
                        }
                    }
                }
                self.tokens.expect(TokenType::RightBrace)?;
                let init = match init {
                    Some(stmt_id) => stmt_id,
                    None => {
                        let stmts = if superclass.is_some() {
                            <[_]>::into_vec(
                                #[rustc_box]
                                ::alloc::boxed::Box::new([
                                    self.virtual_super_call(pos.clone())?,
                                ]),
                            )
                        } else {
                            Vec::new()
                        };
                        let body = self.ast.add_stmt(Stmt::Block(stmts), pos.clone());
                        let fn_decl = Box::new(FnDecl {
                            name,
                            params: Vec::new(),
                            body,
                        });
                        self.ast.add_stmt(Stmt::Fn(fn_decl), pos.clone())
                    }
                };
                let Stmt::Fn(fn_decl) = self.ast.get(&init) else {
                    ::core::panicking::panic("internal error: entered unreachable code")
                };
                let Stmt::Block(body) = self.ast.get_mut(&fn_decl.body.clone()) else {
                    ::core::panicking::panic("internal error: entered unreachable code")
                };
                body.splice(0..0, field_stmts.iter().cloned());
                let class_decl = Box::new(ClassDecl {
                    name,
                    superclass,
                    init,
                    fields,
                    methods: method_stmts,
                });
                self.current_class = prev_class;
                Ok(self.ast.add_stmt(Stmt::Class(class_decl), pos))
            }
            fn parse_if(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::If)?.pos.clone();
                let condition = self.parse_expr()?;
                let then = match self.tokens.match_next(TokenType::LeftBrace) {
                    true => self.parse_block()?,
                    false => self.parse_expr_stmt()?,
                };
                let otherwise = match self.tokens.next_if(TokenType::Else) {
                    Some(_) => {
                        match self.tokens.peek(0).kind {
                            TokenType::LeftBrace => Some(self.parse_block()?),
                            TokenType::If => Some(self.parse_if()?),
                            _ => Some(self.parse_expr_stmt()?),
                        }
                    }
                    None => None,
                };
                Ok(self.ast.add_stmt(Stmt::If(condition, then, otherwise), pos))
            }
            fn parse_while(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::While)?.pos.clone();
                let condition = self.parse_expr()?;
                let body = match self.tokens.match_next(TokenType::LeftBrace) {
                    true => self.parse_block()?,
                    false => self.parse_expr_stmt()?,
                };
                Ok(self.ast.add_stmt(Stmt::While(condition, body), pos))
            }
            fn parse_return(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
                let expr = match self.tokens.match_next(TokenType::Semicolon) {
                    true => None,
                    false => Some(self.parse_expr()?),
                };
                self.tokens.expect(TokenType::Semicolon)?;
                Ok(self.ast.add_stmt(Stmt::Return(expr), pos))
            }
            fn parse_block(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
                let stmts = self.parse_stmts()?;
                self.tokens.expect(TokenType::RightBrace)?;
                Ok(self.ast.add_stmt(Stmt::Block(stmts), pos))
            }
            fn parse_stmts(&mut self) -> Result<Vec<ASTId<Stmt>>, anyhow::Error> {
                let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
                while !self.tokens.match_next(TokenType::RightBrace) {
                    stmts.push(self.parse_stmt()?);
                }
                Ok(stmts)
            }
            fn parse_expr_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
                let pos = self.tokens.peek(0).pos.clone();
                let expr = self.parse_expr()?;
                self.tokens.expect(TokenType::Semicolon)?;
                Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
            }
            fn parse_expr(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
                self.parse_expr_precedence(0)
            }
            fn parse_expr_precedence(
                &mut self,
                min_precedence: u8,
            ) -> Result<ASTId<Expr>, anyhow::Error> {
                let mut left = match Operator::parse_prefix(self.tokens, 0) {
                    Some(op) => self.parse_expr_prefix(op)?,
                    _ => self.parse_expr_atom()?,
                };
                loop {
                    if let Some(op) = Operator::parse_postfix(
                        self.tokens,
                        min_precedence,
                    ) {
                        left = self.parse_expr_postfix(op, left)?;
                    } else if let Some(op) = Operator::parse_infix(
                        self.tokens,
                        min_precedence,
                    ) {
                        left = self.parse_expr_infix(op, left)?;
                    } else {
                        break;
                    }
                }
                Ok(left)
            }
            fn parse_expr_infix(
                &mut self,
                op: Operator,
                expr: ASTId<Expr>,
            ) -> Result<ASTId<Expr>, anyhow::Error> {
                let pos = self.ast.pos(&expr).clone();
                let kind = match &op {
                    Operator::Assign(Some(assign_op)) => {
                        let mut right = self
                            .parse_expr_precedence(op.infix_precedence().unwrap())?;
                        let kind = Expr::Binary(assign_op.as_ref().clone(), expr, right);
                        right = self.ast.add_expr(kind, pos.clone());
                        Expr::Binary(op, expr, right)
                    }
                    Operator::Ternary => {
                        let left = self.parse_expr_precedence(0)?;
                        self.tokens.expect(TokenType::Colon)?;
                        let right = self.parse_expr_precedence(0)?;
                        Expr::Ternary(expr, left, right)
                    }
                    _ => {
                        let right = self
                            .parse_expr_precedence(op.infix_precedence().unwrap())?;
                        Expr::Binary(op, expr, right)
                    }
                };
                Ok(self.ast.add_expr(kind, pos.clone()))
            }
            fn parse_expr_prefix(
                &mut self,
                op: Operator,
            ) -> Result<ASTId<Expr>, anyhow::Error> {
                let pos = self.tokens.peek(0).pos.clone();
                let kind = match &op {
                    Operator::Group => {
                        let expr = self.parse_expr_precedence(0)?;
                        self.tokens.expect(TokenType::RightParen)?;
                        return Ok(expr);
                    }
                    _ => {
                        let right = self
                            .parse_expr_precedence(op.prefix_precedence().unwrap())?;
                        Expr::Unary(op, right)
                    }
                };
                Ok(self.ast.add_expr(kind, pos))
            }
            fn parse_expr_postfix(
                &mut self,
                op: Operator,
                expr: ASTId<Expr>,
            ) -> Result<ASTId<Expr>, anyhow::Error> {
                let pos = self.ast.pos(&expr).clone();
                match op {
                    Operator::Call => {
                        let args = self.parse_args()?;
                        Ok(self.ast.add_expr(Expr::Call(expr, args), pos))
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
            fn parse_args(&mut self) -> Result<Vec<ASTId<Expr>>, anyhow::Error> {
                let mut args: Vec<ASTId<Expr>> = Vec::new();
                while !self.tokens.match_next(TokenType::RightParen) {
                    if args.len() > 0 {
                        self.tokens.expect(TokenType::Comma)?;
                    }
                    args.push(self.parse_expr()?);
                }
                self.tokens.expect(TokenType::RightParen)?;
                Ok(args)
            }
            fn parse_expr_atom(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
                let token = self.tokens.next().clone();
                let pos = token.pos.clone();
                let kind = match token.kind {
                    TokenType::StringLiteral => {
                        let val = token.lexeme.clone();
                        let val = &val[1..val.len() - 1];
                        Expr::Literal(Literal::String(self.gc.intern(val)))
                    }
                    TokenType::NumericLiteral => {
                        Expr::Literal(Literal::Number(token.lexeme.parse().unwrap()))
                    }
                    TokenType::Null => Expr::Literal(Literal::Null),
                    TokenType::True => Expr::Literal(Literal::Boolean(true)),
                    TokenType::False => Expr::Literal(Literal::Boolean(false)),
                    TokenType::This => Expr::This,
                    TokenType::Super => Expr::Super,
                    TokenType::Identifier => {
                        Expr::Identifier(self.gc.intern(token.lexeme.clone()))
                    }
                    _ => {
                        return ::anyhow::__private::Err(
                            ::anyhow::Error::msg(
                                ::alloc::__export::must_use({
                                    let res = ::alloc::fmt::format(
                                        format_args!(
                                            "Unexpected token {0} at {1}",
                                            token,
                                            token.pos,
                                        ),
                                    );
                                    res
                                }),
                            ),
                        );
                    }
                };
                Ok(self.ast.add_expr(kind, pos))
            }
        }
    }
    mod compiler {
        use std::ops::Range;
        use anyhow::anyhow;
        use anyhow::bail;
        use fnv::FnvHashMap;
        use super::chunk::BytecodeChunk;
        use super::gc::Gc;
        use super::objects::ObjClass;
        use super::objects::ClassMember;
        use super::objects::ObjString;
        use super::objects::{ObjFn, UpvalueLocation};
        use super::opcode;
        use super::opcode::OpCode;
        use super::operator::Operator;
        use super::parser::{
            ASTId, ClassDecl, Expr, FieldInit, FnDecl, Literal, Stmt, AST,
        };
        use super::value::Value;
        struct Local {
            name: *mut ObjString,
            depth: u8,
            is_mutable: bool,
            is_captured: bool,
        }
        enum AccessKind {
            Get,
            Set,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for AccessKind {
            #[inline]
            fn clone(&self) -> AccessKind {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for AccessKind {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for AccessKind {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for AccessKind {
            #[inline]
            fn eq(&self, other: &AccessKind) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        struct FnFrame {
            upvalues: Vec<UpvalueLocation>,
            local_offset: u8,
            class_frame: Option<u8>,
        }
        enum FnType {
            None,
            Function,
            Method,
            Initializer,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for FnType {
            #[inline]
            fn clone(&self) -> FnType {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for FnType {}
        struct ClassFrame {
            class: ObjClass,
            superclass: Option<*mut ObjClass>,
        }
        pub struct Compiler<'a> {
            chunk: &'a mut BytecodeChunk,
            ast: &'a AST,
            gc: &'a mut Gc,
            locals: Vec<Local>,
            scope_depth: u8,
            fn_frames: Vec<FnFrame>,
            class_frames: Vec<ClassFrame>,
            fn_type: FnType,
            classes: FnvHashMap<*mut ObjString, u8>,
        }
        impl<'a> Compiler<'a> {
            pub fn compile<'b>(
                ast: &'b AST,
                gc: &'b mut Gc,
            ) -> Result<BytecodeChunk, anyhow::Error> {
                let mut chunk = BytecodeChunk::new();
                let mut compiler = Compiler {
                    chunk: &mut chunk,
                    ast,
                    gc,
                    locals: Vec::new(),
                    scope_depth: 0,
                    fn_frames: Vec::new(),
                    class_frames: Vec::new(),
                    fn_type: FnType::None,
                    classes: FnvHashMap::default(),
                };
                let stmt_id = compiler.ast.get_root();
                compiler.enter_function();
                compiler.statement(&stmt_id)?;
                compiler.exit_function(&stmt_id);
                Ok(chunk)
            }
            fn error<T: 'static>(
                &self,
                msg: impl Into<String>,
                node_id: &ASTId<T>,
            ) -> anyhow::Error {
                let pos = self.ast.pos(node_id);
                ::anyhow::Error::msg(
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!("{0}\n\tat {1}", msg.into(), pos),
                        );
                        res
                    }),
                )
            }
            fn emit<T: 'static>(&mut self, byte: u8, node_id: &ASTId<T>) {
                let pos = self.ast.pos(node_id);
                self.chunk.write(byte, pos);
            }
            fn emit_jump<T: 'static>(
                &mut self,
                op: OpCode,
                pos: u16,
                node_id: &ASTId<T>,
            ) -> u16 {
                self.emit(op, node_id);
                self.emit(pos as u8, node_id);
                self.emit((pos >> 8) as u8, node_id);
                return self.chunk.code.len() as u16 - 3;
            }
            fn patch_jump(&mut self, jump_ref: u16) -> Result<(), anyhow::Error> {
                if self.chunk.code.len() > u16::MAX as usize {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Jump too large"),
                        );
                        error
                    });
                }
                let pos = self.chunk.code.len() as u16;
                self.chunk.code[jump_ref as usize + 1] = pos as u8;
                self.chunk.code[jump_ref as usize + 2] = (pos >> 8) as u8;
                Ok(())
            }
            fn enter_scope(&mut self) {
                self.scope_depth += 1;
            }
            fn exit_scope<T: 'static>(&mut self, node_id: &ASTId<T>) {
                self.scope_depth -= 1;
                while !self.locals.is_empty()
                    && self.locals.last().unwrap().depth > self.scope_depth
                {
                    if self.locals.last().unwrap().is_captured {
                        self.emit(opcode::CLOSE_UPVALUE, node_id);
                        self.emit(self.locals.len() as u8 - 1, node_id);
                    } else {
                        self.emit(opcode::POP, node_id);
                    }
                    self.locals.pop();
                }
            }
            fn enter_function(&mut self) {
                self.fn_frames
                    .push(FnFrame {
                        upvalues: Vec::new(),
                        local_offset: if self.locals.is_empty() {
                            0
                        } else {
                            self.locals.len() as u8 - 1
                        },
                        class_frame: self
                            .class_frames
                            .last()
                            .map(|_| self.class_frames.len() as u8 - 1),
                    });
                self.scope_depth += 1;
            }
            fn exit_function<T: 'static>(&mut self, node_id: &ASTId<T>) {
                if self.chunk.code[self.chunk.code.len() - 1] != opcode::RETURN {
                    if let FnType::Initializer = self.fn_type {
                        self.emit(opcode::GET_LOCAL, node_id);
                        self.emit(0, node_id);
                    } else {
                        self.emit(opcode::PUSH_NULL, node_id);
                    }
                    self.emit(opcode::RETURN, node_id);
                }
                self.scope_depth -= 1;
                while !self.locals.is_empty()
                    && self.locals.last().unwrap().depth > self.scope_depth
                {
                    self.locals.pop();
                }
            }
            fn declare_local(
                &mut self,
                name: *mut ObjString,
                is_mutable: bool,
            ) -> Result<u8, anyhow::Error> {
                if self.locals.len() >= u8::MAX as usize {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Too many variables in scope"),
                        );
                        error
                    });
                }
                if self
                    .locals
                    .iter()
                    .rev()
                    .any(|local| local.depth == self.scope_depth && local.name == name)
                {
                    return ::anyhow::__private::Err(
                        ::anyhow::Error::msg(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "Variable \'{0}\' already declared in this scope",
                                        unsafe { &(*name).value },
                                    ),
                                );
                                res
                            }),
                        ),
                    );
                }
                self.chunk.add_constant(Value::from(name))?;
                self.locals
                    .push(Local {
                        name,
                        depth: self.scope_depth,
                        is_mutable,
                        is_captured: false,
                    });
                Ok((self.locals.len() - 1) as u8)
            }
            fn resolve_local(&self, name: *mut ObjString) -> Option<u8> {
                let local_offset = match self.fn_frames.last() {
                    Some(frame) => frame.local_offset,
                    None => 0,
                };
                return self
                    .resolve_local_in_range(name, local_offset..self.locals.len() as u8);
            }
            fn resolve_local_in_range(
                &self,
                name: *mut ObjString,
                range: Range<u8>,
            ) -> Option<u8> {
                for i in range.clone().rev() {
                    let local = &self.locals[i as usize];
                    if local.name == name {
                        return Some((i - range.start) as u8);
                    }
                }
                None
            }
            fn resolve_upvalue(
                &mut self,
                name: *mut ObjString,
                max_class_frame: Option<u8>,
            ) -> Result<Option<u8>, anyhow::Error> {
                if self.fn_frames.is_empty() {
                    return Ok(None);
                }
                self.resolve_frame_upvalue(
                    name,
                    self.fn_frames.len() - 1,
                    max_class_frame,
                )
            }
            fn resolve_frame_upvalue(
                &mut self,
                name: *mut ObjString,
                frame_idx: usize,
                max_class_frame: Option<u8>,
            ) -> Result<Option<u8>, anyhow::Error> {
                let class_frame = self.fn_frames[frame_idx].class_frame;
                if max_class_frame.is_some() && class_frame.is_some()
                    && class_frame.unwrap() < max_class_frame.unwrap()
                {
                    return Ok(None);
                }
                if max_class_frame.is_some() && class_frame.is_none() {
                    return Ok(None);
                }
                let range_start = if frame_idx == 0 {
                    0
                } else {
                    self.fn_frames[frame_idx - 1].local_offset
                };
                let range_end = self.fn_frames[frame_idx].local_offset;
                if let Some(idx) = self
                    .resolve_local_in_range(name, range_start..range_end)
                {
                    self.locals[(range_start + idx) as usize].is_captured = true;
                    return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
                }
                if frame_idx == 0 {
                    return Ok(None);
                }
                if let Some(idx) = self
                    .resolve_frame_upvalue(name, frame_idx - 1, max_class_frame)?
                {
                    return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
                }
                Ok(None)
            }
            fn resolve_member_class(&self, name: *mut ObjString) -> Option<u8> {
                for i in (0..self.class_frames.len()).rev() {
                    let class = &self.class_frames[i].class;
                    if class.resolve(name).is_some() {
                        return Some(i as u8);
                    }
                }
                None
            }
            fn add_upvalue(
                &mut self,
                location: u8,
                is_local: bool,
                frame_idx: usize,
            ) -> Result<u8, anyhow::Error> {
                for i in 0..self.fn_frames[frame_idx].upvalues.len() {
                    let upvalue = &self.fn_frames[frame_idx].upvalues[i];
                    if upvalue.location == location && upvalue.is_local == is_local {
                        return Ok(i as u8);
                    }
                }
                if self.fn_frames[frame_idx].upvalues.len() >= u8::MAX as usize {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Too many upvalues"),
                        );
                        error
                    });
                }
                let upvalue = UpvalueLocation {
                    location,
                    is_local,
                };
                self.fn_frames[frame_idx].upvalues.push(upvalue);
                return Ok((self.fn_frames[frame_idx].upvalues.len() - 1) as u8);
            }
            fn statement(&mut self, stmt_id: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
                match self.ast.get(stmt_id) {
                    Stmt::Block(stmts) => self.block(stmt_id, stmts)?,
                    Stmt::Return(expr) => {
                        if let FnType::Initializer = self.fn_type {
                            if expr.is_some() {
                                return ::anyhow::__private::Err({
                                    let error = ::anyhow::__private::format_err(
                                        format_args!(
                                            "Cannot return a value from a class initializer",
                                        ),
                                    );
                                    error
                                });
                            }
                            self.emit(opcode::GET_LOCAL, stmt_id);
                            self.emit(0, stmt_id);
                        } else if let Some(expr) = expr {
                            self.expression(expr)?;
                        } else {
                            self.emit(opcode::PUSH_NULL, stmt_id);
                        }
                        self.emit(opcode::RETURN, stmt_id);
                    }
                    Stmt::Fn(decl) => {
                        self.declare_local(decl.name, false)?;
                        let prev_fn_type = self.fn_type;
                        self.fn_type = FnType::Function;
                        let const_idx = self.function(stmt_id, decl)?;
                        self.fn_type = prev_fn_type;
                        self.emit(opcode::PUSH_CLOSURE, stmt_id);
                        self.emit(const_idx, stmt_id);
                    }
                    Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
                    Stmt::If(cond, then, otherwise) => {
                        self.if_stmt(cond, then, otherwise)?
                    }
                    Stmt::Say(FieldInit { name, value }) => {
                        let local = self.declare_local(*name, true)?;
                        if let Some(expr) = value {
                            self.expression(expr)?;
                            self.emit(opcode::SET_LOCAL, stmt_id);
                        } else {
                            self.emit(opcode::GET_LOCAL, stmt_id);
                        }
                        self.emit(local, stmt_id);
                    }
                    Stmt::Expression(expr) => {
                        self.expression(expr)?;
                        self.emit(opcode::POP, stmt_id);
                    }
                    Stmt::While(cond, body) => {
                        let pos = self.chunk.code.len() as u16;
                        self.expression(cond)?;
                        let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, stmt_id);
                        self.statement(body)?;
                        self.emit_jump(opcode::JUMP, pos, stmt_id);
                        self.patch_jump(jump_ref)?;
                    }
                };
                Ok(())
            }
            fn block(
                &mut self,
                stmt: &ASTId<Stmt>,
                stmts: &Vec<ASTId<Stmt>>,
            ) -> Result<(), anyhow::Error> {
                self.enter_scope();
                for stmt in stmts {
                    self.statement(stmt)?;
                }
                self.exit_scope(stmt);
                Ok(())
            }
            fn if_stmt(
                &mut self,
                cond: &ASTId<Expr>,
                then: &ASTId<Stmt>,
                otherwise: &Option<ASTId<Stmt>>,
            ) -> Result<(), anyhow::Error> {
                self.expression(cond)?;
                let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
                self.statement(then)?;
                let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
                self.patch_jump(jump_ref)?;
                if let Some(otherwise) = otherwise {
                    self.statement(otherwise)?;
                }
                self.patch_jump(else_jump_ref)?;
                Ok(())
            }
            fn if_expression(
                &mut self,
                cond: &ASTId<Expr>,
                then: &ASTId<Expr>,
                otherwise: &ASTId<Expr>,
            ) -> Result<(), anyhow::Error> {
                self.expression(cond)?;
                let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
                self.expression(then)?;
                let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
                self.patch_jump(jump_ref)?;
                self.expression(otherwise)?;
                self.patch_jump(else_jump_ref)?;
                Ok(())
            }
            fn method(
                &mut self,
                stmt: &ASTId<Stmt>,
                decl: &Box<FnDecl>,
            ) -> Result<u8, anyhow::Error> {
                let prev_fn_type = self.fn_type;
                self.fn_type = FnType::Method;
                let const_idx = self.function(stmt, decl)?;
                self.fn_type = prev_fn_type;
                Ok(const_idx)
            }
            fn initializer(
                &mut self,
                stmt: &ASTId<Stmt>,
                decl: &Box<FnDecl>,
            ) -> Result<u8, anyhow::Error> {
                let prev_fn_type = self.fn_type;
                self.fn_type = FnType::Initializer;
                let const_idx = self.function(stmt, decl)?;
                self.fn_type = prev_fn_type;
                Ok(const_idx)
            }
            fn function(
                &mut self,
                stmt: &ASTId<Stmt>,
                decl: &Box<FnDecl>,
            ) -> Result<u8, anyhow::Error> {
                if let FnType::None = self.fn_type {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Function declaration outside of function"),
                        );
                        error
                    });
                }
                self.enter_function();
                let jump_ref = self.emit_jump(opcode::JUMP, 0, stmt);
                let ip_start = self.chunk.code.len();
                let arity = decl.params.len() as u8;
                for param in &decl.params {
                    self.declare_local(*param, true)?;
                }
                self.statement(&decl.body)?;
                self.exit_function(&decl.body);
                self.patch_jump(jump_ref)?;
                let frame = self.fn_frames.pop().unwrap();
                let func = self
                    .gc
                    .alloc(ObjFn::new(decl.name, arity, ip_start, frame.upvalues));
                self.chunk.add_constant(Value::from(func))
            }
            fn class_declaration(
                &mut self,
                stmt: &ASTId<Stmt>,
                decl: &Box<ClassDecl>,
            ) -> Result<(), anyhow::Error> {
                self.declare_local(decl.name, false)?;
                self.enter_scope();
                let superclass = match decl.superclass {
                    Some(name) => {
                        let const_idx = self
                            .classes
                            .get(&name)
                            .ok_or(
                                ::anyhow::Error::msg(
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "Class \'{0}\' not declared",
                                                unsafe { &(*name).value },
                                            ),
                                        );
                                        res
                                    }),
                                ),
                            )?;
                        let value = self.chunk.constants[*const_idx as usize];
                        Some(unsafe { value.as_object().class })
                    }
                    None => None,
                };
                let mut frame = ClassFrame {
                    class: ObjClass::new(
                        decl.name,
                        superclass.map(|gc_ref| unsafe { &*gc_ref }),
                    ),
                    superclass,
                };
                for &field in &decl.fields {
                    frame.class.declare_field(field);
                }
                for stmt_id in &decl.methods {
                    let Stmt::Fn(decl) = self.ast.get(stmt_id) else {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        );
                    };
                    frame.class.declare_method(decl.name);
                }
                self.class_frames.push(frame);
                let Stmt::Fn(init_fn_decl) = self.ast.get(&decl.init) else {
                    ::core::panicking::panic("internal error: entered unreachable code");
                };
                let const_idx = self.initializer(stmt, init_fn_decl)?;
                let init_const = self.chunk.constants[const_idx as usize];
                let init_str = self.gc.intern("init");
                let frame = self.class_frames.last_mut().unwrap();
                frame.class.declare_method(init_str);
                frame
                    .class
                    .define_method(init_str, unsafe { init_const.as_object().function });
                for stmt_id in &decl.methods {
                    let Stmt::Fn(decl) = self.ast.get(stmt_id) else {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        );
                    };
                    let const_idx = self.method(stmt_id, decl)?;
                    let funct_const = self.chunk.constants[const_idx as usize];
                    let frame = self.class_frames.last_mut().unwrap();
                    frame
                        .class
                        .define_method(
                            decl.name,
                            unsafe { funct_const.as_object().function },
                        );
                }
                let class = self.class_frames.pop().unwrap().class;
                self.exit_scope(stmt);
                let idx = self.chunk.add_constant(Value::from(self.gc.alloc(class)))?;
                self.classes.insert(decl.name, idx);
                self.emit(opcode::PUSH_CLASS, stmt);
                self.emit(idx, stmt);
                Ok(())
            }
            fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
                match self.ast.get(expr) {
                    Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
                    Expr::Binary(op, left, right) => {
                        self.binary_expression(op, left, right)?
                    }
                    Expr::Ternary(cond, then, otherwise) => {
                        self.if_expression(cond, then, otherwise)?
                    }
                    Expr::Call(expr, args) => self.call_expression(expr, args)?,
                    Expr::Literal(lit) => self.literal(expr, lit)?,
                    Expr::Identifier(name) => self.identifier(expr, *name, false)?,
                    Expr::This => self.this(expr)?,
                    Expr::Super => self.super_(expr)?,
                };
                Ok(())
            }
            fn this(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
                if self.class_frames.is_empty() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Cannot use \'this\' outside of a class method"),
                        );
                        error
                    });
                }
                self.emit(opcode::GET_LOCAL, expr);
                self.emit(0, expr);
                Ok(())
            }
            fn super_(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
                let Some(frame) = self.class_frames.last() else {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!(
                                "Cannot use \'super\' outside of a class method",
                            ),
                        );
                        error
                    });
                };
                if frame.superclass.is_none() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!(
                                "Cannot use \'super\' outside of a child class method",
                            ),
                        );
                        error
                    });
                }
                self.emit(opcode::GET_LOCAL, expr);
                self.emit(0, expr);
                Ok(())
            }
            fn unary_expression(
                &mut self,
                op: &Operator,
                expr: &ASTId<Expr>,
            ) -> Result<(), anyhow::Error> {
                self.expression(expr)?;
                self.emit(op.as_opcode(), expr);
                Ok(())
            }
            fn binary_expression(
                &mut self,
                op: &Operator,
                left: &ASTId<Expr>,
                right: &ASTId<Expr>,
            ) -> Result<(), anyhow::Error> {
                if let Operator::Assign(_) = op {
                    match self.ast.get(left) {
                        Expr::Identifier(name) => {
                            if let Some(local) = self
                                .locals
                                .iter()
                                .rev()
                                .find(|local| local.name == *name)
                            {
                                if !local.is_mutable {
                                    return Err(
                                        self
                                            .error(
                                                ::alloc::__export::must_use({
                                                    let res = ::alloc::fmt::format(
                                                        format_args!(
                                                            "Invalid assignment: \'{0}\' is immutable",
                                                            unsafe { &(**name).value },
                                                        ),
                                                    );
                                                    res
                                                }),
                                                left,
                                            ),
                                    );
                                }
                            }
                            self.expression(right)?;
                            self.identifier(left, *name, true)?;
                            return Ok(());
                        }
                        Expr::Binary(Operator::MemberAccess, obj, member) => {
                            self.expression(right)?;
                            self.member_access(obj, member, AccessKind::Set)?;
                            return Ok(());
                        }
                        _ => return Err(self.error("Invalid assignment", left)),
                    }
                }
                if let Operator::MemberAccess = op {
                    self.member_access(left, right, AccessKind::Get)?;
                    return Ok(());
                }
                self.expression(left)?;
                self.expression(right)?;
                self.emit(op.as_opcode(), right);
                Ok(())
            }
            fn member_access(
                &mut self,
                expr: &ASTId<Expr>,
                member_expr: &ASTId<Expr>,
                access_kind: AccessKind,
            ) -> Result<(), anyhow::Error> {
                let Expr::Identifier(member) = *self.ast.get(member_expr) else {
                    return Err(self.error("Invalid member access", member_expr));
                };
                self.expression(expr)?;
                let (operand, get_op, set_op) = match self.ast.get(expr) {
                    Expr::This => {
                        let frame = self.class_frames.last().unwrap();
                        let id = self.resolve_class_member(expr, &frame.class, member)?;
                        (id, opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
                    }
                    Expr::Super => {
                        let frame = self.class_frames.last().unwrap();
                        let superclass = unsafe { &*frame.superclass.unwrap() };
                        let id = self.resolve_class_member(expr, superclass, member)?;
                        (id, opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
                    }
                    _ => {
                        let const_idx = self.chunk.add_constant(Value::from(member))?;
                        (const_idx, opcode::GET_PROPERTY, opcode::SET_PROPERTY)
                    }
                };
                let op = match access_kind {
                    AccessKind::Get => get_op,
                    AccessKind::Set => set_op,
                };
                self.emit(op, expr);
                self.emit(operand, expr);
                Ok(())
            }
            fn resolve_class_member(
                &self,
                expr: &ASTId<Expr>,
                class: &ObjClass,
                member: *mut ObjString,
            ) -> Result<u8, anyhow::Error> {
                let id = match class.resolve(member) {
                    Some(ClassMember::Field(id)) => id,
                    Some(ClassMember::Method(id)) => id,
                    None => {
                        unsafe {
                            return Err(
                                self
                                    .error(
                                        ::alloc::__export::must_use({
                                            let res = ::alloc::fmt::format(
                                                format_args!(
                                                    "Class \'{0}\' has no member \'{1}\'",
                                                    (*class.name).value,
                                                    (*member).value,
                                                ),
                                            );
                                            res
                                        }),
                                        expr,
                                    ),
                            )
                        }
                    }
                };
                Ok(id)
            }
            fn call_expression(
                &mut self,
                expr: &ASTId<Expr>,
                args: &Vec<ASTId<Expr>>,
            ) -> Result<(), anyhow::Error> {
                match self.ast.get(expr) {
                    Expr::Super => {
                        let frame = self.class_frames.last().unwrap();
                        let superclass = frame.superclass.unwrap();
                        let init_str = self.gc.intern("init");
                        let member_id = unsafe { &*superclass }
                            .resolve_id(init_str)
                            .unwrap();
                        self.emit(opcode::GET_LOCAL, expr);
                        self.emit(0, expr);
                        self.emit(opcode::GET_PROPERTY_ID, expr);
                        self.emit(member_id, expr);
                    }
                    _ => self.expression(expr)?,
                };
                for arg in args {
                    self.expression(arg)?;
                }
                self.emit(opcode::CALL, expr);
                self.emit(args.len() as u8, expr);
                Ok(())
            }
            fn literal(
                &mut self,
                expr: &ASTId<Expr>,
                literal: &Literal,
            ) -> Result<(), anyhow::Error> {
                match literal {
                    Literal::Number(num) => {
                        let idx = self.chunk.add_constant(Value::from(*num))?;
                        self.emit(opcode::PUSH_CONSTANT, expr);
                        self.emit(idx, expr);
                    }
                    Literal::String(str_ref) => {
                        let idx = self.chunk.add_constant(Value::from(*str_ref))?;
                        self.emit(opcode::PUSH_CONSTANT, expr);
                        self.emit(idx, expr);
                    }
                    Literal::Null => {
                        self.emit(opcode::PUSH_NULL, expr);
                    }
                    Literal::Boolean(true) => {
                        self.emit(opcode::PUSH_TRUE, expr);
                    }
                    Literal::Boolean(false) => {
                        self.emit(opcode::PUSH_FALSE, expr);
                    }
                };
                return Ok(());
            }
            fn identifier(
                &mut self,
                expr: &ASTId<Expr>,
                name: *mut ObjString,
                assign: bool,
            ) -> Result<(), anyhow::Error> {
                let class_frame_idx = self.resolve_member_class(name);
                let (operand, get_op, set_op) = if let Some(local_idx) = self
                    .resolve_local(name)
                {
                    (local_idx, opcode::GET_LOCAL, opcode::SET_LOCAL)
                } else if let Some(upvalue_idx) = self
                    .resolve_upvalue(name, class_frame_idx)?
                {
                    (upvalue_idx, opcode::GET_UPVALUE, opcode::SET_UPVALUE)
                } else if let Some(id) = self
                    .class_frames
                    .last()
                    .and_then(|f| f.class.resolve_id(name))
                {
                    self.emit(opcode::GET_LOCAL, expr);
                    self.emit(0, expr);
                    (id, opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
                } else {
                    let const_idx = self.chunk.add_constant(Value::from(name))?;
                    (const_idx, opcode::GET_GLOBAL, opcode::SET_GLOBAL)
                };
                let op = if assign { set_op } else { get_op };
                self.emit(op, expr);
                self.emit(operand, expr);
                return Ok(());
            }
        }
    }
    pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
        vm::Vm::run(file_name, src)
    }
}
pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::run(file_name, src)
}
