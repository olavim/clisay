use std::ops::Range;

use anyhow::bail;

use crate::lexer::{Token, TokenStream, TokenType};

use super::{gc::GcRef, operator::Operator, BytecodeChunk, Function, Gc, OpCode, Value};

struct Local {
    name: GcRef<String>,
    depth: u8,
    is_captured: bool
}

struct FnUpvalue {
    is_local: bool,
    location: u8
}

struct FnFrame {
    upvalues: Vec<FnUpvalue>,
    local_offset: u8
}

pub struct Compiler<'a> {
    pub chunk: &'a mut BytecodeChunk,
    stream: TokenStream<'a>,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>
}

impl Compiler<'_> {
    pub fn write(chunk: &mut BytecodeChunk, tokens: &Vec<Token>, gc: &mut Gc) -> Result<(), anyhow::Error> {
        let mut compiler = Compiler::new(chunk, tokens, gc);
        compiler.compile()?;
        Ok(())
    }

    fn new<'a>(chunk: &'a mut BytecodeChunk, tokens: &'a Vec<Token>, gc: &'a mut Gc) -> Compiler<'a> {
        return Compiler { 
            stream: TokenStream::new(tokens),
            chunk,
            gc,
            locals: Vec::new(),
            scope_depth: 0,
            fn_frames: Vec::new()
        };
    }

    pub fn compile(&mut self) -> Result<BytecodeChunk, anyhow::Error> {
        while !self.stream.match_next(TokenType::EOF) {
            self.statement()?;
        }
        self.stream.expect(TokenType::EOF)?;
        return Ok(self.chunk.clone());
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.chunk.write_byte(OpCode::CloseUpvalue);
            } else {
                self.chunk.write_byte(OpCode::Pop);
            }
            self.locals.pop();
        }
    }

    fn enter_function(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_function(&mut self) {
        if self.chunk.code[self.chunk.code.len() - 1] != OpCode::Return as u8 {
            self.chunk.write_byte(OpCode::PushNull);
            self.chunk.write_byte(OpCode::Return);
        }

        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }
    }

    fn declare_local(&mut self, name: String) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && self.local_name_equals(local, &name)) {
            bail!("Variable '{}' already declared in this scope", name);
        }

        let name_ref = self.gc.intern(name);
        self.locals.push(Local { name: name_ref, depth: self.scope_depth, is_captured: false });
        Ok((self.locals.len() - 1) as u8)
    }

    fn resolve_local(&self, name: &str) -> Option<u8> {
        let local_offset = match self.fn_frames.last() {
            Some(frame) => frame.local_offset,
            None => 0
        };

        return self.resolve_local_in_range(name, local_offset..self.locals.len() as u8);
    }

    fn resolve_local_in_range(&self, name: &str, range: Range<u8>) -> Option<u8> {
        for i in range.clone().rev() {
            let local = &self.locals[i as usize];
            if self.local_name_equals(local, name) {
                return Some((i - range.start) as u8);
            }
        }
        
        None
    }

    fn resolve_upvalue(&mut self, name: &str) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }

        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1)
    }

    fn resolve_frame_upvalue(&mut self, name: &str, frame_idx: usize) -> Result<Option<u8>, anyhow::Error> {
        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;
        
        if let Some(idx) = self.resolve_local_in_range(name, range_start..range_end) {
            self.locals[(range_start + idx) as usize].is_captured = true;
            return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        for i in 0..self.fn_frames[frame_idx].upvalues.len() {
            let upvalue = &self.fn_frames[frame_idx].upvalues[i];
            if upvalue.location == location && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }

        if self.fn_frames[frame_idx].upvalues.len() >= u8::MAX as usize {
            bail!("Too many upvalues");
        }

        let upvalue = FnUpvalue { location, is_local };
        self.fn_frames[frame_idx].upvalues.push(upvalue);
        return Ok((self.fn_frames[frame_idx].upvalues.len() - 1) as u8);
    }

    fn local_name_equals(&self, local: &Local, name: &str) -> bool {
        *self.gc.get(&local.name) == name
    }

    fn statement(&mut self) -> Result<(), anyhow::Error> {
        match &self.stream.peek(0).kind {
            TokenType::LeftCurlyBracket => self.block()?,
            TokenType::Return => {
                self.stream.next();
                if let Some(_) = self.stream.next_if(TokenType::Semicolon) {
                    self.chunk.write_byte(OpCode::PushNull);
                    self.chunk.write_byte(OpCode::Return);
                } else {
                    self.expression(0)?;
                    self.chunk.write_byte(OpCode::Return);
                    self.stream.expect(TokenType::Semicolon)?;
                }
            },
            TokenType::Fn => self.function_declaration()?,
            TokenType::If => self.if_stmt()?,
            // TokenType::Say => {
            //     self.stream.next();
            //     let name = self.stream.expect(TokenType::Identifier)?.lexeme.clone();
            //     self.declare_local(name.clone())?;

            //     self.stream.expect(TokenType::Semicolon)?;
            // },
            _ => {
                self.expression(0)?;
                self.chunk.write_byte(OpCode::Pop);
                self.stream.expect(TokenType::Semicolon)?;
            }
        };

        Ok(())
    }

    fn block(&mut self) -> Result<(), anyhow::Error> {
        self.stream.expect(TokenType::LeftCurlyBracket)?;
        self.enter_scope();
        while !self.stream.match_next(TokenType::RightCurlyBracket) {
            self.statement()?;
        }
        self.exit_scope();
        self.stream.expect(TokenType::RightCurlyBracket)?;
        Ok(())
    }

    fn if_stmt(&mut self) -> Result<(), anyhow::Error> {
        self.stream.expect(TokenType::If)?;
        self.expression(0)?;
        let jump_ref = self.chunk.init_jump(OpCode::JumpIfFalse);
        self.block()?;
        let else_jump_ref = self.chunk.init_jump(OpCode::Jump);
        self.chunk.patch_jump(jump_ref)?;
        if let Some(_) = self.stream.next_if(TokenType::Else) {
            self.block()?;
        }
        self.chunk.patch_jump(else_jump_ref)?;
        Ok(())
    }

    fn function_declaration(&mut self) -> Result<(), anyhow::Error> {
        self.stream.expect(TokenType::Fn)?;
        let name = self.stream.expect(TokenType::Identifier)?.lexeme.clone();
        let local_idx = self.declare_local(name.clone())?;

        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset: local_idx
        });

        let jump_ref = self.chunk.init_jump(OpCode::Jump);

        self.enter_function();

        let ip_start = self.chunk.code.len();
        let arity = self.parameter_list()?;

        self.block()?;
        self.exit_function();

        self.chunk.patch_jump(jump_ref)?;

        let frame = self.fn_frames.pop().unwrap();
        let func = self.gc.alloc(Function {
            name: self.locals[local_idx as usize].name,
            arity,
            ip_start,
            upvalue_count: frame.upvalues.len() as u8
        });

        let idx = self.chunk.add_constant(Value::Function(func))?;
        self.chunk.write_byte(OpCode::PushClosure);
        self.chunk.write_byte(idx);
        
        for upvalue in frame.upvalues {
            self.chunk.write_byte(upvalue.is_local as u8);
            self.chunk.write_byte(upvalue.location);
        }
        
        Ok(())
    }

    fn expression(&mut self, min_precedence: u8) -> Result<(), anyhow::Error> {
        match Operator::parse_prefix(&mut self.stream, 0) {
            Some(op) => self.prefix_expression(op)?,
            _ => self.atom()?
        };
    
        loop {
            if let Some(op) = Operator::parse_postfix(&mut self.stream, min_precedence) {
                self.postfix_expression(op)?;
            } else if let Some(op) = Operator::parse_infix(&mut self.stream, min_precedence) {
                self.infix_expression(op)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn infix_expression(&mut self, op: Operator) -> Result<(), anyhow::Error> {
        self.expression(op.infix_precedence().unwrap())?;
        self.chunk.write_byte(OpCode::from_operator(op));
        Ok(())
    }
    
    fn prefix_expression(&mut self, op: Operator) -> Result<(), anyhow::Error> {
        self.expression(0)?;

        match &op {
            Operator::Group => { self.stream.expect(TokenType::RightParenthesis)?; },
            _ => { self.chunk.write_byte(OpCode::from_operator(op)); }
        };
        
        Ok(())
    }
    
    fn postfix_expression(&mut self, op: Operator) -> Result<(), anyhow::Error> {
        match &op {
            Operator::Call => {
                let arg_count = self.argument_list()?;
                self.chunk.write_byte(OpCode::Call);
                self.chunk.write_byte(arg_count);
            },
            _ => unreachable!()
        };

        Ok(())
    }

    fn parameter_list(&mut self) -> Result<u8, anyhow::Error> {
        self.stream.expect(TokenType::LeftParenthesis)?;

        let mut param_count = 0;
        while !self.stream.match_next(TokenType::RightParenthesis) {
            if param_count > 0 {
                self.stream.expect(TokenType::Comma)?;
            }
            let name = self.stream.expect(TokenType::Identifier)?.lexeme.clone();
            self.declare_local(name)?;
            param_count += 1;
        }

        self.stream.expect(TokenType::RightParenthesis)?;
        return Ok(param_count);
    }

    fn argument_list(&mut self) -> Result<u8, anyhow::Error> {
        let mut arg_count = 0;
        while !self.stream.match_next(TokenType::RightParenthesis) {
            if arg_count > 0 {
                self.stream.expect(TokenType::Comma)?;
            }
            self.expression(0)?;
            arg_count += 1;
        }

        self.stream.expect(TokenType::RightParenthesis)?;
        return Ok(arg_count);
    }
    
    fn atom(&mut self) -> Result<(), anyhow::Error> {
        let token = self.stream.next().clone();    
        match &token.kind {
            TokenType::NumericLiteral => {
                let idx = self.chunk.add_constant(Value::Number(token.lexeme.parse()?))?;
                self.chunk.write_byte(OpCode::PushConstant);
                self.chunk.write_byte(idx);
            },
            TokenType::StringLiteral => {
                let idx = self.chunk.add_constant(Value::String(self.gc.intern(token.lexeme)))?;
                self.chunk.write_byte(OpCode::PushConstant);
                self.chunk.write_byte(idx);
            },
            TokenType::Identifier => {
                let op: OpCode;
                let idx: u8;
                if let Some(local_idx) = self.resolve_local(&token.lexeme) {
                    op = OpCode::GetLocal;
                    idx = local_idx;
                } else if let Some(upvalue_idx) = self.resolve_upvalue(&token.lexeme)? {
                    op = OpCode::GetUpvalue;
                    idx = upvalue_idx;
                } else {
                    op = OpCode::GetGlobal;
                    idx = self.chunk.add_constant(Value::String(self.gc.intern(token.lexeme)))?;
                }
                self.chunk.write_byte(op);
                self.chunk.write_byte(idx);
            },
            TokenType::Null => { self.chunk.write_byte(OpCode::PushNull); },
            TokenType::True => { self.chunk.write_byte(OpCode::PushTrue); },
            TokenType::False => { self.chunk.write_byte(OpCode::PushFalse); },
            _ => bail!("Unexpected token {} at {}", token, token.pos)
        };

        return Ok(());
    }
}