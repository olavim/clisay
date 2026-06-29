#[cfg(debug_assertions)]
#[cfg_attr(debug_assertions, path = "debug_output.rs")]
mod output;

#[cfg(not(debug_assertions))]
mod output {
    pub struct Output;
    impl Output {
        #[inline(always)]
        pub fn println(value: impl Into<String>) {
            println!("{}", value.into());
        }
    }
}

mod core;
mod frontend;
mod middle;
mod backend;
mod runtime;

pub(crate) use frontend::ast;
pub use output::Output;

/// Exposes compiler internals for unit tests under `tests/`. Hidden from docs
/// and not a stable public API.
#[doc(hidden)]
pub mod internals {
    pub use crate::ast::{MatchArm, Ast, AstId, Expr, FieldInit, FnDecl, Literal, MatchBody, MatchElem, MatchField, MatchScalar, Matcher, Operator, Param, ReturnShape, Stmt, Symbol, TypeDecl};
    pub use crate::frontend::lex::{ContextualKeyword, Token, TokenType};
    pub use crate::middle::hir::{
        Hir, HirMatchArm, HirMatchBody, HirExpr, HirFieldInit, HirFnDecl, HirId, HirLiteral, HirMatcher, HirMatchElem, HirMatchField, HirParam, HirStmt, HirTypeDecl,
    };
    pub use crate::middle::bind::{Bindings, TypeLayout};
    pub use crate::middle::check::Barriers;

    use crate::frontend::lex::{tokenize, TokenStream};
    use crate::frontend::parse::Parser;

    pub fn lex(src: &str) -> Vec<Token> {
        tokenize(String::new(), src.to_string()).expect("lex error")
    }

    pub fn parse(src: &str) -> Ast {
        Parser::parse(&mut TokenStream::new(&lex(src))).expect("parse error")
    }

    pub fn try_parse(src: &str) -> Result<Ast, String> {
        Parser::parse(&mut TokenStream::new(&lex(src))).map_err(|e| e.to_string())
    }

    pub fn parse_matcher(src: &str) -> Result<(Ast, AstId<Matcher>), String> {
        Parser::parse_matcher_root(&mut TokenStream::new(&lex(src))).map_err(|e| e.to_string())
    }

    pub fn try_resolve(src: &str) -> Result<(), String> {
        crate::middle::names::resolve(&parse(src)).map(|_| ()).map_err(|e| e.to_string())
    }

    pub fn lower(src: &str) -> Hir {
        let ast = parse(src);
        let names = crate::middle::names::resolve(&ast).expect("name resolution error");
        crate::middle::lower::lower(ast, &names).expect("lower error")
    }

    pub fn bind(src: &str) -> (Hir, Bindings) {
        let hir = lower(src);
        let bindings = crate::middle::bind::resolve(&hir).expect("bind error");
        (hir, bindings)
    }

    pub fn nullck(src: &str) -> Barriers {
        let (hir, bindings) = bind(src);
        let sigs = crate::middle::signatures::collect(&hir);
        crate::middle::check::check(&hir, &bindings, &sigs).expect("nullck error")
    }
}

use crate::backend::assemble::assemble;
use crate::core::gc::Gc;
use crate::frontend::lex::{tokenize, TokenStream};
use crate::frontend::parse::Parser;
use crate::middle::codegen::Compiler;
use crate::middle::lower::lower;
use crate::middle::names::resolve as resolve_names;
use crate::middle::check::check;
use crate::middle::optimize::optimize;
use crate::middle::bind::resolve as resolve_bindings;
use crate::middle::signatures::collect as collect_signatures;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    let mut gc = Gc::new();

    let tokens = tokenize(String::from(file_name), String::from(src))?;
    let ast = Parser::parse(&mut TokenStream::new(&tokens))?;

    let names = resolve_names(&ast)?;
    let hir = lower(ast, &names)?;
    let bindings = resolve_bindings(&hir)?;
    let sigs = collect_signatures(&hir);
    let barriers = check(&hir, &bindings, &sigs)?;
    let ir = Compiler::compile(&hir, &mut gc, &bindings, &barriers)?;
    let ir = optimize(ir);
    
    let chunk = assemble(ir)?;
    runtime::execute(chunk, gc)
}