// Tail-call dispatch (`become`) in the VM needs this. Nightly-only until it stabilizes.
#![feature(explicit_tail_calls)]
#![feature(variant_count)]
#![allow(incomplete_features)]

// Capture is a debug facility, and `capture_output` turns it on in release so the test harness can
// collect `print` output there too.
#[cfg(any(debug_assertions, feature = "capture_output"))]
#[cfg_attr(any(debug_assertions, feature = "capture_output"), path = "debug_output.rs")]
mod output;

#[cfg(not(any(debug_assertions, feature = "capture_output")))]
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
pub use frontend::lex::enable_color;
pub use output::Output;

/// Exposes compiler internals for unit tests under `tests/`. Hidden from docs
/// and not a stable public API.
#[doc(hidden)]
pub mod internals {
    pub use crate::ast::{MatchArm, Ast, AstId, Expr, SayDecl, FnDecl, Literal, MatchElem, MatchField, MatchScalar, Matcher, ObligationRules, Operator, Param, ReturnShape, Stmt, Symbol, TypeDecl};
    pub use crate::frontend::lex::{ContextualKeyword, Token, TokenType};
    pub use crate::middle::hir::{
        Hir, HirMatchArm, HirExpr, HirSayDecl, HirFnDecl, HirId, HirLiteral, HirMatcher, HirMatchElem, HirMatchField, HirParam, HirStmt, HirTypeDecl,
    };
    pub use crate::core::objects::TypeMember;
    pub use crate::middle::bind::{Bindings, TypeLayout};
    pub use crate::middle::check::Barriers;
    pub use crate::middle::check::scope::{intersect_narrowings, merge_local_flow, LocalFlow};
    pub use crate::middle::obligations::Obligations;
    pub use crate::middle::signatures::{CallableId, TypeTag};

    pub use crate::middle::codegen::matching::{Scalar, tree::{build_tree, Access, Clause, DecisionTree, Path, ValueTest}};
    pub use crate::middle::ir::{Ir, Label};

    use crate::frontend::lex::{tokenize, TokenStream};
    use crate::frontend::parse::Parser;

    pub fn symbol(id: u32) -> Symbol {
        Symbol::from_raw(id)
    }

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

    /// Runs the signature pass alone, so a benchmark can time it without the rest of the pipeline.
    pub fn signatures(hir: &Hir, bindings: &Bindings) {
        crate::middle::signatures::collect(hir, bindings);
    }

    pub fn nullck(src: &str) -> Barriers {
        let (hir, bindings) = bind(src);
        let sigs = crate::middle::signatures::collect(&hir, &bindings);
        crate::middle::check::check(&hir, &bindings, &sigs, crate::RunConfig::default()).expect("nullck error")
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
use crate::middle::shape::check as check_shape;
use crate::middle::optimize::optimize;
use crate::middle::bind::resolve as resolve_bindings;
use crate::middle::signatures::collect as collect_signatures;

/// How the pipeline is built for one run. The default is the shipped pipeline.
#[derive(Clone, Copy)]
pub struct RunConfig {
    /// Whether the peephole pass runs.
    pub optimize: bool,
    /// Whether codegen emits the checks the check pass proved unnecessary.
    pub force_checks: bool,
    pub drop_guards: bool,
}

impl Default for RunConfig {
    fn default() -> RunConfig {
        RunConfig { optimize: true, force_checks: false, drop_guards: false }
    }
}

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    run_with(file_name, src, RunConfig::default())
}

pub fn run_with(file_name: &str, src: &str, config: RunConfig) -> Result<Vec<String>, anyhow::Error> {
    let mut gc = Gc::new();

    let tokens = tokenize(String::from(file_name), String::from(src))?;
    let ast = Parser::parse(&mut TokenStream::new(&tokens))?;

    let names = resolve_names(&ast)?;
    let hir = lower(ast, &names)?;
    let bindings = resolve_bindings(&hir)?;
    let sigs = collect_signatures(&hir, &bindings);
    check_shape(&hir, &bindings, &sigs)?;
    let barriers = check(&hir, &bindings, &sigs, config)?;
    let ir = Compiler::compile(&hir, &mut gc, &bindings, &barriers, &sigs, config.drop_guards)?;
    let ir = if config.optimize { optimize(ir) } else { ir };

    let chunk = assemble(ir)?;
    runtime::execute(chunk, gc)
}