//! Constructor assembly and `init` orchestration.
//!
//! Builds each composer's initializer and the `"<Trait>.init"` methods, and verifies orchestration:
//! - a parameterized trait init must be called explicitly once;
//! - a parameterless one (explicit or virtual) runs automatically when omitted.
//! The qualified call `Trait.init(args)` is rewritten here into an internal method call.

use std::collections::HashMap;

use anyhow::anyhow;

use crate::ast::{AstId, Expr, Literal, Stmt, Symbol, TypeDecl};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt};

use super::Lowerer;

impl<'a> Lowerer<'a> {
    /// Verifies init orchestration: a composer's `init` must call `T.init(args)` exactly once
    /// for each **directly** `with`-mixed trait whose init takes parameters. A parameterless
    /// trait init (explicit or virtual) is optional and runs automatically when omitted, but
    /// may be called at most once. You may not call the init of a trait you do not directly
    /// `with`-own. Applies to types and traits alike.
    pub(super) fn check_init_orchestration(&self, decl: &TypeDecl, decl_pos: &SourcePosition) -> Result<(), anyhow::Error> {
        let calls = match decl.init {
            Some(init_stmt) => self.explicit_init_calls(self.ast_block(&self.ast_fn(&init_stmt).body)),
            None => HashMap::new(),
        };

        // A call must target a directly `with`-owned trait.
        for (t, sites) in &calls {
            if !decl.with_traits.contains(t) {
                let kind = if decl.is_trait { "trait" } else { "type" };
                return Err(self.error(format!("'{}.init(...)': '{}' is not directly with-mixed by this {kind}",
                    self.hir.text(*t), self.hir.text(*t)), &sites[0]));
            }
        }
        // Each direct `with`-trait: parameterized inits required exactly once, parameterless inits at most once.
        for t in &decl.with_traits {
            let count = calls.get(t).map_or(0, |v| v.len());
            let parameterized = matches!(self.trait_init(*t), Some((param_count, _)) if param_count >= 1);
            if parameterized && count == 0 {
                return Err(anyhow!("init must call '{}.init(...)': '{}' has init parameters and cannot be auto-initialized\n\tat {}",
                    self.hir.text(*t), self.hir.text(*t), decl_pos));
            }
            if count > 1 {
                return Err(self.error(format!("'{}.init(...)' is called more than once", self.hir.text(*t)), &calls[t][1]));
            }
        }
        Ok(())
    }

    /// If `callee` is `Trait.init` (a `.`-access of `init` on an in-scope trait name),
    /// returns the trait's symbol — the shape of a qualified init-orchestration call.
    pub(super) fn as_qualified_init(&self, callee: &AstId<Expr>) -> Option<Symbol> {
        let Expr::Index(target, member, true) = self.ast.get(callee) else { return None };
        let Expr::Identifier(t) = self.ast.get(target) else { return None };
        if !self.is_trait_in_scope(*t) { return None; }
        let Expr::Literal(Literal::String(m)) = self.ast.get(member) else { return None };
        if m != "init" { return None; }
        Some(*t)
    }

    /// Builds the HIR for `Trait.init(args)`: an internal call of the spliced `"<Trait>.init"`
    /// method. A trait with no declared init still has a virtual empty (no-arg) init.
    pub(super) fn qualified_init_call(&mut self, trait_sym: Symbol, args: Vec<HirId<HirExpr>>, callee: &AstId<Expr>, pos: &SourcePosition) -> Result<HirExpr, anyhow::Error> {
        let trait_stmt = self.lookup_trait(trait_sym).expect("qualified-init trait is in scope");
        let trait_decl = self.ast_type(&trait_stmt);
        if trait_decl.init.is_none() {
            // A virtual empty init is callable as a no-op; being empty it takes no arguments.
            if !args.is_empty() {
                return Err(self.error(format!("Trait '{}' has a virtual empty init that takes no arguments", self.hir.text(trait_sym)), callee));
            }
            return Ok(HirExpr::Literal(HirLiteral::Null));
        }
        let method_name = self.hir.text(trait_decl.init_name).to_string();
        Ok(HirExpr::Call(self.this_method(&method_name, pos), args))
    }

    /// Lowers a `type`'s initializer: field defaults, then a virtual `super()` (child types),
    /// then auto-injected parameterless trait inits, then the declared body.
    pub(super) fn lower_type_init(&mut self, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)], type_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
        let is_child = decl.superclass.is_some();
        let (params, body_stmts, has_super, init_pos): (_, &[AstId<Stmt>], _, _) = match &decl.init {
            Some(init_id) => {
                let init_pos = self.ast.pos(init_id).clone();
                let fn_decl = self.ast_fn(init_id);
                let params = self.exprs(&fn_decl.params)?;
                let stmts = self.ast_block(&fn_decl.body);
                let has_super = stmts.first().is_some_and(|s| self.is_super_call(s));
                (params, stmts, has_super, init_pos)
            },
            None => (Vec::new(), &[], false, type_pos.clone()),
        };

        let mut body = Vec::new();

        // Field initializers (`this.f = v`), spliced ahead of everything.
        for (field, value) in field_inits {
            let target = self.hir.add(HirExpr::Identifier(*field), type_pos.clone());
            let value = self.expr(value)?;
            let assign = self.hir.add(HirExpr::Assign(target, value), type_pos.clone());
            body.push(self.hir.add(HirStmt::Expression(assign), type_pos.clone()));
        }
        // A virtual `super()` for a child type that didn't write one.
        if is_child && !has_super {
            body.push(self.virtual_super_call(&init_pos));
        }
        // Auto-call any `with`-mixed parameterless trait init not explicitly orchestrated.
        body.extend(self.synthesize_auto_inits(&decl.with_traits, body_stmts, &init_pos)?);
        // The declared body (which may itself open with an explicit `super(...)`).
        for stmt_id in body_stmts {
            body.push(self.stmt(stmt_id)?);
        }

        Ok(self.make_init_fn(decl.init_name, params, body, &init_pos))
    }

    /// Lowers a `with`-mixed trait's declared init into its `"<Trait>.init"` method,
    /// auto-injecting the trait's own parameterless owned inits ahead of its body.
    pub(super) fn lower_trait_init(&mut self, trait_sym: Symbol) -> Result<HirId<HirStmt>, anyhow::Error> {
        let trait_stmt = self.lookup_trait(trait_sym).expect("with-mixed trait is in scope");
        let td = self.ast_type(&trait_stmt);
        let init_ast = td.init.expect("only traits with a declared init are lowered here");
        let init_name = td.init_name;
        let with_traits = &td.with_traits;
        let init_pos = self.ast.pos(&init_ast).clone();
        let fd = self.ast_fn(&init_ast);
        let params = self.exprs(&fd.params)?;
        let body_stmts = self.ast_block(&fd.body);

        // The trait init's body accesses the trait's own private members via plain `this.<name>` /
        // bare `<name>`; the resolver scopes those to the trait.
        let mut body = self.synthesize_auto_inits(with_traits, body_stmts, &init_pos)?;
        for s in body_stmts {
            body.push(self.stmt(s)?);
        }
        Ok(self.make_init_fn(init_name, params, body, &init_pos))
    }

    /// The explicit `T.init(...)` calls in a body, grouped by trait → call statements.
    fn explicit_init_calls(&self, body_stmts: &[AstId<Stmt>]) -> HashMap<Symbol, Vec<AstId<Stmt>>> {
        let mut calls: HashMap<Symbol, Vec<AstId<Stmt>>> = HashMap::new();
        for s in body_stmts {
            let Stmt::Expression(expr) = self.ast.get(s) else { continue };
            let Expr::Call(callee, _) = self.ast.get(expr) else { continue };
            if let Some(t) = self.as_qualified_init(callee) {
                calls.entry(t).or_default().push(*s);
            }
        }
        calls
    }

    /// `(param_count, init_name)` for a trait that declares an explicit `init`, else `None`
    /// (a defaulted-only trait has only a virtual empty init).
    fn trait_init(&self, trait_sym: Symbol) -> Option<(usize, Symbol)> {
        let td = self.ast_type(&self.lookup_trait(trait_sym)?);
        let fd = self.ast_fn(&td.init?);
        Some((fd.params.len(), td.init_name))
    }

    /// Synthesizes `this.<Trait.init>()` calls for each `with`-mixed trait with a
    /// **parameterless** explicit init that the declared body does not call itself — these
    /// run automatically (like a synthesised `super()`).
    fn synthesize_auto_inits(&mut self, with_traits: &[Symbol], declared_body: &[AstId<Stmt>], pos: &SourcePosition) -> Result<Vec<HirId<HirStmt>>, anyhow::Error> {
        let called = self.explicit_init_calls(declared_body);
        let mut autos = Vec::new();
        for t in with_traits {
            if called.contains_key(t) { continue; }
            if let Some((0, init_name)) = self.trait_init(*t) {
                let method_name = self.hir.text(init_name).to_string();
                let call = HirExpr::Call(self.this_method(&method_name, pos), Vec::new());
                let call = self.hir.add(call, pos.clone());
                autos.push(self.hir.add(HirStmt::Expression(call), pos.clone()));
            }
        }
        Ok(autos)
    }

    /// Whether `stmt` is a `super(...)` call statement.
    fn is_super_call(&self, stmt: &AstId<Stmt>) -> bool {
        let Stmt::Expression(expr) = self.ast.get(stmt) else { return false };
        let Expr::Call(callee, _) = self.ast.get(expr) else { return false };
        matches!(self.ast.get(callee), Expr::Super)
    }

    fn virtual_super_call(&mut self, pos: &SourcePosition) -> HirId<HirStmt> {
        let super_expr = self.hir.add(HirExpr::Super, pos.clone());
        let call = self.hir.add(HirExpr::Call(super_expr, Vec::new()), pos.clone());
        self.hir.add(HirStmt::Expression(call), pos.clone())
    }

    /// A `this.<name>` member-access callee (a dotted access), used to build internal init calls.
    pub(super) fn this_method(&mut self, name: &str, pos: &SourcePosition) -> HirId<HirExpr> {
        let this_expr = self.hir.add(HirExpr::This, pos.clone());
        let name_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(name.to_string())), pos.clone());
        self.hir.add(HirExpr::Index(this_expr, name_lit, true), pos.clone())
    }

    /// Wraps an init body into its `HirStmt::Fn`.
    fn make_init_fn(&mut self, name: Symbol, params: Vec<HirId<HirExpr>>, body: Vec<HirId<HirStmt>>, pos: &SourcePosition) -> HirId<HirStmt> {
        let body = self.hir.add(HirExpr::Block(body), pos.clone());
        let fn_decl = HirFnDecl { name, params, body };
        self.hir.add(HirStmt::Fn(fn_decl), pos.clone())
    }
}
