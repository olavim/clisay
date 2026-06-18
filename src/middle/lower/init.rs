//! Initializer assembly.
//!
//! Builds a `type`'s initializer: field defaults, a virtual `super()` for child types,
//! the declared body, and the `gives` delegate-verification checks.

use crate::ast::{AstId, Expr, Stmt, Symbol, TypeDecl};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, UnOp};

use super::Lowerer;

impl<'a> Lowerer<'a> {
    /// Lowers a `type`'s initializer: field defaults, then a virtual `super()` (child types),
    /// then the declared body, then `gives` delegate verification.
    pub(super) fn lower_type_init(&mut self, composer_id: AstId<Stmt>, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)], type_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
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
        // The declared body (which may itself open with an explicit `super(...)`).
        for stmt_id in body_stmts {
            body.push(self.stmt(stmt_id)?);
        }
        // `gives` delegate verification: once construction has run, each delegate field must
        // actually provide its trait, else construction fails.
        body.extend(self.synthesize_gives_verifications(composer_id, &init_pos)?);

        Ok(self.make_init_fn(decl.init_name, params, body, &init_pos))
    }

    /// Builds the construction-time verification for each `gives` delegate:
    /// `if !(this.<field> is Trait) { throw "<message>"; }`. A failed check
    /// is a catchable runtime error raised at the end of construction.
    fn synthesize_gives_verifications(&mut self, composer_id: AstId<Stmt>, pos: &SourcePosition) -> Result<Vec<HirId<HirStmt>>, anyhow::Error> {
        let mut out = Vec::new();
        for (field, trait_sym, _) in self.names.gives_traits(&composer_id).to_vec() {
            let field_name = self.hir.text(field).to_string();
            let trait_name = self.hir.text(trait_sym).to_string();

            let this = self.hir.add(HirExpr::This, pos.clone());
            let field_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(field_name.clone())), pos.clone());
            let access = self.hir.add(HirExpr::Index(this, field_lit, true), pos.clone());
            let is_check = self.hir.add(HirExpr::Is(access, trait_sym), pos.clone());
            let not_check = self.hir.add(HirExpr::Unary(UnOp::Not, is_check), pos.clone());

            let msg = format!("Delegate field '{field_name}' does not provide trait '{trait_name}'");
            let msg_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(msg)), pos.clone());
            let throw = self.hir.add(HirStmt::Throw(msg_lit), pos.clone());
            let then_block = self.hir.add(HirExpr::Block(vec![throw]), pos.clone());
            out.push(self.hir.add(HirStmt::If(not_check, then_block, None), pos.clone()));
        }
        Ok(out)
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

    /// A `this.<name>` member-access callee (a dotted access), used to build internal calls
    /// (qualified `T.method(...)` dispatch).
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
