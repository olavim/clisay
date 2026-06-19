//! Initializer assembly.
//!
//! Builds a `type`'s initializer: field defaults, the declared body, and the `gives`
//! delegate-verification checks.

use crate::ast::{AstId, Expr, ReturnShape, Stmt, Symbol, TypeDecl};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirParam, HirStmt, UnOp};

use super::Lowerer;

impl<'a> Lowerer<'a> {
    /// Lowers a `type`'s initializer: field defaults, then the declared body, then `gives`
    /// delegate verification.
    pub(super) fn lower_type_init(&mut self, composer_id: AstId<Stmt>, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)], type_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
        let (params, body_stmts, init_pos): (_, &[AstId<Stmt>], _) = match &decl.init {
            Some(init_id) => {
                let init_pos = self.ast.pos(init_id).clone();
                let fn_decl = self.ast_fn(init_id);
                let params = self.params(&fn_decl.params)?;
                let stmts = self.ast_block(&fn_decl.body);
                (params, stmts, init_pos)
            },
            None => (Vec::new(), &[], type_pos.clone()),
        };

        let mut body = Vec::new();

        // Field initializers, spliced ahead of everything. The target is an explicit `this.<field>`
        // member store so the default always lands on the field, never on a same-named outer local.
        for (field, value) in field_inits {
            let field_name = self.hir.text(*field).to_string();
            let target = self.this_method(&field_name, type_pos);
            let value = self.expr(value)?;
            let assign = self.hir.add(HirExpr::Assign(target, value), type_pos.clone());
            body.push(self.hir.add(HirStmt::Expression(assign), type_pos.clone()));
        }
        // The declared body.
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

    /// A `this.<name>` member-access callee (a dotted access), used to build internal calls
    /// (qualified `T.method(...)` dispatch).
    pub(super) fn this_method(&mut self, name: &str, pos: &SourcePosition) -> HirId<HirExpr> {
        let this_expr = self.hir.add(HirExpr::This, pos.clone());
        let name_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(name.to_string())), pos.clone());
        self.hir.add(HirExpr::Index(this_expr, name_lit, true), pos.clone())
    }

    /// Wraps an init body into its `HirStmt::Fn`. An init always yields the constructed
    /// instance, so its return shape is non-null.
    fn make_init_fn(&mut self, name: Symbol, params: Vec<HirParam>, body: Vec<HirId<HirStmt>>, pos: &SourcePosition) -> HirId<HirStmt> {
        let body = self.hir.add(HirExpr::Block(body), pos.clone());
        let fn_decl = HirFnDecl { name, params, body, ret: ReturnShape::NonNull };
        self.hir.add(HirStmt::Fn(fn_decl), pos.clone())
    }
}
