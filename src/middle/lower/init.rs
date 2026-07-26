//! Factory lowering.

use std::collections::HashSet;

use crate::ast::{AstId, Expr, ReturnShape, Stmt, Symbol, TypeDecl};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirSlotClause, HirExpr, HirFieldInit, HirFnDecl, HirId, HirLiteral, HirParam, HirStmt, UnOp};

use super::Lowerer;

impl<'a> Lowerer<'a> {
    /// Lowers a `type`'s factory. Each field becomes a `mut` field-local. The declared body's
    /// `this.<field>` accesses read and write those locals. The body ends by copying each local
    /// onto `this` and verifying `gives`. A type with no user factory that cannot fill every
    /// non-null field from defaults gets no factory at all, lowered to a `Nop`.
    pub(super) fn lower_factory(&mut self, composer_id: AstId<Stmt>, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)], type_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
        let (params, body_stmts, init_pos): (_, &[AstId<Stmt>], _) = match &decl.init {
            Some(init_id) => {
                let init_pos = self.ast.pos(init_id).clone();
                let fn_decl = self.ast_fn(init_id);
                let params = self.params(&fn_decl.params)?;
                let stmts = self.ast_block(&fn_decl.body);
                (params, stmts, init_pos)
            },
            None if !self.all_defaulted_or_opt(decl, field_inits) => {
                return Ok(self.hir.add(HirStmt::Nop, type_pos.clone()));
            },
            None => (Vec::new(), &[], type_pos.clone()),
        };

        let mut body = Vec::new();

        // A factory body and its field defaults name `this` only as `this.<field>`, and each field
        // access desugars to the field's local. A param shadows a same-named field's bare access.
        let params_set: HashSet<Symbol> = match &decl.init {
            Some(init_id) => self.ast_fn(init_id).params.iter()
                .filter_map(|p| match self.ast.get(&p.name) { Expr::Identifier(s) => Some(*s), _ => None })
                .collect(),
            None => HashSet::new(),
        };
        let saved_in_factory = self.in_factory.replace((decl.fields.clone(), params_set));

        // A stable field order keeps the synthesized locals deterministic.
        let mut fields: Vec<Symbol> = decl.fields.iter().copied().collect();
        fields.sort_by(|a, b| self.hir.text(*a).cmp(self.hir.text(*b)));

        // Declare a `mut` field-local per field, seeded with its default, null for `opt`, or unassigned.
        for &field in &fields {
            let default = field_inits.iter().find(|(f, _)| *f == field).map(|(_, v)| *v);
            let nullable = decl.nullable_fields.contains(&field);
            let value = match default {
                Some(v) => Some(self.expr(&v)?),
                None if nullable => Some(self.hir.add(HirExpr::Literal(HirLiteral::Null), type_pos.clone())),
                None => None,
            };
            body.push(self.field_local_decl(field, value, nullable, type_pos));
        }

        // The declared body, with each `this.<field>` now a field-local.
        for stmt_id in body_stmts {
            body.push(self.stmt(stmt_id)?);
        }

        self.in_factory = saved_in_factory;

        // Copy each definitely-assigned field-local onto `this`, then verify `gives`. `this` is
        // handed back by the factory epilogue.
        for &field in &fields {
            body.push(self.copy_field_local(field, &init_pos));
        }
        body.extend(self.synthesize_gives_verifications(composer_id, &init_pos)?);

        Ok(self.make_factory_fn(decl.init_name, params, body, &init_pos))
    }

    /// A factory epilogue copy: `this.<field> = $<field>`, writing a field-local onto the instance.
    fn copy_field_local(&mut self, field: Symbol, pos: &SourcePosition) -> HirId<HirStmt> {
        let field_name = self.hir.text(field).to_string();
        let target = self.this_method(&field_name, pos);
        let local = self.field_local_sym(field);
        let value = self.hir.add(HirExpr::Identifier(local), pos.clone());
        let assign = self.hir.add(HirExpr::Assign(target, value), pos.clone());
        self.hir.add(HirStmt::Expression(assign), pos.clone())
    }

    /// Whether every field can be filled without a factory: it has a default or is nullable (so it
    /// defaults to null). Such a type synthesizes a parameterless factory. Any other type does not.
    fn all_defaulted_or_opt(&self, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)]) -> bool {
        decl.fields.iter().all(|field| {
            decl.nullable_fields.contains(field) || field_inits.iter().any(|(f, _)| f == field)
        })
    }

    /// Declares a factory's field-local: `say mut $<field> [= value]`. A nullable field is `opt`, so
    /// a null seed and later null writes are accepted.
    fn field_local_decl(&mut self, field: Symbol, value: Option<HirId<HirExpr>>, nullable: bool, pos: &SourcePosition) -> HirId<HirStmt> {
        let name = self.field_local_sym(field);
        let clause = if nullable {
            HirSlotClause { names: vec![self.opt], ..Default::default() }
        } else {
            HirSlotClause::default()
        };
        let field_init = HirFieldInit { name, value, nullable, mutable: true, clause };
        self.hir.add(HirStmt::Say(field_init), pos.clone())
    }

    /// Builds the construction-time verification for each `gives` delegate:
    /// `if !(this.<field> is Trait) { throw "<message>"; }`.
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

    fn make_factory_fn(&mut self, name: Symbol, params: Vec<HirParam>, body: Vec<HirId<HirStmt>>, pos: &SourcePosition) -> HirId<HirStmt> {
        let body = self.hir.add(HirExpr::Block(body), pos.clone());
        let fn_decl = HirFnDecl { name, sig_pos: pos.clone(), params, body, ret: ReturnShape::Inferred, clause: HirSlotClause::default() };
        self.hir.add(HirStmt::Fn(fn_decl), pos.clone())
    }
}
