//! Factory lowering.

use crate::ast::{AstId, Expr, Receiver, Stmt, Symbol, TypeDecl, SlotClause};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{HirExpr, HirSayDecl, HirFnDecl, HirId, HirLiteral, HirMatcher, HirParam, HirStmt, UnOp};

use super::{Factory, Lowerer};

impl<'a> Lowerer<'a> {
    pub(super) fn lower_factory(&mut self, composer_id: AstId<Stmt>, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)], type_pos: &SourcePosition) -> Result<HirId<HirStmt>, anyhow::Error> {
        let (params, body_stmts, init_pos): (_, &[AstId<Stmt>], _) = match &decl.init {
            Some(init_id) => {
                let init_pos = self.ast.pos(init_id).clone();
                let fn_decl = self.ast_fn(init_id);
                let params = self.params(&fn_decl.params)?;
                let stmts = self.ast_block_stmts(&fn_decl.body);
                (params, stmts, init_pos)
            },
            None if !self.all_defaulted_or_opt(decl, field_inits) => {
                return Ok(self.hir.add(HirStmt::Nop, type_pos.clone()));
            },
            None => (Vec::new(), &[], type_pos.clone()),
        };

        let mut body = Vec::new();

        let prev_in_factory = self.in_factory.replace(Factory {
            fields: decl.fields.clone(),
            composer: composer_id,
        });

        let fields = self.factory_fields();
        for &field in &fields {
            let default = field_inits.iter().find(|(f, _)| *f == field).map(|(_, v)| *v);
            let nullable = decl.field_owes(field, self.opt);
            let value = match default {
                Some(v) => Some(self.expr(&v)?),
                None if nullable => Some(self.hir.add(HirExpr::Literal(HirLiteral::Null), type_pos.clone())),
                None => None,
            };
            let clause = decl.field_clauses.iter().find(|(f, _)| *f == field).map(|(_, c)| c);
            body.push(self.field_local_decl(field, value, clause, type_pos));
        }

        for stmt_id in body_stmts {
            body.push(self.stmt(stmt_id)?);
        }

        body.extend(self.factory_epilogue(&init_pos)?);
        self.in_factory = prev_in_factory;

        Ok(self.make_factory_fn(decl.init_name, params, body, &init_pos))
    }

    fn factory_fields(&self) -> Vec<Symbol> {
        let Some(factory) = &self.in_factory else { return Vec::new() };
        let mut fields: Vec<Symbol> = factory.fields.iter().copied().collect();
        // Return fields in stable order.
        fields.sort_by(|a, b| self.hir.text(*a).cmp(self.hir.text(*b)));
        fields
    }

    pub(super) fn factory_epilogue(&mut self, pos: &SourcePosition) -> Result<Vec<HirId<HirStmt>>, anyhow::Error> {
        let Some(composer) = self.in_factory.as_ref().map(|f| f.composer) else { return Ok(Vec::new()) };
        let mut out: Vec<HirId<HirStmt>> = self.factory_fields().into_iter()
            .map(|field| self.copy_field_local(field, pos))
            .collect();
        out.extend(self.synthesize_gives_verifications(composer, pos)?);
        Ok(out)
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

    fn all_defaulted_or_opt(&self, decl: &TypeDecl, field_inits: &[(Symbol, AstId<Expr>)]) -> bool {
        decl.fields.iter().all(|field| {
            decl.field_owes(*field, self.opt) || field_inits.iter().any(|(f, _)| f == field)
        })
    }

    /// Declares a factory's field-local: `say var $<field> [= value]`.
    fn field_local_decl(&mut self, field: Symbol, value: Option<HirId<HirExpr>>, declared: Option<&SlotClause>, pos: &SourcePosition) -> HirId<HirStmt> {
        let name = self.field_local_sym(field);
        // The local stands for the field, so it accepts exactly what the field declares.
        let clause = declared.cloned().unwrap_or_default();
        let decl = HirSayDecl { name, pattern: None, otherwise: None, value, reassignable: true, clause };
        self.hir.add(HirStmt::Say(decl), pos.clone())
    }

    fn synthesize_gives_verifications(&mut self, composer_id: AstId<Stmt>, pos: &SourcePosition) -> Result<Vec<HirId<HirStmt>>, anyhow::Error> {
        let mut out = Vec::new();
        for (field, trait_sym, _) in self.names.gives_traits(&composer_id).to_vec() {
            let field_name = self.hir.text(field).to_string();
            let trait_name = self.hir.text(trait_sym).to_string();

            let this = self.hir.add(HirExpr::This, pos.clone());
            let field_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(field_name.clone())), pos.clone());
            let access = self.hir.add(HirExpr::Index { base: this, member: field_lit, is_dot: true, safe: false }, pos.clone());
            let matcher = HirMatcher::Type { nominal: true, name: trait_sym, shape: None };
            let matcher = self.hir.add(matcher, pos.clone());
            let is_check = self.hir.add(HirExpr::Match(access, matcher), pos.clone());
            let not_check = self.hir.add(HirExpr::Unary(UnOp::Not, is_check), pos.clone());

            let msg = format!("Delegate field '{field_name}' does not provide trait '{trait_name}'");
            let msg_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(msg)), pos.clone());
            let throw = self.hir.add(HirStmt::Throw(msg_lit), pos.clone());
            let then_block = self.hir.add(HirExpr::Block(vec![throw]), pos.clone());
            out.push(self.hir.add(HirStmt::If(not_check, then_block, None), pos.clone()));
        }
        Ok(out)
    }

    pub(super) fn this_method(&mut self, name: &str, pos: &SourcePosition) -> HirId<HirExpr> {
        let this_expr = self.hir.add(HirExpr::This, pos.clone());
        let name_lit = self.hir.add(HirExpr::Literal(HirLiteral::String(name.to_string())), pos.clone());
        self.hir.add(HirExpr::Index { base: this_expr, member: name_lit, is_dot: true, safe: false }, pos.clone())
    }

    fn make_factory_fn(&mut self, name: Symbol, params: Vec<HirParam>, body: Vec<HirId<HirStmt>>, pos: &SourcePosition) -> HirId<HirStmt> {
        let body = self.hir.add(HirExpr::Block(body), pos.clone());
        let receiver = Some(Receiver { pos: pos.clone(), clause: SlotClause::default(), reassignable: true, anchor: false });
        let fn_decl = HirFnDecl { name, sig_pos: pos.clone(), receiver, params, body, clause: SlotClause::default() };
        self.hir.add(HirStmt::Fn(fn_decl), pos.clone())
    }
}
