//! Propagation inference.

use crate::middle::bind::Declared;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt};
use crate::middle::obligations::Obligations;
use super::{CallableId, Collector, RetSig, Signatures, TypeTag};

impl<'a> Collector<'a> {
    pub(super) fn infer_propagated(&mut self) {
        self.converge_groups(Self::record_propagations);
    }

    /// Adds what a body hands back to its function's return shape. Answers whether anything was added.
    fn record_propagations(&mut self, stmt: CallableId) -> bool {
        let Some(decl) = Signatures::decl_of(self.hir, stmt) else { return false };
        let mut gained = RetSig::default();
        for operand in self.body_propagates(stmt) {
            gained.obligations.extend(&self.shape_of(operand, decl).obligations);
        }

        if decl.has_no_return_clause() {
            let returns = self.body_returns(stmt);
            let mut all_void = !returns.is_empty();
            for ret in returns {
                let shape = self.shape_of(ret, decl);
                all_void &= shape.void;
                gained.obligations.extend(&shape.obligations);
            }
            gained.void = all_void;
        }
        self.sigs.fns.get_mut(&stmt).unwrap().ret.absorb(&gained)
    }

    fn shape_of(&self, expr: &HirId<HirExpr>, enclosing: &HirFnDecl) -> RetSig {
        match self.hir.get(expr) {
            HirExpr::Call(callee, _) => match self.is_err_call(expr) {
                true => RetSig::owing(Obligations::from([self.fails])),
                false => self.callee_ret(callee).cloned().unwrap_or_default(),
            },
            HirExpr::SafeCall(callee, _) => {
                let mut shape = self.shape_of(callee, enclosing);
                if let Some(from) = self.callee_ret(callee) {
                    shape.absorb(from);
                }
                shape
            },
            HirExpr::Index { base, safe: true, .. } => self.shape_of(base, enclosing),
            HirExpr::Literal(HirLiteral::Null) => RetSig::owing(Obligations::from([self.opt])),
            HirExpr::Identifier(_) => RetSig::owing(self.binding_owes(expr, enclosing)),
            _ => RetSig::default(),
        }
    }

    fn callee_ret(&self, callee: &HirId<HirExpr>) -> Option<&RetSig> {
        self.callee_decl(callee).and_then(|s| self.sigs.fn_sig_of(&s)).map(|f| &f.ret)
    }

    fn binding_owes(&self, name: &HirId<HirExpr>, enclosing: &HirFnDecl) -> Obligations {
        match self.bindings.declaration_of(name) {
            Some(Declared::Param(at)) => enclosing.params.iter()
                .find(|p| p.name == at)
                .map_or_else(Obligations::new, |p| p.clause.owed()),
            Some(Declared::Say(at)) => match self.hir.get(&at) {
                HirStmt::Say(init) => init.clause.owed(),
                _ => Obligations::new(),
            },
            _ => Obligations::new(),
        }
    }

    pub(super) fn callee_decl(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        self.callee_decl_with_tag(callee).map(|(stmt, _)| stmt)
    }

    pub(super) fn callee_decl_with_tag(&self, callee: &HirId<HirExpr>) -> Option<(HirId<HirStmt>, TypeTag)> {
        match self.hir.get(callee) {
            HirExpr::Identifier(_) => self.bindings.function_named(callee).map(|stmt| (stmt, TypeTag::Unknown)),
            HirExpr::Index { base: receiver, member, safe: false, .. } => {
                let (owner, tag) = self.receiver_decl(receiver)?;
                let method = self.sigs.method_of(owner, self.hir.member_symbol(member)?)?;
                Some((method, tag))
            },
            _ => None,
        }
    }

    fn receiver_decl(&self, receiver: &HirId<HirExpr>) -> Option<(HirId<HirStmt>, TypeTag)> {
        match self.hir.get(receiver) {
            HirExpr::This => self.current_type().map(|owner| (owner, TypeTag::SelfType)),
            HirExpr::Anchor(inner) => self.receiver_decl(inner),
            HirExpr::Index { base, member: field, safe: false, .. } => {
                let (owner, _) = self.receiver_decl(base)?;
                let given = self.resolved().given_trait(owner, self.hir.member_symbol(field)?)?;
                Some((given, TypeTag::Concrete(given)))
            },
            _ => None,
        }
    }

    fn current_type(&self) -> Option<HirId<HirStmt>> {
        self.current.and_then(|body| self.sigs.owner_of(body))
    }

    pub(super) fn body_returns(&self, stmt: CallableId) -> &[HirId<HirExpr>] {
        self.bodies.get(&stmt).map_or(&[], |b| b.returns.as_slice())
    }

    fn body_propagates(&self, stmt: CallableId) -> &[HirId<HirExpr>] {
        self.bodies.get(&stmt).map_or(&[], |b| b.propagates.as_slice())
    }
}
