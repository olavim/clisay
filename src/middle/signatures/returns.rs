//! Return inference.

use crate::middle::hir::{HirExpr, HirId, HirStmt};

use crate::middle::walk::{self, Child};

use super::{BodyFacts, CallableId, Collector, Signatures, TypeTag};

impl<'a> Collector<'a> {
    /// Infers every function's return type tag.
    pub(super) fn infer_ret_tags(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        for stmt in &callables {
            self.sigs.ret_tags.insert(*stmt, TypeTag::Unknown);
        }
        self.converge_groups(Self::record_ret_tag);
    }

    fn record_ret_tag(&mut self, stmt: CallableId) -> bool {
        let tag = self.infer_body_tag(stmt);
        if self.sigs.ret_tags.get(&stmt) == Some(&tag) {
            return false;
        }
        self.sigs.ret_tags.insert(stmt, tag);
        true
    }

    pub(super) fn collect_body_facts(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        for stmt in callables {
            let Some(decl) = Signatures::decl_of(self.hir, stmt) else { continue };
            self.current = Some(stmt);
            let facts = self.read_body_facts(&decl.body);
            self.bodies.insert(stmt, facts);
        }
    }

    fn infer_body_tag(&self, within: CallableId) -> TypeTag {
        let mut joined: Option<TypeTag> = None;
        for ret in self.body_returns(within) {
            let tag = self.classify_return(ret);
            joined = Some(match joined {
                None => tag,
                Some(prev) if prev == tag => prev,
                Some(_) => TypeTag::Unknown,
            });
        }
        joined.unwrap_or(TypeTag::Unknown)
    }

    fn classify_return(&self, expr: &HirId<HirExpr>) -> TypeTag {
        match self.hir.get(expr) {
            HirExpr::This => TypeTag::SelfType,
            HirExpr::Construct(callee, _) => self.resolved().constructed_tag(callee),
            // A callee naming a type is a factory call, so it reports the type it builds.
            HirExpr::Call(callee, _) => match self.resolved().type_named(callee) {
                Some(decl) => TypeTag::Concrete(decl),
                None => self.callee_decl_with_tag(callee)
                    .and_then(|(stmt, recv)| self.sigs.ret_tag_of(&stmt).map(|t| t.resolve(&recv)))
                    .unwrap_or(TypeTag::Unknown),
            },
            _ => TypeTag::Unknown,
        }
    }

    fn read_body_facts(&self, body: &HirId<HirExpr>) -> BodyFacts {
        let mut facts = BodyFacts::default();
        walk::visit_body(self.hir, body, &mut |node| match node {
            Child::Stmt(s) => match self.hir.get(&s) {
                HirStmt::Return(Some(e)) => facts.returns.push(*e),
                _ => {},
            },
            Child::Expr(e) => match self.hir.get(&e) {
                HirExpr::Propagate(operand) => facts.propagates.push(*operand),
                HirExpr::Call(callee, _) | HirExpr::SafeCall(callee, _) => {
                    if let Some(id) = self.callee_decl(callee).map(CallableId::from) {
                        if !facts.callees.contains(&id) {
                            facts.callees.push(id);
                        }
                    }
                },
                _ => {},
            },
        });
        facts
    }

    /// A nested function's returns belong to that function, which the walk treats as a leaf.
    pub(super) fn collect_returns(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        walk::visit_body(self.hir, expr, &mut |node| {
            if let Child::Stmt(s) = node {
                if let HirStmt::Return(Some(e)) = self.hir.get(&s) { out.push(*e); }
            }
        });
    }
}
