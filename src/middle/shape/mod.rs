//! Shape checks: what a declaration promises, read off the declaration alone.
//!
//! Nothing here asks what happened on the way to a program point. Running before the flow pass
//! means a malformed declaration is reported as one, rather than as whatever the flow pass makes
//! of it.

mod construct;
mod traits;

use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Hir, HirExpr, HirId, HirLiteral, HirStmt, HirTypeDecl, Symbol};
use crate::middle::signatures::Signatures;
use crate::middle::walk::{children_of, Child};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures) -> Result<(), anyhow::Error> {
    let shape = Shape { hir, bindings, sigs };
    shape.visit(Child::Stmt(hir.get_root()))
}

struct Shape<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
}

impl<'a> Diagnose for Shape<'a> {
    fn hir(&self) -> &Hir { self.hir }
}

impl<'a> Shape<'a> {
    fn visit(&self, node: Child) -> Result<(), anyhow::Error> {
        match node {
            Child::Expr(e) => self.expr(&e)?,
            Child::Stmt(s) => self.stmt(&s)?,
        }
        for child in children_of(self.hir, node) {
            self.visit(child)?;
        }
        Ok(())
    }

    fn expr(&self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(node) {
            HirExpr::Construct(callee, args, brace) => self.construct(node, callee, args, brace)?,
            HirExpr::Literal(HirLiteral::Lambda(decl)) => self.visit(Child::Expr(decl.body))?,
            _ => {},
        }
        Ok(())
    }

    fn stmt(&self, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(node) {
            HirStmt::Fn(decl) => self.visit(Child::Expr(decl.body))?,
            HirStmt::Type(decl) => {
                self.check_method_overrides(decl)?;
                self.check_req_conformance(decl)?;
                self.type_body(decl)?;
            },
            HirStmt::Trait(decl) => self.type_body(decl)?,
            _ => {},
        }
        Ok(())
    }

    fn type_body(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        self.visit(Child::Stmt(decl.init))?;
        for method in &decl.methods {
            self.visit(Child::Stmt(*method))?;
        }
        Ok(())
    }

    /// The layout of a tracked concrete type.
    fn layout_of(&self, name: Symbol) -> Option<&'a TypeLayout> {
        self.sigs.types_by_name.get(&name).map(|stmt| self.bindings.type_layout(stmt))
    }
}
