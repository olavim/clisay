//! Per-type field-assignment facts: which fields an `init` assigns, and where methods assign fields.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{HirExpr, HirId, HirLiteral, HirStmt, HirTypeDecl, Symbol};

use super::Collector;
use super::walk::Child;

impl<'a> Collector<'a> {
    /// The fields a type's `init` assigns directly: defaults, `this.f =`, and bare `f =`.
    /// Assignments inside a called helper do not count, since the helper is opaque to init.
    pub(super) fn init_fields(&self, decl: &HirTypeDecl) -> HashSet<Symbol> {
        let mut assigns = Vec::new();
        if let HirStmt::Fn(init) = self.hir.get(&decl.init) {
            self.scan_field_assigns(&init.body, &decl.fields, &mut assigns);
        }
        assigns.into_iter().map(|(field, _)| field).collect()
    }

    /// The field assignments found in the type's methods, keyed by field (first one wins). These
    /// do not initialize the field, but they let a definition error point at the misplaced assign.
    pub(super) fn method_field_assigns(&self, decl: &HirTypeDecl) -> HashMap<Symbol, HirId<HirExpr>> {
        let mut assigns = Vec::new();
        for method in &decl.methods {
            if let HirStmt::Fn(m) = self.hir.get(method) {
                self.scan_field_assigns(&m.body, &decl.fields, &mut assigns);
            }
        }
        let mut map = HashMap::new();
        for (field, node) in assigns {
            map.entry(field).or_insert(node);
        }
        map
    }

    /// Collects each direct field assignment as `(field, lhs node)`: `this.f =` and bare `f =`.
    fn scan_field_assigns(&self, expr: &HirId<HirExpr>, fields: &HashSet<Symbol>, out: &mut Vec<(Symbol, HirId<HirExpr>)>) {
        if let HirExpr::Assign(target, _) = self.hir.get(expr) {
            match self.hir.get(target) {
                HirExpr::Index(obj, member, true) if matches!(self.hir.get(obj), HirExpr::This) => {
                    if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                        if let Some(sym) = self.hir.symbol_of(name) { out.push((sym, *target)); }
                    }
                },
                HirExpr::Identifier(name) if fields.contains(name) => { out.push((*name, *target)); },
                _ => {},
            }
        }
        for child in self.children_of_expr(expr) {
            match child {
                Child::Expr(e) => self.scan_field_assigns(&e, fields, out),
                Child::Stmt(s) => self.scan_field_assigns_stmt(&s, fields, out),
            }
        }
    }

    fn scan_field_assigns_stmt(&self, stmt: &HirId<HirStmt>, fields: &HashSet<Symbol>, out: &mut Vec<(Symbol, HirId<HirExpr>)>) {
        for child in self.children_of_stmt(stmt) {
            match child {
                Child::Expr(e) => self.scan_field_assigns(&e, fields, out),
                Child::Stmt(s) => self.scan_field_assigns_stmt(&s, fields, out),
            }
        }
    }
}
