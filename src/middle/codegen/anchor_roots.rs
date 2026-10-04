//! Decides where a path anchor like `&x.y` needs a check that `x` was not rebound since the anchor
//! was formed.

use fnv::{FnvHashMap, FnvHashSet};

use crate::compiler_error;
use crate::middle::anchors::AnchorChain;
use crate::middle::bind::Bindings;
use crate::middle::hir::{Hir, HirExpr, HirId};
use crate::middle::ir::Inst;
use crate::middle::walk::{visit_body, Child};

use super::Compiler;

/// What a function body can rebind, and where a rebind can reach each anchor it passes to a call.
pub(super) struct BodyRebinds {
    rebound: ReboundBindings,
    /// Each anchor a call passes, with the arguments evaluated after it.
    passed_before: FnvHashMap<HirId<HirExpr>, Vec<HirId<HirExpr>>>,
}

/// The bindings that code under some scopes can rebind.
#[derive(Default)]
struct ReboundBindings {
    decls: FnvHashSet<usize>,
    this: bool,
}

impl ReboundBindings {
    fn scan(hir: &Hir, bindings: &Bindings, scopes: &[HirId<HirExpr>]) -> ReboundBindings {
        let mut rebound = ReboundBindings::default();
        for scope in scopes {
            visit_body(hir, scope, &mut |child| {
                let Child::Expr(e) = child else { return };
                for name in hir.rebound_names(&e) {
                    let Some(chain) = AnchorChain::of_binding(hir, bindings, &name) else { continue };
                    let Some(binding) = chain.rebind_target() else { continue };
                    match hir.get(&binding) {
                        HirExpr::This => rebound.this = true,
                        _ => rebound.decls.extend(bindings.declaring_node(&binding)),
                    }
                }
            });
        }
        rebound
    }

    fn contains(&self, hir: &Hir, bindings: &Bindings, binding: &HirId<HirExpr>) -> bool {
        match hir.get(binding) {
            HirExpr::This => self.this,
            _ => bindings.declaring_node(binding).is_some_and(|decl| self.decls.contains(&decl)),
        }
    }
}

/// A root check: the root's slot, whether the root is read through the anchor in it, and the slot
/// that saved the root's value when the anchor was formed.
#[derive(Clone, Copy)]
struct RootCheck {
    root: u8,
    root_is_anchor: bool,
    formed_on: u8,
}

impl<'a> Compiler<'a> {
    /// Saves the root's value for the checks a path anchor will need.
    pub(super) fn after_forming_path_anchor(&mut self, site: &HirId<HirExpr>, target: u8) -> Result<(), anyhow::Error> {
        let Some(anchor) = AnchorChain::of_anchor(self.hir, self.bindings, site) else { return Ok(()) };
        if let Some(check) = self.root_check(site)? {
            if check.formed_on != target {
                match anchor.root_is_anchor {
                    true => self.emit(Inst::LoadAnchor(anchor.root_slot), site),
                    false => self.emit(Inst::LoadLocal(anchor.root_slot), site),
                }
                self.emit(Inst::StoreTempPop(check.formed_on), site);
            }
        }
        if self.drop_guards || self.force_checks {
            match anchor.root_is_anchor {
                true => self.emit(Inst::LoadLocal(anchor.root_slot), site),
                false => self.emit(Inst::PushSlotAnchor(anchor.root_slot, self.slot_witness_set_pool_id(anchor.root_slot)), site),
            }
            self.emit(Inst::RecordAnchorRoot(!self.drop_guards as u8), site);
        }
        Ok(())
    }

    pub(super) fn emit_call_root_checks(&mut self, passed: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for anchor in passed {
            if let Some(check) = self.root_check(anchor)? {
                self.emit_root_check(check, anchor);
            }
        }
        Ok(())
    }

    pub(super) fn emit_binding_root_check(&mut self, name: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(bound) = self.bindings.anchor_binding(name) else { return Ok(()) };
        if let Some(check) = self.root_check(&bound)? {
            self.emit_root_check(check, name);
        }
        Ok(())
    }

    fn root_check(&mut self, anchor_expr: &HirId<HirExpr>) -> Result<Option<RootCheck>, anyhow::Error> {
        if self.drop_guards {
            return Ok(None);
        }

        let Some(anchor) = AnchorChain::of_anchor(self.hir, self.bindings, anchor_expr) else { return Ok(None) };
        let Some(site) = anchor.formed_at() else { return Ok(None) };
        let Some(body) = self.current_body else { return Ok(None) };

        if !self.body_rebinds.contains_key(&body) {
            let scanned = self.scan_body_rebinds(&body);
            self.body_rebinds.insert(body, scanned);
        }

        let rebinds = &self.body_rebinds[&body];
        let replaced = match rebinds.passed_before.get(&site) {
            Some(later) => ReboundBindings::scan(self.hir, self.bindings, later).contains(self.hir, self.bindings, &anchor.root),
            None => rebinds.rebound.contains(self.hir, self.bindings, &anchor.root),
        };

        if !replaced {
            return Ok(None);
        }

        let Some(slots) = self.bindings.anchor_path_slots(&site) else {
            compiler_error!(self, &site, "a path anchor reserved no slots");
        };

        let formed_on = match (anchor.container_is_root(), slots.saved_root()) {
            (true, _) => slots.target(),
            (false, Some(saved)) => saved,
            (false, None) => compiler_error!(self, &site, "a path anchor whose root can be replaced reserved no slot to save it"),
        };

        Ok(Some(RootCheck { root: anchor.root_slot, root_is_anchor: anchor.root_is_anchor, formed_on }))
    }

    fn scan_body_rebinds(&self, body: &HirId<HirExpr>) -> BodyRebinds {
        let mut passed_before = FnvHashMap::default();
        visit_body(self.hir, body, &mut |child| {
            let Child::Expr(e) = child else { return };
            let (HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args)) = self.hir.get(&e) else { return };
            let passed = self.hir.call_anchors(callee, args);
            for (at, anchor) in passed.iter().enumerate() {
                passed_before.insert(*anchor, passed[at + 1..].to_vec());
            }
        });
        BodyRebinds { rebound: ReboundBindings::scan(self.hir, self.bindings, &[*body]), passed_before }
    }

    fn emit_root_check(&mut self, check: RootCheck, node: &HirId<HirExpr>) {
        self.emit(Inst::CheckAnchorRoot(check.root, check.root_is_anchor as u8, check.formed_on), node);
    }
}
