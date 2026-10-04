//! The order a write evaluates its keys and value in. In `a[f()][g()] = h()`, the calls run in
//! source order first, and only then does the walk go from `a` to the container it writes.

use crate::middle::bind::Bindings;
use crate::middle::hir::{access_path_steps, AccessStep, Hir, HirExpr, HirId, HirLiteral, Symbol};
use crate::middle::walk::{visit_body, Child};

/// What codegen needs to know to compile an assignment.
pub enum AssignStrategy {
    /// `x = v`, `this = v` or `@r = v`.
    Rebind,
    /// `a[i].b = v`. The keys and the value are evaluated first, and the walk to the container last.
    WalkLast {
        /// Keys the walk needs but cannot safely evaluate a second time, in source order. The write
        /// keeps each one for the walk. In `a[f()][0] = v`, `f()` is saved, so `f` runs once.
        saved_write_path_keys: Vec<HirId<HirExpr>>,
        /// The keys and value that could evaluate to a container the walk passes through, or to a
        /// value containing one. Each is marked shared before the walk, so the walk forks that
        /// container instead of writing into it. In `a.b.c = a` the value `a` is marked, and in
        /// `d[d] = 1` the key `d` is. In `a.b = c.d` nothing is.
        ///
        /// A saved key or the last key is also marked when something after it could write into
        /// it. In `d[k] = (k[0] = 5)` that write then forks `k`, and the key stays `[1]`.
        shared: Vec<HirId<HirExpr>>,
    },
}

pub(super) fn assign_strategy(hir: &Hir, bindings: &Bindings, wants_anchor_receiver: bool, target: HirId<HirExpr>, value: HirId<HirExpr>) -> AssignStrategy {
    WriteOrder { hir, bindings, wants_anchor_receiver }.strategy(target, value)
}

struct WriteOrder<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    /// Whether `this` is a `&var this` receiver.
    wants_anchor_receiver: bool,
}

impl WriteOrder<'_> {
    fn strategy(&self, target: HirId<HirExpr>, value: HirId<HirExpr>) -> AssignStrategy {
        let (root_expr, steps) = access_path_steps(self.hir, &target);

        let Some((root, implied_steps)) = self.path_start(&root_expr) else {
            // `f()[0] = v` starts at a value, not a name. The check pass refuses it, so codegen
            // never reads this `Rebind`.
            return AssignStrategy::Rebind;
        };

        if implied_steps == 0 && steps.is_empty() {
            return AssignStrategy::Rebind;
        }

        // The walk takes every step but the last, and the store takes the last key itself. In
        // `a[i][j] = v` the walk needs `i`, and the store takes `j`. A path through a `Ref` also
        // needs the `Ref`, so in `@(get())[i] = v` the walk needs what `get()` returned.
        let (walk_steps, store_key) = match steps.split_last() {
            Some((last, before_last)) => (before_last, (!last.is_dot).then_some(last.key)),
            None => (&steps[..], None),
        };
        let ref_holder = match self.hir.get(&root_expr) {
            HirExpr::RefValue { holder, .. } => Some(*holder),
            _ => None,
        };
        let walk_keys: Vec<HirId<HirExpr>> = ref_holder.into_iter().chain(bracket_keys(walk_steps)).collect();

        // What the write evaluates, in source order. The walk's keys come first.
        let evaluated: Vec<HirId<HirExpr>> = walk_keys.iter().copied().chain(store_key).chain([value]).collect();
        let saved_write_path_keys: Vec<HirId<HirExpr>> = walk_keys.iter().enumerate()
            .filter(|(position, key)| !self.safe_to_evaluate_twice(key, &evaluated[position + 1..]))
            .map(|(_, key)| *key)
            .collect();

        let containers_on_path = implied_steps + steps.len();
        let mut shared: Vec<HirId<HirExpr>> = bracket_keys(&steps).chain([value])
            .filter(|expr| self.can_reach_path(expr, root, containers_on_path))
            .collect();

        // A key evaluated before the walk must not change before the walk. In `d[k] = (k[0] = 5)`
        // the key is `[1]`, so `k` is marked as shared and the later write forks it. A `Ref` is
        // never forked, so its holder doesn't need a mark.
        for (position, key) in evaluated.iter().enumerate() {
            let used_later = saved_write_path_keys.contains(key) || Some(*key) == store_key;
            if used_later && Some(*key) != ref_holder && !shared.contains(key)
                && self.key_might_change_later(key, &evaluated[position + 1..]) {
                shared.push(*key);
            }
        }
        AssignStrategy::WalkLast { saved_write_path_keys, shared }
    }

    /// The root a path's first expression reads from, and how many steps below the root it
    /// already is. An anchor `b = &a[0]` is one step below `a`.
    fn path_start(&self, expr: &HirId<HirExpr>) -> Option<(RootName, usize)> {
        match self.hir.get(expr) {
            HirExpr::Identifier(name) => match self.bindings.anchor_binding(expr) {
                Some(anchor) => {
                    let (origin, steps) = access_path_steps(self.hir, &anchor);
                    self.path_start(&origin).map(|(root, implied_steps)| (root, implied_steps + steps.len()))
                },
                None => Some((RootName::Local(*name), 0)),
            },
            HirExpr::This => Some((self.this_root(), 0)),
            HirExpr::RefValue { .. } => Some((RootName::Ref, 0)),
            _ => None,
        }
    }

    fn this_root(&self) -> RootName {
        match self.wants_anchor_receiver {
            true => RootName::AnchoredThis,
            false => RootName::This,
        }
    }

    /// Whether an expression could evaluate to a container the walk passes through, or to a value
    /// containing one. In `a.b.c = v` the walk passes through `a` and `a.b`.
    fn can_reach_path(&self, expr: &HirId<HirExpr>, root: RootName, containers_on_path: usize) -> bool {
        self.hir.reads_handed_back(expr).iter().any(|read| {
            let (base, steps) = access_path_steps(self.hir, read);
            match self.path_start(&base) {
                Some((read_root, implied_steps)) if read_root == root => implied_steps + steps.len() < containers_on_path,
                // Another root can still name the same storage when both could lie inside a
                // `Ref`. How deep is not known, so any read of it counts.
                Some((read_root, _)) => read_root.may_overlap(root),
                None => self.can_reach_path(&base, root, containers_on_path),
            }
        })
    }

    /// Whether an expression could change what a root holds. It could assign to the root or into
    /// it, as `i = 2` or `k[0] = 5` do, or hand it to a call as an anchor, as `f(&k)` and
    /// `&k.push(5)` do. With a root that could lie inside a `Ref`, every call counts, since the
    /// callee could change any `Ref`.
    fn changes_root(&self, expr: &HirId<HirExpr>, root: RootName) -> bool {
        let mut changes = false;
        visit_body(self.hir, expr, &mut |child| {
            let Child::Expr(inner) = child else { return };
            changes |= match self.hir.get(&inner) {
                HirExpr::Assign(target, _) | HirExpr::CompoundAssign(target, _, _) | HirExpr::Anchor(target) => {
                    let (base, _) = access_path_steps(self.hir, target);
                    self.path_start(&base).is_some_and(|(written, _)| written.may_overlap(root))
                },
                HirExpr::Call(..) | HirExpr::SafeCall(..) => root.may_be_inside_a_ref(),
                _ => false,
            };
        });
        changes
    }

    /// Whether the walk can evaluate a key a second time after `later` has run, and still get the same value.
    /// In `a[i][j] = v` the walk can read `i` again safely, but in `a[i][j] = (i = 2)` it cannot.
    fn safe_to_evaluate_twice(&self, key: &HirId<HirExpr>, later: &[HirId<HirExpr>]) -> bool {
        match self.hir.get(key) {
            HirExpr::Literal(HirLiteral::Null | HirLiteral::Boolean(_) | HirLiteral::Number(_) | HirLiteral::String(_)) => true,
            HirExpr::Identifier(_) => !self.key_might_change_later(key, later),
            _ => false,
        }
    }

    /// Whether something in `later` could change the value `key` evaluated to.
    fn key_might_change_later(&self, key: &HirId<HirExpr>, later: &[HirId<HirExpr>]) -> bool {
        self.hir.reads_handed_back(key).iter().any(|read| {
            let (base, _) = access_path_steps(self.hir, read);
            match self.path_start(&base) {
                Some((root, _)) => later.iter().any(|expr| self.changes_root(expr, root)),
                None => self.key_might_change_later(&base, later),
            }
        })
    }
}

/// The keys of a path's `[k]` steps, as `i` and `j` in `a[i].b[j]`.
fn bracket_keys(steps: &[AccessStep]) -> impl Iterator<Item = HirId<HirExpr>> + '_ {
    steps.iter().filter(|step| !step.is_dot).map(|step| step.key)
}

#[derive(Clone, Copy, PartialEq)]
enum RootName {
    Local(Symbol),
    This,
    AnchoredThis,
    Ref,
}

impl RootName {
    fn may_be_inside_a_ref(self) -> bool {
        matches!(self, RootName::AnchoredThis | RootName::Ref)
    }

    /// Whether two roots could name the same storage.
    fn may_overlap(self, other: RootName) -> bool {
        self == other || (self.may_be_inside_a_ref() && other.may_be_inside_a_ref())
    }
}
