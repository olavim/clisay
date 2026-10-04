use crate::middle::bind::{Bindings, Place};
use crate::middle::hir::{access_path_steps, same_scalar, Hir, HirExpr, HirId, AccessStep};

/// The anchor paths an anchor goes through. After
/// ```text
/// say var s = &o.f;
/// say var t = &s;
/// ```
/// the chain of `&t.g` has root `o` and segments `&o.f`, `&s` and `&t.g`.
pub struct AnchorChain {
    pub root: HirId<HirExpr>,
    pub root_slot: u8,
    pub root_is_anchor: bool,
    pub segments: Vec<AnchorChainSegment>,
}

pub struct AnchorChainSegment {
    pub anchor: HirId<HirExpr>,
    /// The name the anchor's path starts at.
    pub root: HirId<HirExpr>,
    // Empty steps means this is an expression like `say var a = &b`.
    pub steps: Vec<AccessStep>,
}

impl AnchorChain {
    pub fn container_is_root(&self) -> bool {
        self.segments.iter().map(|segment| segment.steps.len()).sum::<usize>() == 1
    }

    pub fn keys(&self) -> Vec<AnchorKey> {
        self.segments.iter().flat_map(|segment| anchor_keys(segment.anchor, &segment.steps)).collect()
    }

    pub fn formed_at(&self) -> Option<HirId<HirExpr>> {
        self.segments.iter().rev().find(|segment| !segment.steps.is_empty()).map(|segment| segment.anchor)
    }

    /// The binding whose value a write to the anchor rebinds. None when the write lands
    /// inside a container.
    pub fn rebind_target(&self) -> Option<HirId<HirExpr>> {
        self.segments.iter().all(|segment| segment.steps.is_empty()).then_some(self.root)
    }

    /// The chain of an anchor expression.
    pub fn of_anchor(hir: &Hir, bindings: &Bindings, anchor: &HirId<HirExpr>) -> Option<AnchorChain> {
        let HirExpr::Anchor(path) = hir.get(anchor) else { return None };
        let (root, steps) = access_path_steps(hir, path);
        Self::extend(hir, bindings, vec![AnchorChainSegment { anchor: *anchor, root, steps }])
    }

    pub fn of_binding(hir: &Hir, bindings: &Bindings, name: &HirId<HirExpr>) -> Option<AnchorChain> {
        match anchor_binding_local(hir, bindings, name) {
            Some((anchor, _)) => Self::of_anchor(hir, bindings, &anchor),
            None => Self::finish(bindings, *name, Vec::new()),
        }
    }

    fn extend(hir: &Hir, bindings: &Bindings, mut segments: Vec<AnchorChainSegment>) -> Option<AnchorChain> {
        loop {
            let root = segments.last().expect("a chain starts with a segment").root;
            let Some((anchor, path)) = anchor_binding_local(hir, bindings, &root) else {
                segments.reverse();
                return Self::finish(bindings, root, segments);
            };
            let (path_root, steps) = access_path_steps(hir, &path);
            segments.push(AnchorChainSegment { anchor, root: path_root, steps });
        }
    }

    fn finish(bindings: &Bindings, root: HirId<HirExpr>, segments: Vec<AnchorChainSegment>) -> Option<AnchorChain> {
        let Some(Place::Local(root_slot)) = bindings.place_of(&root) else { return None };
        let root_is_anchor = bindings.anchor_binding(&root).is_some();
        Some(AnchorChain { root, root_slot, root_is_anchor, segments })
    }
}

fn anchor_binding_local(hir: &Hir, bindings: &Bindings, name: &HirId<HirExpr>) -> Option<(HirId<HirExpr>, HirId<HirExpr>)> {
    let anchor = bindings.anchor_binding(name)?;
    let HirExpr::Anchor(path) = hir.get(&anchor) else { unreachable!("an anchor binding is bound to an anchor") };
    Some((anchor, *path))
}

fn anchor_keys(anchor: HirId<HirExpr>, steps: &[AccessStep]) -> Vec<AnchorKey> {
    steps.iter().enumerate()
        .map(|(step, at)| AnchorKey { anchor, step, at: *at })
        .collect()
}

pub fn path_overlaps(hir: &Hir, bindings: &Bindings, args: &[HirId<HirExpr>]) -> Vec<AnchorOverlap> {
    let chains: Vec<Option<(u8, Vec<AnchorKey>)>> = args.iter()
        .map(|arg| AnchorChain::of_anchor(hir, bindings, arg).map(|chain| (chain.root_slot, chain.keys())))
        .collect();

    let mut pairs = Vec::new();
    for (i, a) in chains.iter().enumerate() {
        let Some((a_root, a_keys)) = a else { continue };
        for (b, second) in chains[i + 1..].iter().zip(&args[i + 1..]) {
            // Different roots cannot meet, since every other way of reaching one object copies it.
            let Some((_, b_keys)) = b.as_ref().filter(|(b_root, _)| b_root == a_root) else { continue };
            if let Some(undecided) = maybe_overlapping_anchor_path_keys(hir, a_keys, b_keys) {
                pairs.push(AnchorOverlap { second: *second, maybe_overlapping_keys: undecided });
            }
        }
    }
    pairs
}

fn maybe_overlapping_anchor_path_keys(hir: &Hir, a: &[AnchorKey], b: &[AnchorKey]) -> Option<Vec<(AnchorKey, AnchorKey)>> {
    let mut undecided = Vec::new();
    for (x, y) in a.iter().zip(b) {
        match compare_keys(hir, x.at, y.at) {
            KeyMatch::Different => return None,
            KeyMatch::Unknown => undecided.push((*x, *y)),
            KeyMatch::Same => {},
        }
    }
    Some(undecided)
}

#[derive(Clone, Copy)]
pub struct AnchorKey {
    pub anchor: HirId<HirExpr>,
    pub step: usize,
    pub at: AccessStep,
}

pub struct AnchorOverlap {
    pub second: HirId<HirExpr>,
    pub maybe_overlapping_keys: Vec<(AnchorKey, AnchorKey)>,
}

pub enum KeyMatch {
    Same,
    Different,
    Unknown,
}

fn compare_keys(hir: &Hir, a: AccessStep, b: AccessStep) -> KeyMatch {
    let (HirExpr::Literal(x), HirExpr::Literal(y)) = (hir.get(&a.key), hir.get(&b.key)) else {
        return KeyMatch::Unknown;
    };
    match same_scalar(x, y) {
        true => KeyMatch::Same,
        false => KeyMatch::Different,
    }
}
