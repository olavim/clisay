#![feature(variant_count)]

//! Tests `merge_flow` on its own, not through a program that happens to reach it.
//!
//! Each field of `LocalFlow` gets a list of the values the merge can tell apart. Those lists are
//! crossed to make the test values.

use std::collections::HashMap;

use clisay::internals::{
    intersect_narrowings, merge_flow, symbol, ElementKey, LocalFlow, WriteOwnershipTransfer, TransferSite,
    Mutability, Obligations, Symbol, TypeTag, HirId,
};

// Two obligations the local owes, and one it does not. The merge drops anything from `handled` that
// is not owed, and `UNOWED` is here to test that.
const OWED_A: u32 = 1;
const OWED_B: u32 = 2;
const UNOWED: u32 = 3;

// Field names. These are the keys of `field_discharged`. They use different numbers from the
// obligations above, so the two are easy to tell apart.
const FIELD_A: u32 = 10;
const FIELD_B: u32 = 11;

// How many values `each_rule_is_decided_by_its_own_field_alone` takes from each side of a pair. It
// is the only law that walks the whole cross, and every pair of that would be far too many.
const SAMPLE: usize = 120;

// Node and slot numbers. The merge only asks whether two of them are equal, so the numbers
// themselves do not matter.
const TYPE_A: usize = 0;
const TYPE_B: usize = 1;
const TRANSFER_SITE_A: usize = 0;
const TRANSFER_SITE_B: usize = 1;
const CONTAINER_A: usize = 0;
const CONTAINER_B: usize = 1;
const CALLEE: usize = 2;

fn obligations(ids: &[u32]) -> Obligations {
    ids.iter().map(|&id| symbol(id)).collect()
}

/// What every local here owes. The merge filters `handled` against this.
fn owed() -> Obligations {
    obligations(&[OWED_A, OWED_B])
}

fn narrowings(entries: &[(u32, &[u32])]) -> HashMap<Symbol, Obligations> {
    entries.iter().map(|(field, obs)| (symbol(*field), obligations(obs))).collect()
}

// One list per field. The full cross and the smaller sets below both use these, so they cannot
// drift apart.
fn tags() -> Vec<TypeTag> {
    vec![
        TypeTag::Unknown,
        TypeTag::SelfType,
        TypeTag::Concrete(HirId::from_index(TYPE_A)),
        TypeTag::Concrete(HirId::from_index(TYPE_B)),
    ]
}

fn mutabilities() -> Vec<Mutability> {
    vec![Mutability::Mutable, Mutability::Immutable, Mutability::Unknown]
}

/// Both causes, because they mean different things. `check_moved` refuses a read after a `Value`
/// move but clears an `Opaque` one.
fn moves() -> Vec<Option<TransferSite>> {
    vec![
        None,
        Some(TransferSite { node: HirId::from_index(TRANSFER_SITE_A), transfer: WriteOwnershipTransfer::Transferred }),
        Some(TransferSite { node: HirId::from_index(TRANSFER_SITE_B), transfer: WriteOwnershipTransfer::Transferred }),
        Some(TransferSite { node: HirId::from_index(TRANSFER_SITE_A), transfer: WriteOwnershipTransfer::Unknown(HirId::from_index(CALLEE), 0) }),
    ]
}

/// Every `ElementKey` variant. If a rule ever reads the key, the values are already here, instead
/// of waiting for someone to remember to add them. The last entry names two origins, which is what
/// a join produces when the branches read out of different aggregates.
fn extractions() -> Vec<Vec<(usize, Option<ElementKey>)>> {
    vec![
        vec![],
        vec![(CONTAINER_A, None)],
        vec![(CONTAINER_B, Some(ElementKey::Null))],
        vec![(CONTAINER_A, Some(ElementKey::Number(1.0)))],
        vec![(CONTAINER_B, Some(ElementKey::Bool(true)))],
        vec![(CONTAINER_A, Some(ElementKey::Name(HirId::from_index(CALLEE))))],
        vec![(CONTAINER_A, None), (CONTAINER_B, Some(ElementKey::Null))],
    ]
}

fn sets() -> Vec<Obligations> {
    vec![
        obligations(&[]),
        obligations(&[OWED_A]),
        obligations(&[OWED_B]),
        obligations(&[OWED_A, OWED_B]),
        obligations(&[OWED_A, UNOWED]),
    ]
}

fn field_sets() -> Vec<HashMap<Symbol, Obligations>> {
    vec![
        narrowings(&[]),
        narrowings(&[(FIELD_A, &[OWED_A])]),
        narrowings(&[(FIELD_A, &[OWED_A, OWED_B]), (FIELD_B, &[OWED_B])]),
    ]
}

fn base() -> LocalFlow {
    LocalFlow {
        assigned: true,
        tag: tags()[0].clone(),
        mutability: mutabilities()[0],
        transfer_site: moves()[0],
        provenance: vec![0],
        extracted_from: extractions()[0].clone(),
        handled: sets()[0].clone(),
        discharged: sets()[0].clone(),
        field_discharged: field_sets()[0].clone(),
    }
}

/// The full cross. Only `each_rule_is_decided_by_its_own_field_alone` needs it, because that law is
/// about what happens when the other fields change. Every other law reads a few fields, and uses one
/// of the smaller sets below.
fn domain() -> Vec<LocalFlow> {
    let mut out = Vec::new();
    for assigned in [true, false] {
        for tag in &tags() {
            for mutability in &mutabilities() {
                for transfer_site in &moves() {
                    for extracted_from in &extractions() {
                        for handled in &sets() {
                            for discharged in &sets() {
                                for field_discharged in &field_sets() {
                                    out.push(LocalFlow {
                                        assigned,
                                        tag: tag.clone(),
                                        mutability: *mutability,
                                        transfer_site: *transfer_site,
                                        provenance: vec![0],
                                        extracted_from: extracted_from.clone(),
                                        handled: handled.clone(),
                                        discharged: discharged.clone(),
                                        field_discharged: field_discharged.clone(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    out
}

/// The two fields the merge reads together. `handled` is decided from `discharged` as well, so a
/// law about either one needs both.
fn coupled_domain() -> Vec<LocalFlow> {
    let mut out = Vec::new();
    for handled in &sets() {
        for discharged in &sets() {
            out.push(LocalFlow { handled: handled.clone(), discharged: discharged.clone(), ..base() });
        }
    }
    out
}

/// The base, plus every value that differs from it in one field. Crossing this covers each field
/// rule over its own values, which is all such a rule can read. That the rules really are per-field
/// is checked by `each_rule_is_decided_by_its_own_field_alone`.
fn one_field_apart() -> Vec<LocalFlow> {
    let mut out = vec![base()];
    out.push(LocalFlow { assigned: false, ..base() });
    out.extend(tags().into_iter().map(|tag| LocalFlow { tag, ..base() }));
    out.extend(mutabilities().into_iter().map(|mutability| LocalFlow { mutability, ..base() }));
    out.extend(moves().into_iter().map(|transfer_site| LocalFlow { transfer_site, ..base() }));
    out.extend(extractions().into_iter().map(|extracted_from| LocalFlow { extracted_from, ..base() }));
    out.extend(sets().into_iter().map(|handled| LocalFlow { handled, ..base() }));
    out.extend(sets().into_iter().map(|discharged| LocalFlow { discharged, ..base() }));
    out.extend(field_sets().into_iter().map(|field_discharged| LocalFlow { field_discharged, ..base() }));
    out
}

/// One field turned into a string, so a law can compare that field on its own.
type Key = fn(&LocalFlow) -> String;

/// Every field the merge decides from that field alone. `handled` is left out on purpose: it is the
/// one field decided from two, and `coupled_key` covers it.
const FIELD_KEYS: [(&str, Key); 8] = [
    ("assigned", |f| format!("{}", f.assigned)),
    ("tag", |f| match &f.tag { TypeTag::Concrete(id) => format!("c{}", id.index()), _ => "other".into() }),
    ("mutability", |f| format!("{}{}", f.mutability == Mutability::Mutable, f.mutability == Mutability::Immutable)),
    ("transfer_site", |f| f.transfer_site.map_or("none".into(), |m| match m.transfer {
        WriteOwnershipTransfer::Transferred => format!("value@{}", m.node.index()),
        WriteOwnershipTransfer::Unknown(callee, pos) => format!("opaque@{}:{}:{}", m.node.index(), callee.index(), pos),
    })),
    ("extracted_from", |f| {
        let mut origins: Vec<String> = f.extracted_from.iter().map(origin_key).collect();
        origins.sort();
        origins.join("+")
    }),
    ("provenance", |f| format!("{:?}", f.provenance)),
    ("discharged", |f| set_key(&f.discharged)),
    ("field_discharged", |f| {
        let mut entries: Vec<String> = f.field_discharged.iter()
            .map(|(field, obs)| format!("{}:{}", field.index(), set_key(obs)))
            .collect();
        entries.sort();
        entries.join("|")
    }),
];

/// One origin as a string, so a set of them has an order that does not depend on how it was built.
fn origin_key(origin: &(usize, Option<ElementKey>)) -> String {
    let key = match origin.1 {
        None => "any".to_string(),
        Some(ElementKey::Null) => "null".to_string(),
        Some(ElementKey::Bool(b)) => format!("bool{b}"),
        Some(ElementKey::Number(n)) => format!("num{n}"),
        Some(ElementKey::Name(id)) => format!("name{}", id.index()),
    };
    format!("c{}:{key}", origin.0)
}

fn set_key(set: &Obligations) -> String {
    set.iter().map(|s| s.index().to_string()).collect::<Vec<_>>().join(",")
}

/// The two fields together. `handled` is filtered on whether each side resolved, and working that
/// out reads `discharged` too.
fn coupled_key(flow: &LocalFlow) -> String {
    format!("{}/{}", set_key(&flow.handled), set_key(&flow.discharged))
}

fn merged(a: &LocalFlow, b: &LocalFlow) -> LocalFlow {
    let mut out = a.clone();
    merge_flow(&mut out, &owed(), b);
    out
}

/// What the local has settled: an obligation it handled, or that a path proved it's not a witness.
fn resolved(flow: &LocalFlow) -> Vec<Symbol> {
    owed().iter().copied()
        .filter(|ob| flow.handled.contains(ob) || flow.discharged.contains(ob))
        .collect()
}

#[test]
fn merging_an_outcome_with_itself_settles_nothing_new() {
    // Not the same value back. A join moves what both sides discharged into `handled`, and drops
    // anything the local does not owe. So it settles after one merge, not zero.
    for a in domain() {
        let once = merged(&a, &a);
        assert!(resolved(&once) == resolved(&a), "merging an outcome into itself changed what it settled");
        assert!(merged(&once, &a) == once, "merging an outcome into itself is not a fixed point");
        // Only the obligation fields may move, and only because of that promotion. Every other field
        // has to come back untouched, whatever it holds.
        let mut untouched = a.clone();
        untouched.handled = once.handled.clone();
        untouched.discharged = once.discharged.clone();
        assert!(once == untouched, "merging an outcome into itself changed a field the join should leave alone");
    }
}

#[test]
fn a_merge_resolves_only_what_both_outcomes_resolved() {
    // `resolved` reads `handled` and `discharged` and nothing else. So the coupled set is the whole
    // space this law has, and every pair of it fits.
    let domain = coupled_domain();
    for a in domain.iter() {
        for b in domain.iter() {
            let out = merged(a, b);
            for ob in resolved(&out) {
                assert!(resolved(a).contains(&ob) && resolved(b).contains(&ob),
                    "the join resolved an obligation an outcome had not");
            }
        }
    }
}

#[test]
fn merging_is_commutative() {
    // All fields except `extracted_from`, which still merges with `or` and keeps the left one when
    // both sides are set.
    let mut domain = one_field_apart();
    domain.extend(coupled_domain());
    for a in domain.iter() {
        for b in domain.iter() {
            let (mut ab, mut ba) = (merged(a, b), merged(b, a));
            // The union is built by pushing, so the two orders hold the same origins in a different
            // order. The set is what the rule means, so compare it as one.
            ab.extracted_from.sort_by_key(origin_key);
            ba.extracted_from.sort_by_key(origin_key);
            assert!(ab == ba, "the join depends on the order of its outcomes");
        }
    }
}


#[test]
fn merging_is_associative() {
    let domain = coupled_domain();
    for a in &domain {
        for b in &domain {
            for c in &domain {
                let left = merged(&merged(a, b), c);
                let right = merged(a, &merged(b, c));
                assert!(left == right, "the join depends on how its outcomes are grouped");
            }
        }
    }
}

#[test]
fn folding_another_outcome_never_resolves_more() {
    let domain = coupled_domain();
    for a in &domain {
        for b in &domain {
            for c in &domain {
                let two = merged(a, b);
                let three = merged(&two, c);
                for ob in resolved(&three) {
                    assert!(resolved(&two).contains(&ob),
                        "a third outcome resolved an obligation two had not");
                }
                assert!(three.provenance.len() <= two.provenance.len(), "a fold grew provenance");
                assert!(three.field_discharged.len() <= two.field_discharged.len(),
                    "a fold grew the field narrowings");
            }
        }
    }
}

#[test]
fn a_restriction_survives_a_merge_from_either_side() {
    // Reads `transfer_site` and `extracted_from`. Both are per-field rules, so one field apart is enough.
    let domain = one_field_apart();
    for a in domain.iter() {
        for b in domain.iter() {
            let out = merged(a, b);
            assert!(out.transfer_site.is_some() == (a.transfer_site.is_some() || b.transfer_site.is_some()),
                "a move was lost or invented by the join");
            for origin in a.extracted_from.iter().chain(&b.extracted_from) {
                assert!(out.extracted_from.contains(origin), "an origin was lost by the join");
            }
            for origin in &out.extracted_from {
                assert!(a.extracted_from.contains(origin) || b.extracted_from.contains(origin),
                    "an origin was invented by the join");
            }
        }
    }
}

#[test]
fn field_narrowings_intersect() {
    let sets = [
        narrowings(&[]),
        narrowings(&[(3, &[1])]),
        narrowings(&[(3, &[1, 2])]),
        narrowings(&[(3, &[2]), (4, &[1])]),
        narrowings(&[(4, &[1, 2])]),
    ];
    for a in &sets {
        for b in &sets {
            let (mut left, mut right) = (a.clone(), b.clone());
            intersect_narrowings(&mut left, b);
            intersect_narrowings(&mut right, a);
            assert!(left == right, "the field narrowing merge depends on the order of its sides");

            for (field, obligations) in &left {
                for ob in obligations.iter() {
                    assert!(a.get(field).is_some_and(|s| s.contains(ob)),
                        "a field narrowing survived that one side never proved");
                    assert!(b.get(field).is_some_and(|s| s.contains(ob)),
                        "a field narrowing survived that one side never proved");
                }
            }

            let mut idempotent = a.clone();
            intersect_narrowings(&mut idempotent, a);
            assert!(idempotent == *a, "intersecting a set of narrowings with itself changed it");
        }
    }
}

#[test]
fn the_sample_covers_every_value() {
    // A sample that missed a value would make the law above stop testing that value without saying
    // so. This catches that, and it fails if `SAMPLE` is set too low to reach every one.
    let full = domain();
    let distinct = |flows: &[&LocalFlow], key: Key| -> usize {
        flows.iter().map(|f| key(f)).collect::<std::collections::HashSet<_>>().len()
    };

    let everything: Vec<&LocalFlow> = full.iter().collect();
    let sampled = spread(&full, SAMPLE);
    for (name, key) in FIELD_KEYS {
        assert!(distinct(&sampled, key) == distinct(&everything, key),
            "the sample of {SAMPLE} misses some `{name}` values that the cross has");
    }
}

#[test]
fn each_rule_is_decided_by_its_own_field_alone() {
    // The laws over three values cross only `handled` and `discharged`, because every other rule
    // reads its own field and nothing else. That is an assumption about `merge_flow`, and this turns
    // it into a check. A rule that starts reading a second field fails here, and names the field.
    let full = domain();
    let sample = spread(&full, SAMPLE);
    let pairs: Vec<(&LocalFlow, &LocalFlow)> = sample.iter()
        .flat_map(|a| sample.iter().map(move |b| (*a, *b)))
        .collect();

    for (name, key) in FIELD_KEYS {
        let mut seen: HashMap<(String, String), String> = HashMap::new();
        for (a, b) in &pairs {
            let inputs = (key(a), key(b));
            let result = key(&merged(a, b));
            if let Some(previous) = seen.insert(inputs, result.clone()) {
                assert!(previous == result,
                    "`{name}` is no longer decided by `{name}` alone: the same inputs merged to \
                     {previous:?} and to {result:?}, so some other field now feeds its rule");
            }
        }
    }

    // And the one coupled field is coupled to one other field, not to a third.
    let mut seen: HashMap<(String, String), String> = HashMap::new();
    for (a, b) in &pairs {
        let inputs = (coupled_key(a), coupled_key(b));
        let result = set_key(&merged(a, b).handled);
        if let Some(previous) = seen.insert(inputs, result.clone()) {
            assert!(previous == result,
                "`handled` is decided by more than `handled` and `discharged`: the same inputs \
                 merged to {previous:?} and to {result:?}");
        }
    }
}


/// Every variant of every enum in the domain has to appear in it.
#[test]
fn the_domain_carries_every_variant() {
    use std::mem::{discriminant, variant_count, Discriminant};

    let full = domain();
    fn count<T>(seen: std::collections::HashSet<Discriminant<T>>) -> usize { seen.len() }

    let kinds = count(full.iter().filter_map(|f| f.transfer_site).map(|m| discriminant(&m.transfer)).collect());
    assert!(kinds == variant_count::<WriteOwnershipTransfer>(),
        "the domain carries {kinds} of {} `WriteOwnershipTransfer` variants", variant_count::<WriteOwnershipTransfer>());

    let keys = count(full.iter().flat_map(|f| &f.extracted_from).filter_map(|(_, k)| k.as_ref())
        .map(discriminant).collect());
    assert!(keys == variant_count::<ElementKey>(),
        "the domain carries {keys} of {} `ElementKey` variants", variant_count::<ElementKey>());

    let tags = count(full.iter().map(|f| discriminant(&f.tag)).collect());
    assert!(tags == variant_count::<TypeTag>(),
        "the domain carries {tags} of {} `TypeTag` variants", variant_count::<TypeTag>());

    let mutabilities = count(full.iter().map(|f| discriminant(&f.mutability)).collect());
    assert!(mutabilities == variant_count::<Mutability>(),
        "the domain carries {mutabilities} of {} `Mutability` variants", variant_count::<Mutability>());
}

/// Takes `count` values spread across `all`. The step starts at the even spacing, `len / count`,
/// so the picks cover the whole range, and it is nudged up until it shares no factor with the
/// length, so it cannot line up with the loops in `domain` and hold a field at one value.
fn spread(all: &[LocalFlow], count: usize) -> Vec<&LocalFlow> {
    let len = all.len();
    let mut step = (len / count.max(1)).max(1);
    while gcd(step, len) != 1 {
        step += 1;
    }
    (0..count.min(len)).map(|i| &all[(i * step) % len]).collect()
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 { a } else { gcd(b, a % b) }
}

#[test]
fn a_merge_reports_assigned_only_when_both_outcomes_did() {
    // A binding assigned on one path only is not assigned after the join.
    let domain = one_field_apart();
    for a in domain.iter() {
        for b in domain.iter() {
            assert!(merged(a, b).assigned == (a.assigned && b.assigned),
                "the join called a binding assigned that an outcome had not assigned");
        }
    }
}
