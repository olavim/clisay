#![feature(variant_count)]

//! Tests `merge_flow` on its own, not through a program that happens to reach it.

use std::collections::HashMap;

use clisay::internals::{
    intersect_narrowings, merge_local_flow, symbol, LocalFlow,
    CallableId, Obligations, Symbol, TypeTag, HirId,
};

// Two obligations the local owes, and one it does not.
const OWED_A: u32 = 1;
const OWED_B: u32 = 2;
const UNOWED: u32 = 3;

// Field names.
const FIELD_A: u32 = 10;
const FIELD_B: u32 = 11;

const SAMPLE: usize = 120;

// Node and slot numbers.
const TYPE_A: usize = 0;
const TYPE_B: usize = 1;

fn obligations(ids: &[u32]) -> Obligations {
    ids.iter().map(|&id| symbol(id)).collect()
}

fn owed() -> Obligations {
    obligations(&[OWED_A, OWED_B])
}

fn narrowings(entries: &[(u32, &[u32])]) -> HashMap<Symbol, Obligations> {
    entries.iter().map(|(field, obs)| (symbol(*field), obligations(obs))).collect()
}

fn tags() -> Vec<TypeTag> {
    vec![
        TypeTag::Unknown,
        TypeTag::SelfType,
        TypeTag::Concrete(HirId::from_index(TYPE_A)),
        TypeTag::Concrete(HirId::from_index(TYPE_B)),
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

/// The callables a name can be found to reach, plus reaching none.
fn resolutions() -> Vec<Option<CallableId>> {
    vec![None, Some(CallableId::Fn(HirId::from_index(TYPE_A))), Some(CallableId::Lambda(HirId::from_index(TYPE_B)))]
}

fn base() -> LocalFlow {
    LocalFlow {
        assigned: true,
        tag: tags()[0].clone(),
        discharged: sets()[0].clone(),
        field_discharged: field_sets()[0].clone(),
        resolved_callable: resolutions()[0],
    }
}

fn domain() -> Vec<LocalFlow> {
    let mut out = Vec::new();
    for assigned in [true, false] {
        for tag in &tags() {
            for discharged in &sets() {
                for field_discharged in &field_sets() {
                    for resolves_to in &resolutions() {
                        out.push(LocalFlow {
                            assigned,
                            tag: tag.clone(),
                            discharged: discharged.clone(),
                            field_discharged: field_discharged.clone(),
                            resolved_callable: *resolves_to,
                        });
                    }
                }
            }
        }
    }
    out
}

fn discharge_domain() -> Vec<LocalFlow> {
    sets().into_iter().map(|discharged| LocalFlow { discharged, ..base() }).collect()
}

fn one_field_apart() -> Vec<LocalFlow> {
    let mut out = vec![base()];
    out.push(LocalFlow { assigned: false, ..base() });
    out.extend(tags().into_iter().map(|tag| LocalFlow { tag, ..base() }));
    out.extend(sets().into_iter().map(|discharged| LocalFlow { discharged, ..base() }));
    out.extend(field_sets().into_iter().map(|field_discharged| LocalFlow { field_discharged, ..base() }));
    out
}

/// One field turned into a string.
type Key = fn(&LocalFlow) -> String;

const FIELD_KEYS: [(&str, Key); 4] = [
    ("assigned", |f| format!("{}", f.assigned)),
    ("tag", |f| match &f.tag { TypeTag::Concrete(id) => format!("c{}", id.index()), _ => "other".into() }),
    ("discharged", |f| set_key(&f.discharged)),
    ("field_discharged", |f| {
        let mut entries: Vec<String> = f.field_discharged.iter()
            .map(|(field, obs)| format!("{}:{}", field.index(), set_key(obs)))
            .collect();
        entries.sort();
        entries.join("|")
    }),
];

fn set_key(set: &Obligations) -> String {
    set.iter().map(|s| s.index().to_string()).collect::<Vec<_>>().join(",")
}

fn merged(a: &LocalFlow, b: &LocalFlow) -> LocalFlow {
    let mut out = a.clone();
    merge_local_flow(&mut out, b);
    out
}

fn resolved(flow: &LocalFlow) -> Vec<Symbol> {
    owed().iter().copied().filter(|ob| flow.discharged.contains(ob)).collect()
}

#[test]
fn merging_an_outcome_with_itself_settles_nothing_new() {
    for a in domain() {
        let once = merged(&a, &a);
        assert!(resolved(&once) == resolved(&a), "merging an outcome into itself changed what it settled");
        assert!(merged(&once, &a) == once, "merging an outcome into itself is not a fixed point");
        let mut untouched = a.clone();
        untouched.discharged = once.discharged.clone();
        assert!(once == untouched, "merging an outcome into itself changed a field the join should leave alone");
    }
}

#[test]
fn a_merge_resolves_only_what_both_outcomes_resolved() {
    let domain = discharge_domain();
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
    let mut domain = one_field_apart();
    domain.extend(discharge_domain());
    for a in domain.iter() {
        for b in domain.iter() {
            let (ab, ba) = (merged(a, b), merged(b, a));
            assert!(ab == ba, "the join depends on the order of its outcomes");
        }
    }
}


#[test]
fn merging_is_associative() {
    let domain = discharge_domain();
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
    let domain = discharge_domain();
    for a in &domain {
        for b in &domain {
            for c in &domain {
                let two = merged(a, b);
                let three = merged(&two, c);
                for ob in resolved(&three) {
                    assert!(resolved(&two).contains(&ob),
                        "a third outcome resolved an obligation two had not");
                }
                assert!(three.field_discharged.len() <= two.field_discharged.len(),
                    "a fold grew the field narrowings");
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
}

#[test]
fn the_domain_carries_every_variant() {
    use std::mem::{discriminant, variant_count, Discriminant};

    let full = domain();
    fn count<T>(seen: std::collections::HashSet<Discriminant<T>>) -> usize { seen.len() }

    let tags = count(full.iter().map(|f| discriminant(&f.tag)).collect());
    assert!(tags == variant_count::<TypeTag>(),
        "the domain carries {tags} of {} `TypeTag` variants", variant_count::<TypeTag>());
}

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
    let domain = one_field_apart();
    for a in domain.iter() {
        for b in domain.iter() {
            assert!(merged(a, b).assigned == (a.assigned && b.assigned),
                "the join called a binding assigned that an outcome had not assigned");
        }
    }
}
