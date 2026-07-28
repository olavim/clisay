//! White-box tests for the `match` decision-tree builder (`codegen::matching::tree`).

use clisay::internals::{build_tree, Access, Clause, DecisionTree, HirMatcher, Ir, Label, Path, Scalar, ValueTest};

fn clause(tests: Vec<(Path, ValueTest)>, body: Label) -> Clause<'static> {
    Clause { tests, nested: Vec::new(), binds: Vec::new(), guard: None, body, binders: &[] }
}

fn present(key: &str) -> ValueTest {
    ValueTest::Present(Scalar::Str(key.to_string()))
}

fn eq_num(n: u64) -> ValueTest {
    ValueTest::Equal(Scalar::Num(n))
}

fn field(key: &str) -> Path {
    vec![Access::Field(Scalar::Str(key.to_string()))]
}

/// Two arms testing the same key share one `Test` node; both land under its matched branch.
#[test]
fn shared_test_is_emitted_once() {
    let mut ir = Ir::new();
    let (b1, b2) = (ir.new_label(), ir.new_label());
    let clauses = vec![
        clause(vec![(vec![], present("a"))], b1),
        clause(vec![(vec![], present("a"))], b2),
    ];
    let DecisionTree::Test { test, matched, unmatched, .. } = build_tree(&clauses) else { panic!("expected a shared test") };
    assert!(test == present("a"));
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b1));
    assert!(matches!(*unmatched, DecisionTree::Fail));
}

/// A non-equality conflict (differing exact array lengths) chains as binary tests: the second only
/// lives in the first's unmatched branch. Only equality clusters become a switch.
#[test]
fn conflicting_tests_chain_through_unmatched() {
    let mut ir = Ir::new();
    let (b1, b2) = (ir.new_label(), ir.new_label());
    let len = |n| ValueTest::ArrayLen { min: n, exact: true };
    let clauses = vec![
        clause(vec![(vec![], len(1))], b1),
        clause(vec![(vec![], len(2))], b2),
    ];
    let DecisionTree::Test { test, matched, unmatched, .. } = build_tree(&clauses) else { panic!("expected a test") };
    assert!(test == len(1));
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b1));
    let DecisionTree::Test { test, matched, .. } = *unmatched else { panic!("expected the second test") };
    assert!(test == len(2));
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b2));
}

/// The heuristic branches on the shared test first: `Present("b")` (both clauses) outranks
/// `Present("a")` (one clause), so the clause that only tests `b` is not duplicated into both branches.
#[test]
fn heuristic_branches_on_the_shared_test() {
    let mut ir = Ir::new();
    let (b1, b2) = (ir.new_label(), ir.new_label());
    let clauses = vec![
        clause(vec![(vec![], present("a")), (vec![], present("b"))], b1),
        clause(vec![(vec![], present("b"))], b2),
    ];
    let DecisionTree::Test { test, matched, unmatched, .. } = build_tree(&clauses) else { panic!("expected a test") };
    assert!(test == present("b"));
    assert!(matches!(*unmatched, DecisionTree::Fail));
    let DecisionTree::Test { test, matched, unmatched, .. } = *matched else { panic!("expected a nested test") };
    assert!(test == present("a"));
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b1));
    assert!(matches!(*unmatched, DecisionTree::Leaf { body, .. } if body == b2));
}

/// A clause the chosen test neither satisfies nor conflicts with is duplicated into both branches, so
/// `b2` stays reachable whether or not `a` is present. Tied scores keep the top clause's first test.
#[test]
fn indifferent_clause_survives_both_branches() {
    let mut ir = Ir::new();
    let (b1, b2) = (ir.new_label(), ir.new_label());
    let clauses = vec![
        clause(vec![(vec![], present("a")), (field("x"), present("b"))], b1),
        clause(vec![(field("y"), present("c"))], b2),
    ];
    let DecisionTree::Test { test, matched, unmatched, .. } = build_tree(&clauses) else { panic!("expected a test") };
    assert!(test == present("a"));
    let DecisionTree::Test { test, matched, unmatched: shared, .. } = *matched else { panic!("expected the owner's second test") };
    assert!(test == present("b"));
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b1));

    let leads_to_b2 = |tree: DecisionTree| match tree {
        DecisionTree::Test { test, matched, .. } => {
            assert!(test == present("c"));
            matches!(*matched, DecisionTree::Leaf { body, .. } if body == b2)
        },
        _ => panic!("expected a `Present(c)` test"),
    };
    assert!(leads_to_b2(*shared));
    assert!(leads_to_b2(*unmatched));
}

/// A nested matcher runs only for its owner; clauses below it take both branches.
#[test]
fn nested_matcher_owner_takes_matched_rest_takes_both() {
    let mut ir = Ir::new();
    let (b1, b2) = (ir.new_label(), ir.new_label());
    let wildcard = HirMatcher::Wildcard;
    let owner = Clause { tests: Vec::new(), nested: vec![(vec![], &wildcard)], binds: Vec::new(), guard: None, body: b1, binders: &[] };
    let clauses = vec![owner, clause(Vec::new(), b2)];
    let DecisionTree::Nested { matched, unmatched, .. } = build_tree(&clauses) else { panic!("expected a nested node") };
    assert!(matches!(*matched, DecisionTree::Leaf { body, .. } if body == b1));
    assert!(matches!(*unmatched, DecisionTree::Leaf { body, .. } if body == b2));
}

/// Several equality tests on one path build a single `Switch`, cases in order, with the trailing
/// no-match as the default.
#[test]
fn equality_cluster_builds_a_switch() {
    let mut ir = Ir::new();
    let (b1, b2, b3) = (ir.new_label(), ir.new_label(), ir.new_label());
    let tag = field("a");
    let clauses = vec![
        clause(vec![(tag.clone(), eq_num(1))], b1),
        clause(vec![(tag.clone(), eq_num(2))], b2),
        clause(vec![(tag.clone(), eq_num(3))], b3),
    ];
    let DecisionTree::Switch { path, cases, default } = build_tree(&clauses) else { panic!("expected a switch") };
    assert!(path == tag);
    let bodies = [b1, b2, b3];
    assert_eq!(cases.len(), 3);
    for (i, (value, subtree)) in cases.iter().enumerate() {
        assert!(*value == Scalar::Num((i + 1) as u64));
        assert!(matches!(subtree, DecisionTree::Leaf { body, .. } if *body == bodies[i]));
    }
    assert!(matches!(*default, DecisionTree::Fail));
}

/// A single equality test is not worth a switch, so it stays a binary `Test`.
#[test]
fn single_equality_stays_a_test() {
    let mut ir = Ir::new();
    let b1 = ir.new_label();
    let clauses = vec![clause(vec![(field("a"), eq_num(1))], b1)];
    assert!(matches!(build_tree(&clauses), DecisionTree::Test { test: ValueTest::Equal(_), .. }));
}

/// A switch only pays off once its shared load is more than a bare scrutinee read, so an equality
/// cluster on the empty path chains binary `Test`s instead.
#[test]
fn root_equality_cluster_stays_a_chain() {
    let mut ir = Ir::new();
    let (b1, b2, b3) = (ir.new_label(), ir.new_label(), ir.new_label());
    let clauses = vec![
        clause(vec![(vec![], eq_num(1))], b1),
        clause(vec![(vec![], eq_num(2))], b2),
        clause(vec![(vec![], eq_num(3))], b3),
    ];
    let DecisionTree::Test { test, unmatched, .. } = build_tree(&clauses) else { panic!("expected a test") };
    assert!(test == eq_num(1));
    assert!(matches!(*unmatched, DecisionTree::Test { test: ValueTest::Equal(_), .. }));
}

/// The switch covers only the equality cluster on one path: a test at another path stays a binary
/// `Test` in the default.
#[test]
fn switch_leaves_other_paths_in_the_default() {
    let mut ir = Ir::new();
    let (b1, b2, b3) = (ir.new_label(), ir.new_label(), ir.new_label());
    let (tag, other) = (field("a"), field("b"));
    let clauses = vec![
        clause(vec![(tag.clone(), eq_num(1))], b1),
        clause(vec![(tag, eq_num(2))], b2),
        clause(vec![(other.clone(), eq_num(5))], b3),
    ];
    let DecisionTree::Switch { cases, default, .. } = build_tree(&clauses) else { panic!("expected a switch") };
    assert_eq!(cases.len(), 2);
    assert!(matches!(*default, DecisionTree::Test { ref path, test: ValueTest::Equal(_), .. } if *path == other));
}
