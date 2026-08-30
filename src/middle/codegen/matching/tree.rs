//! Decision-tree compilation of a `match` statement. Each arm lowers to one or more clauses, a
//! clause is a conjunction of tests, binds, and nested matchers, and the clauses compile to a tree
//! that shares a test across every arm that needs it rather than re-evaluating the scrutinee.

use std::cmp::Reverse;

use crate::core::value::Value;
use crate::middle::hir::{Hir, HirExpr, HirId, HirMatchArm, HirMatchElem, HirMatcher, HirStmt, Symbol, TypeId};
use crate::middle::ir::{Inst, Label};

use super::{slot_of, split_at_rest, Compiler, Scalar};

/// One hop from a value into a nested value.
#[derive(Clone, PartialEq)]
pub enum Access {
    Field(Scalar),
    ArrayFront(usize),
    ArrayBack(usize),
    ArrayMiddle(usize, usize),
}

/// How to reach a nested value from the value being matched.
pub type Path = Vec<Access>;

/// One discriminating test on the value at a path.
#[derive(Clone, PartialEq)]
pub enum ValueTest {
    Present(Scalar),
    /// A declared member, which is present only if it also admits what the declaration allows.
    Admits { key: Scalar, null_allowed: bool, witnesses: Vec<TypeId> },
    Equal(Scalar),
    Nominal(TypeId),
    ArrayLen { min: usize, exact: bool },
    /// The value is a dict or instance, the only kinds a shape matches.
    Shaped,
}

/// A single step in matching one clause: test a path, bind a path, or run a nested matcher at a path.
#[derive(Clone)]
enum MatchStep {
    Test(Path, ValueTest),
    Bind(Path, u8),
    /// The decision tree reloads a value's whole path from the scrutinee per step, so a nested pattern
    /// would reload its entire path each time the nested pattern's sub-values are read. A nested matcher
    /// loads the path once, so that such sub-values can be matched efficiently.
    Nested(Path, HirId<HirMatcher>),
}

/// One alternative's steps: an AND that must all hold for the alternative to match.
type Steps = Vec<MatchStep>;

/// A matcher's alternatives: an OR across step lists. Only an or-matcher yields more than one.
type Alternatives = Vec<Steps>;

/// One clause of the compiled match: a conjunction that, when its tests and nested matchers hold and
/// its guard passes, binds and runs `body`. An or-matcher expands one arm into several clauses that
/// share a body label. The steps are split by kind so tree building selects a discriminator without
/// scanning past binds: a `test` or `nested` drives a branch, and the `binds` are the leaf's payload.
#[derive(Clone)]
pub struct Clause<'a> {
    pub tests: Vec<(Path, ValueTest)>,
    pub nested: Vec<(Path, HirId<HirMatcher>)>,
    pub binds: Vec<(Path, u8)>,
    pub guard: Option<HirId<HirExpr>>,
    pub body: Label,
    /// The arm's binder name-to-slot, for compiling a nested matcher's binders.
    pub binders: &'a [(Symbol, u8)],
}

impl<'a> Clause<'a> {
    /// Sorts one alternative's flat steps into a clause by step kind.
    fn from_steps(steps: Steps, guard: Option<HirId<HirExpr>>, body: Label, binders: &'a [(Symbol, u8)]) -> Clause<'a> {
        let mut tests = Vec::new();
        let mut nested = Vec::new();
        let mut binds = Vec::new();
        for step in steps {
            match step {
                MatchStep::Test(path, test) => tests.push((path, test)),
                MatchStep::Nested(path, matcher) => nested.push((path, matcher)),
                MatchStep::Bind(path, slot) => binds.push((path, slot)),
            }
        }
        Clause { tests, nested, binds, guard, body, binders }
    }

    /// A copy with the `i`th test discharged, for the branch where that test held.
    fn without_test(&self, i: usize) -> Clause<'a> {
        let mut c = self.clone();
        c.tests.remove(i);
        c
    }

    /// A copy with the `i`th nested matcher discharged, for the branch where it matched.
    fn without_nested(&self, i: usize) -> Clause<'a> {
        let mut c = self.clone();
        c.nested.remove(i);
        c
    }
}

/// A match statement's arms compiled into an efficient tree of tests. A typical match statement
/// has a few shared tests that partition the arms, so the tree avoids re-evaluating the scrutinee
/// or repeating tests.
///
/// As an example, the match statement
///
/// ```text
/// match scrutinee {
///     { a, b, c } => <body1>,
///     { a, b } => <body2>,
///     { a } => <body3>,
/// }
/// ```
///
/// would compile to a tree that tests for the presence of `a`, then `b`, then `c`, with each test
/// branching to the next test or to a body. In other words, if we notice that `a` is present when
/// evaluating the first arm, we don't need to re-test for `a` when evaluating the second or third
/// arms. Otherwise, if we notice that `a` is absent, we can immediately jump to the end without
/// evaluating the remaining arms that test for `a`.
pub enum DecisionTree<'a> {
    /// No arm matched.
    Fail,
    /// A matched clause.
    Leaf { binds: Vec<(Path, u8)>, guard: Option<HirId<HirExpr>>, body: Label, otherwise: Box<DecisionTree<'a>> },
    /// A shared value test at a path.
    Test { path: Path, test: ValueTest, matched: Box<DecisionTree<'a>>, unmatched: Box<DecisionTree<'a>> },
    /// A run of equality tests against one path, dispatched from a single load of that path.
    Switch { path: Path, cases: Vec<(Scalar, DecisionTree<'a>)>, default: Box<DecisionTree<'a>> },
    /// A clause-unique nested matcher at a path.
    Nested { path: Path, matcher: HirId<HirMatcher>, binders: &'a [(Symbol, u8)], matched: Box<DecisionTree<'a>>, unmatched: Box<DecisionTree<'a>> },
}

impl<'a> Compiler<'a> {
    /// Compiles a `match` statement into a decision tree. The scrutinee is evaluated once into a
    /// temp. Each shared test is emitted once and partitions the live arms by its outcome.
    pub(in crate::middle::codegen) fn compile_match(&mut self, scrutinee: &HirId<HirExpr>, arms: &'a [HirMatchArm], stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let info = self.bindings.match_info(stmt_id);
        let scrut_slot = info.scrut_slot;
        let binder_slots = info.binder_slots;

        // The scrutinee value occupies its temp slot.
        self.expression(scrutinee)?;

        // The binder block is allocated once, sized to the widest arm.
        for _ in 0..binder_slots {
            self.emit(Inst::PushNull, stmt_id);
        }

        let mut body_labels = Vec::with_capacity(arms.len());
        for _ in arms {
            body_labels.push(self.ir.new_label());
        }

        let mut clauses = Vec::new();
        for (i, arm) in arms.iter().enumerate() {
            let binders = info.arm_binders[i].as_slice();
            let alts = self.lower_matcher(&arm.matcher, &[], binders, stmt_id)?;
            for steps in alts {
                clauses.push(Clause::from_steps(steps, arm.guard, body_labels[i], binders));
            }
        }

        let end = self.ir.new_label();
        let tree = build_tree(&clauses);
        self.emit_tree(&tree, scrut_slot, scrutinee, end, stmt_id)?;

        for (i, arm) in arms.iter().enumerate() {
            self.ir.bind(body_labels[i]);
            self.expression_stmt(&arm.body)?;
            self.emit(Inst::Jump(end), stmt_id);
        }

        self.ir.bind(end);
        self.exit_scope(stmt_id)?;
        Ok(())
    }

    /// Walks the built tree and emits its IR. Every path ends with a jump to a body or to the
    /// no-match end, so control never falls between subtrees.
    fn emit_tree(&mut self, tree: &DecisionTree<'a>, scrut_slot: u8, scrut_expr: &HirId<HirExpr>, end: Label, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match tree {
            DecisionTree::Fail => self.emit(Inst::Jump(end), node),
            DecisionTree::Leaf { binds, guard, body, otherwise } => {
                for (path, slot) in binds {
                    self.load_path(scrut_slot, path, node)?;
                    self.emit(Inst::StoreLocalPop(*slot), node);
                }
                match guard {
                    None => self.emit(Inst::Jump(*body), node),
                    Some(guard) => {
                        self.expression(guard)?;
                        let guard_fail = self.ir.new_label();
                        self.emit(Inst::JumpIfFalse(guard_fail), node);
                        self.emit(Inst::Jump(*body), node);
                        self.ir.bind(guard_fail);
                        self.emit_tree(otherwise, scrut_slot, scrut_expr, end, node)?;
                    },
                }
            },
            DecisionTree::Test { path, test, matched, unmatched } => {
                self.load_path(scrut_slot, path, node)?;
                self.emit_test(test, node)?;
                let unmatched_lbl = self.ir.new_label();
                self.emit(Inst::JumpIfFalse(unmatched_lbl), node);
                self.emit_tree(matched, scrut_slot, scrut_expr, end, node)?;
                self.ir.bind(unmatched_lbl);
                self.emit_tree(unmatched, scrut_slot, scrut_expr, end, node)?;
            },
            DecisionTree::Switch { path, cases, default } => {
                // Load the switched value once. Every case but the last tests a duplicate so the value
                // survives to the next comparison, and drops the survivor when it matches. The last
                // case consumes the value itself, so nothing is left for the default to clean up.
                self.load_path(scrut_slot, path, node)?;
                for (i, (value, subtree)) in cases.iter().enumerate() {
                    let survives = i + 1 < cases.len();
                    let next = self.ir.new_label();
                    if survives {
                        self.emit(Inst::Dup, node);
                    }
                    self.emit_equal(value, node)?;
                    self.emit(Inst::JumpIfFalse(next), node);
                    if survives {
                        self.emit(Inst::Pop, node);
                    }
                    self.emit_tree(subtree, scrut_slot, scrut_expr, end, node)?;
                    self.ir.bind(next);
                }
                self.emit_tree(default, scrut_slot, scrut_expr, end, node)?;
            },
            DecisionTree::Nested { path, matcher, binders, matched, unmatched } => {
                self.load_path(scrut_slot, path, node)?;
                self.compile_binding_matcher(matcher, binders, scrut_expr)?;
                let unmatched_lbl = self.ir.new_label();
                self.emit(Inst::JumpIfFalse(unmatched_lbl), node);
                self.emit_tree(matched, scrut_slot, scrut_expr, end, node)?;
                self.ir.bind(unmatched_lbl);
                self.emit_tree(unmatched, scrut_slot, scrut_expr, end, node)?;
            },
        }
        Ok(())
    }

    /// Pushes the value at `path`: loads the scrutinee temp, then applies each access in turn.
    fn load_path(&mut self, scrut_slot: u8, path: &[Access], node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        self.emit(Inst::LoadLocal(scrut_slot), node);
        for access in path {
            match access {
                Access::Field(key) => {
                    let idx = self.scalar_constant(key)?;
                    self.emit(Inst::GetIndexOrNull(idx), node);
                },
                Access::ArrayFront(i) => self.emit(Inst::ArrayElem(*i as u8, 0), node),
                Access::ArrayBack(j) => self.emit(Inst::ArrayElem(*j as u8, 1), node),
                Access::ArrayMiddle(prefix, suffix) => {
                    self.emit(Inst::ArrayMiddle(*prefix as u8, *suffix as u8), node);
                },
            }
        }
        Ok(())
    }

    /// Tests the value on top of the stack against `test`, consuming it and pushing a boolean.
    fn emit_test(&mut self, test: &ValueTest, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match test {
            ValueTest::Present(key) => {
                let idx = self.scalar_constant(key)?;
                self.emit(Inst::HasMember(idx), node);
            },
            ValueTest::Admits { key, null_allowed, witnesses } => {
                let idx = self.scalar_constant(key)?;
                let allow = self.accepted_witness_set(witnesses, *null_allowed);
                let allow_idx = self.ir.add_witness_allow(allow)?;
                self.emit(Inst::MemberAdmits(idx, allow_idx), node);
            },
            ValueTest::Equal(lit) => self.emit_equal(lit, node)?,
            ValueTest::Nominal(id) => self.emit(Inst::Is(*id), node),
            ValueTest::ArrayLen { min, exact } => {
                self.emit(Inst::ArrayLen, node);
                let idx = self.ir.add_constant(Value::from(*min as f64))?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(if *exact { Inst::Equal } else { Inst::GreaterThanEqual }, node);
            },
            ValueTest::Shaped => self.emit(Inst::IsShaped, node),
        }
        Ok(())
    }

    /// Compares the value on top of the stack against a scalar, consuming it and pushing a boolean.
    fn emit_equal(&mut self, value: &Scalar, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let idx = self.scalar_constant(value)?;
        self.emit(Inst::PushConstant(idx), node);
        self.emit(Inst::Equal, node);
        Ok(())
    }

    /// Lowers a matcher into its alternatives at `path`. Each alternative is an AND of steps, and
    /// the alternatives are an OR. Only an or-matcher yields more than one alternative.
    fn lower_matcher(&self, matcher: &HirId<HirMatcher>, path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives, anyhow::Error> {
        Ok(match self.hir.get(matcher) {
            HirMatcher::Wildcard => vec![vec![]],
            HirMatcher::Literal(lit) => vec![vec![MatchStep::Test(path.to_vec(), ValueTest::Equal(lit.into()))]],
            HirMatcher::Binder(name) => vec![vec![MatchStep::Bind(path.to_vec(), slot_of(binders, *name))]],
            HirMatcher::As(name, inner) => {
                let mut alts = self.lower_matcher(inner, path, binders, node)?;
                let bind = MatchStep::Bind(path.to_vec(), slot_of(binders, *name));
                prepend_step(&[bind], &mut alts);
                alts
            },
            HirMatcher::Type { nominal, name, shape } => self.lower_type(matcher, *nominal, *name, shape, path, binders, node)?,
            HirMatcher::Shape(fields) if fields.is_empty() => vec![vec![MatchStep::Test(path.to_vec(), ValueTest::Shaped)]],
            HirMatcher::Shape(fields) => {
                let mut groups = Vec::with_capacity(fields.len());
                for field in fields {
                    let key = Scalar::from(&field.key);
                    let mut field_path = path.to_vec();
                    field_path.push(Access::Field(key.clone()));
                    let mut field_alts = self.lower_value(&field.value, field_path, binders, node)?;

                    // A value test that rejects null already fails on an absent key.
                    if !self.hir.get(&field.value).rejects_null(self.hir) {
                        prepend_step(&[MatchStep::Test(path.to_vec(), ValueTest::Present(key))], &mut field_alts);
                    }

                    groups.push(field_alts);
                }

                cartesian_product(groups)
            },
            HirMatcher::Array(elements) => self.lower_array(elements, path, binders, node)?,
            HirMatcher::And(parts) => {
                let mut groups = Vec::with_capacity(parts.len());
                for part in parts {
                    groups.push(self.lower_matcher(part, path, binders, node)?);
                }
                cartesian_product(groups)
            },
            HirMatcher::Or(parts) => {
                let mut alts = Vec::new();
                for part in parts {
                    alts.extend(self.lower_matcher(part, path, binders, node)?);
                }
                alts
            },
        })
    }

    /// Lowers a shape field or array element value at `path`. A value that reads itself more than
    /// once becomes one `Nested` step so its path is loaded once. A simple value is lowered in
    /// place, keeping its single test shareable across arms.
    fn lower_value(&self, matcher: &HirId<HirMatcher>, path: Vec<Access>, binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives, anyhow::Error> {
        if needs_nested_matcher(self.hir, matcher) {
            Ok(vec![vec![MatchStep::Nested(path, *matcher)]])
        } else {
            self.lower_matcher(matcher, &path, binders, node)
        }
    }

    /// Lowers a type matcher: the type test, then the optional shape that destructures it further.
    /// A nominal type is one `is` test. A structural one tests each surface member's presence.
    fn lower_type(&self, matcher: &HirId<HirMatcher>, nominal: bool, name: Symbol, shape: &Option<HirId<HirMatcher>>, path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives, anyhow::Error> {
        let base = if nominal {
            vec![MatchStep::Test(path.to_vec(), ValueTest::Nominal(self.type_test_id(matcher, node)?))]
        } else {
            let members = self.surface_members(matcher, name, node)?;
            match members.is_empty() {
                true => vec![MatchStep::Test(path.to_vec(), ValueTest::Shaped)],
                false => members.into_iter()
                    .map(|(member, admits)| {
                        let key = Scalar::Str(self.hir.text(member).to_string());
                        let test = match admits {
                            Some((null_allowed, witnesses)) => ValueTest::Admits { key, null_allowed, witnesses },
                            None => ValueTest::Present(key),
                        };
                        MatchStep::Test(path.to_vec(), test)
                    })
                    .collect(),
            }
        };

        let mut alts = match shape {
            None => vec![vec![]],
            Some(shape) => self.lower_matcher(shape, path, binders, node)?,
        };
        prepend_step(&base, &mut alts);
        Ok(alts)
    }

    fn lower_array(&self, elements: &'a [HirMatchElem], path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives, anyhow::Error> {
        let (prefix, rest, suffix) = split_at_rest(elements, self.hir.pos(node))?;

        // The length is one test among the clause's, and the tree decides when it runs.
        let mut groups = vec![vec![vec![MatchStep::Test(path.to_vec(), ValueTest::ArrayLen { min: prefix.len() + suffix.len(), exact: rest.is_none() })]]];
        for (i, elem) in prefix.iter().enumerate() {
            if let HirMatchElem::Elem(matcher) = elem {
                let mut elem_path = path.to_vec();
                elem_path.push(Access::ArrayFront(i));
                groups.push(self.lower_value(matcher, elem_path, binders, node)?);
            }
        }
        if let Some(HirMatchElem::Rest(Some(name))) = rest {
            let mut rest_path = path.to_vec();
            rest_path.push(Access::ArrayMiddle(prefix.len(), suffix.len()));
            groups.push(vec![vec![MatchStep::Bind(rest_path, slot_of(binders, *name))]]);
        }
        for (i, elem) in suffix.iter().enumerate() {
            if let HirMatchElem::Elem(matcher) = elem {
                let mut elem_path = path.to_vec();
                elem_path.push(Access::ArrayBack(suffix.len() - i));
                groups.push(self.lower_value(matcher, elem_path, binders, node)?);
            }
        }
        Ok(cartesian_product(groups))
    }
}

/// The three ways a shared branch sorts a clause: the clause satisfies it (with that step
/// discharged), it conflicts and only survives the unmatched side, or it is indifferent to the
/// branch and survives both sides.
enum Branch<'a> {
    Matched(Clause<'a>),
    Unmatched,
    Both,
}

/// Sorts each clause into the matched and unmatched successor lists by `classify`. An indifferent
/// clause is cloned into both.
fn partition<'a>(clauses: &[Clause<'a>], classify: impl Fn(&Clause<'a>) -> Branch<'a>) -> (Vec<Clause<'a>>, Vec<Clause<'a>>) {
    let mut matched = Vec::new();
    let mut unmatched = Vec::new();
    for clause in clauses {
        match classify(clause) {
            Branch::Matched(c) => matched.push(c),
            Branch::Unmatched => unmatched.push(clause.clone()),
            Branch::Both => { matched.push(clause.clone()); unmatched.push(clause.clone()); },
        }
    }
    (matched, unmatched)
}

/// Builds the decision tree from the clauses in priority order, preferring a shareable test over a
/// clause-unique nested matcher. Every test the top clause has is needed to match it, so branching on
/// any of them is sound. Among them it picks the one that decides the most clauses. A chosen equality
/// test at a nested path builds an n-ary `Switch` over the values there, anything else a binary `Test`.
pub fn build_tree<'a>(clauses: &[Clause<'a>]) -> DecisionTree<'a> {
    let Some(first) = clauses.first() else {
        return DecisionTree::Fail;
    };

    // Reversing the score keeps the first of equally good tests, so the top clause's earlier tests win.
    let best = first.tests.iter().min_by_key(|(path, test)| {
        Reverse(clauses.iter().filter(|c| decided_by(c, path, test)).count())
    });

    if let Some((path, test)) = best {
        match test {
            // A switch pays a `Dup` per case to share one load. An empty path loads with a single
            // `LoadLocal`, which costs the same as the `Dup`, so there is nothing to share.
            ValueTest::Equal(_) if !path.is_empty() => build_switch_or_test(clauses, path.clone(), test.clone()),
            _ => build_test(clauses, path.clone(), test.clone()),
        }
    } else if let Some((path, matcher)) = first.nested.first() {
        build_nested(clauses, path.clone(), *matcher, first.binders)
    } else {
        build_leaf(clauses)
    }
}

/// The tests a clause makes at one path.
fn tests_at<'t>(clause: &'t Clause<'_>, path: &'t Path) -> impl Iterator<Item = &'t ValueTest> + 't {
    clause.tests.iter().filter(move |(p, _)| p == path).map(|(_, t)| t)
}

/// Whether a test decides a clause, by either satisfying it or conflicting with it. A clause it does
/// not decide has to be duplicated into both branches.
fn decided_by(clause: &Clause<'_>, path: &Path, test: &ValueTest) -> bool {
    tests_at(clause, path).any(|t| t == test || tests_conflict(test, t))
}

/// Sorts a clause against a chosen test at a path. A clause holding that test matches, with the test
/// discharged. A clause with a conflicting test at the same path is unmatched. A clause with neither
/// survives both outcomes.
fn classify<'a>(clause: &Clause<'a>, path: &Path, test: &ValueTest) -> Branch<'a> {
    if let Some(i) = clause.tests.iter().position(|(p, t)| p == path && t == test) {
        Branch::Matched(clause.without_test(i))
    } else if tests_at(clause, path).any(|t| tests_conflict(test, t)) {
        Branch::Unmatched
    } else {
        Branch::Both
    }
}

/// The clauses that survive a test holding. Used where the unmatched side is built separately, so
/// unlike `partition` it does not collect the clauses that side would need.
fn matched_clauses<'a>(clauses: &[Clause<'a>], path: &Path, test: &ValueTest) -> Vec<Clause<'a>> {
    clauses.iter().filter_map(|clause| match classify(clause, path, test) {
        Branch::Matched(c) => Some(c),
        Branch::Both => Some(clause.clone()),
        Branch::Unmatched => None,
    }).collect()
}

/// Branches on a shared test. A clause needing it takes the matched branch, a conflicting one takes the
/// unmatched branch, and an indifferent one takes both.
fn build_test<'a>(clauses: &[Clause<'a>], path: Path, test: ValueTest) -> DecisionTree<'a> {
    let (matched, unmatched) = partition(clauses, |c| classify(c, &path, &test));
    DecisionTree::Test {
        path,
        test,
        matched: Box::new(build_tree(&matched)),
        unmatched: Box::new(build_tree(&unmatched)),
    }
}

/// The distinct values that clauses test a path against by equality, in order of appearance.
fn equal_values_at(clauses: &[Clause], path: &Path) -> Vec<Scalar> {
    let mut values: Vec<Scalar> = Vec::new();
    for clause in clauses {
        for test in tests_at(clause, path) {
            let ValueTest::Equal(value) = test else { continue };
            if !values.contains(value) {
                values.push(value.clone());
            }
        }
    }
    values
}

/// Branches on a path the top clause tests for equality, loading it once. Two or more distinct values
/// there build an n-ary `Switch`; a lone value is not worth the switch and stays a binary `Test`.
fn build_switch_or_test<'a>(clauses: &[Clause<'a>], path: Path, test: ValueTest) -> DecisionTree<'a> {
    let values = equal_values_at(clauses, &path);
    if values.len() < 2 {
        return build_test(clauses, path, test);
    }

    let cases = values.into_iter().map(|value| {
        let matched = matched_clauses(clauses, &path, &ValueTest::Equal(value.clone()));
        (value, build_tree(&matched))
    }).collect();

    // A clause that pins the path to some value is already in that case, so only the rest can still
    // match once every case has been ruled out.
    let default = clauses.iter()
        .filter(|c| !tests_at(c, &path).any(|t| matches!(t, ValueTest::Equal(_))))
        .cloned()
        .collect::<Vec<_>>();
    DecisionTree::Switch { path, cases, default: Box::new(build_tree(&default)) }
}

/// Runs a clause-unique nested matcher. Only its owning clause (the first) takes the matched branch,
/// minus the nested step. The clauses below the owner take both branches, since the nested match does
/// not decide them.
fn build_nested<'a>(clauses: &[Clause<'a>], path: Path, matcher: HirId<HirMatcher>, binders: &'a [(Symbol, u8)]) -> DecisionTree<'a> {
    let (mut matched, unmatched) = partition(&clauses[1..], |_| Branch::Both);
    matched.insert(0, clauses[0].without_nested(0));
    DecisionTree::Nested {
        path,
        matcher,
        binders,
        matched: Box::new(build_tree(&matched)),
        unmatched: Box::new(build_tree(&unmatched)),
    }
}

/// A clause with no discriminating step left is a leaf: its binds run, then its body. A guarded leaf
/// falls to the clauses below it when its guard fails.
fn build_leaf<'a>(clauses: &[Clause<'a>]) -> DecisionTree<'a> {
    let first = &clauses[0];
    let otherwise = match first.guard {
        Some(_) => Box::new(build_tree(&clauses[1..])),
        // An unguarded leaf's `otherwise` is unreachable. DecisionTree::Fail here is just filler.
        None => Box::new(DecisionTree::Fail),
    };
    DecisionTree::Leaf { binds: first.binds.clone(), guard: first.guard, body: first.body, otherwise }
}

/// Whether a matcher must compile as a nested matcher instead of flat tree steps. The decision tree
/// reloads a value's whole path from the scrutinee per step, so a nested pattern would reload its
/// entire path each time the nested pattern's sub-values are read. A nested matcher loads the path
/// once, so that such sub-values can be matched efficiently.
fn needs_nested_matcher(hir: &Hir, matcher: &HirId<HirMatcher>) -> bool {
    match hir.get(matcher) {
        // Atoms read their value at most once.
        HirMatcher::Wildcard
        | HirMatcher::Literal(_)
        | HirMatcher::Binder(_)
        | HirMatcher::Type { nominal: true, shape: None, .. } => false,
        // A structural type tests each surface member, a shaped type tests the shape too, and the
        // rest read several sub-values or bind and re-match.
        HirMatcher::Type { .. }
        | HirMatcher::Shape(_)
        | HirMatcher::Array(_)
        | HirMatcher::As(..)
        | HirMatcher::And(_)
        | HirMatcher::Or(_) => true,
    }
}

/// Whether two tests at the same path are mutually exclusive, so no value satisfies both.
fn tests_conflict(selected: &ValueTest, other: &ValueTest) -> bool {
    match (selected, other) {
        (ValueTest::Equal(a), ValueTest::Equal(b)) => a != b,
        (ValueTest::ArrayLen { min: m, exact: em }, ValueTest::ArrayLen { min: n, exact: en }) => match (em, en) {
            (true, true) => m != n, // conflict: exact lengths differ
            (true, false) => m < n, // conflict: `selected` exact length is less than the `other`'s minimum
            (false, true) => n < m, // conflict: the `other`'s exact length is less than `selected`'s minimum
            (false, false) => false,
        },
        _ => false,
    }
}

/// Prepends a fixed conjunction of steps to the front of every alternative.
fn prepend_step<'a>(prefix: &[MatchStep], alts: &mut Alternatives) {
    for alt in alts.iter_mut() {
        alt.splice(0..0, prefix.iter().cloned());
    }
}

fn cartesian_product(groups: Vec<Alternatives>) -> Alternatives {
    // [[[A], [B]], [[1], [2]]] => [[A, 1], [A, 2], [B, 1], [B, 2]]
    let mut acc = vec![vec![]];
    for group in groups {
        for base in std::mem::take(&mut acc) {
            for alt in &group {
                let mut combined = base.clone();
                combined.extend(alt.iter().cloned());
                acc.push(combined);
            }
        }
    }
    acc
}
