//! Match statement, `~` match operator, and `has` codegen.

use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirId, HirLiteral, HirMatchArm, HirMatchElem, HirMatcher, HirStmt, Symbol};
use crate::middle::ir::{Inst, Label};

use super::Compiler;

/// A matcher literal or shape key.
#[derive(Clone, PartialEq)]
enum Scalar {
    Null,
    Bool(bool),
    Num(u64),
    Str(String),
}

impl From<&HirLiteral> for Scalar {
    fn from(lit: &HirLiteral) -> Self {
        match lit {
            HirLiteral::Null => Scalar::Null,
            HirLiteral::Boolean(b) => Scalar::Bool(*b),
            // f64 is not Eq, but the decision tree compares scalars to share and exclude tests, so
            // a number is keyed by its bits. The grammar admits no signed zero or NaN literal, the
            // only values where bit equality parts from the runtime `==`, so the key stays faithful.
            HirLiteral::Number(n) => Scalar::Num(n.to_bits()),
            HirLiteral::String(s) => Scalar::Str(s.clone()),
            _ => unreachable!("a matcher literal or key is a scalar"),
        }
    }
}

/// One hop from a value into a nested value.
#[derive(Clone, PartialEq)]
enum Access {
    Field(Scalar),
    ArrayFront(usize),
    ArrayBack(usize),
    ArrayMiddle(usize, usize),
}

/// How to reach a nested value from the value being matched.
type Path = Vec<Access>;

/// One discriminating test on the value at a path.
#[derive(Clone, PartialEq)]
enum ValueTest {
    Present(Scalar),
    Equal(Scalar),
    Nominal(Symbol),
    ArrayLen { min: usize, exact: bool },
    /// The value is a dict or instance, the only kinds a shape matches.
    Shaped,
}

/// A single step in matching one clause: test a path, bind a path, or run a nested matcher at a path.
#[derive(Clone)]
enum MatchStep<'a> {
    Test(Path, ValueTest),
    Bind(Path, u8),
    /// The decision tree reloads a value's whole path from the scrutinee per step, so a nested pattern
    /// would reload its entire path each time the nested pattern's sub-values are read. A nested matcher
    /// loads the path once, so that such sub-values can be matched efficiently.
    Nested(Path, &'a HirMatcher),
}

/// One alternative's steps: an AND that must all hold for the alternative to match.
type Steps<'a> = Vec<MatchStep<'a>>;

/// A matcher's alternatives: an OR across step lists. Only an or-matcher yields more than one.
type Alternatives<'a> = Vec<Steps<'a>>;

/// One clause of the compiled match: a conjunction of steps that, when they all hold and the guard
/// passes, runs `body`. An or-matcher expands one arm into several clauses that share a body label.
#[derive(Clone)]
struct Clause<'a> {
    steps: Steps<'a>,
    guard: Option<HirId<HirExpr>>,
    body: Label,
    /// The arm's binder name-to-slot, for compiling a `Nested` step's binders.
    binders: &'a [(Symbol, u8)],
}

/// One AND step in a stack-based array match: the length, an element by front or back index, or a
/// named `..` binding the middle slice. `Back(k)` is the kth element counted from the end.
#[derive(Clone, Copy)]
enum ArrayStep<'a> {
    Len(usize, bool),
    Front(usize, &'a HirMatcher),
    Back(usize, &'a HirMatcher),
    BindRest(u8, u8, u8),
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
enum DecisionTree<'a> {
    /// No arm matched.
    Fail,
    /// A matched clause.
    Leaf { binds: Vec<(Path, u8)>, guard: Option<HirId<HirExpr>>, body: Label, otherwise: Box<DecisionTree<'a>> },
    /// A shared value test at a path.
    Test { path: Path, test: ValueTest, matched: Box<DecisionTree<'a>>, unmatched: Box<DecisionTree<'a>> },
    /// A clause-unique nested matcher at a path.
    Nested { path: Path, matcher: &'a HirMatcher, binders: &'a [(Symbol, u8)], matched: Box<DecisionTree<'a>>, unmatched: Box<DecisionTree<'a>> },
}

impl<'a> Compiler<'a> {
    /// Compiles a `match` statement into a decision tree. The scrutinee is evaluated once into a
    /// temp. Each shared test is emitted once and partitions the live arms by its outcome.
    pub (super) fn compile_match(&mut self, scrutinee: &HirId<HirExpr>, arms: &'a [HirMatchArm], stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
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
                clauses.push(Clause { steps, guard: arm.guard, body: body_labels[i], binders });
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
        self.exit_scope(stmt_id);
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
                Access::ArrayFront(i) => {
                    let idx = self.ir.add_constant(Value::from(*i as f64))?;
                    self.emit(Inst::PushConstant(idx), node);
                    self.emit(Inst::GetIndex, node);
                },
                Access::ArrayBack(j) => {
                    self.emit(Inst::Dup, node);
                    self.emit(Inst::ArrayLen, node);
                    let idx = self.ir.add_constant(Value::from(*j as f64))?;
                    self.emit(Inst::PushConstant(idx), node);
                    self.emit(Inst::Subtract, node);
                    self.emit(Inst::GetIndex, node);
                },
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
            ValueTest::Equal(lit) => {
                let idx = self.scalar_constant(lit)?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(Inst::Equal, node);
            },
            ValueTest::Nominal(name) => {
                let n = self.gc.intern(self.hir.text(*name));
                let idx = self.ir.add_constant(Value::from(n))?;
                self.emit(Inst::Is(idx), node);
            },
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

    /// Lowers a matcher into its alternatives at `path`. Each alternative is an AND of steps, and
    /// the alternatives are an OR. Only an or-matcher yields more than one alternative.
    fn lower_matcher(&self, matcher: &'a HirMatcher, path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives<'a>, anyhow::Error> {
        Ok(match matcher {
            HirMatcher::Wildcard => vec![vec![]],
            HirMatcher::Literal(lit) => vec![vec![MatchStep::Test(path.to_vec(), ValueTest::Equal(lit.into()))]],
            HirMatcher::Binder(name) => vec![vec![MatchStep::Bind(path.to_vec(), slot_of(binders, *name))]],
            HirMatcher::As(name, inner) => {
                let mut alts = self.lower_matcher(inner, path, binders, node)?;
                let bind = MatchStep::Bind(path.to_vec(), slot_of(binders, *name));
                prepend_step(&[bind], &mut alts);
                alts
            },
            HirMatcher::Type { nominal, name, shape } => self.lower_type(*nominal, *name, shape, path, binders, node)?,
            HirMatcher::Shape(fields) if fields.is_empty() => vec![vec![MatchStep::Test(path.to_vec(), ValueTest::Shaped)]],
            HirMatcher::Shape(fields) => {
                let mut groups = Vec::with_capacity(fields.len());
                for field in fields {
                    let key = Scalar::from(&field.key);
                    let mut field_path = path.to_vec();
                    field_path.push(Access::Field(key.clone()));
                    let mut field_alts = self.lower_value(&field.value, field_path, binders, node)?;

                    // A value test that rejects null already fails on an absent key.
                    if !matcher_rejects_null(&field.value) {
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
    fn lower_value(&self, matcher: &'a HirMatcher, path: Vec<Access>, binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives<'a>, anyhow::Error> {
        if needs_nested_matcher(matcher) {
            Ok(vec![vec![MatchStep::Nested(path, matcher)]])
        } else {
            self.lower_matcher(matcher, &path, binders, node)
        }
    }

    /// Lowers a type matcher: the type test, then the optional shape that destructures it further.
    /// A nominal type is one `is` test. A structural one tests each surface member's presence.
    fn lower_type(&self, nominal: bool, name: Symbol, shape: &'a Option<Box<HirMatcher>>, path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives<'a>, anyhow::Error> {
        let base = if nominal {
            vec![MatchStep::Test(path.to_vec(), ValueTest::Nominal(name))]
        } else {
            let members = match self.bindings.surface(name) {
                Some(members) => members.to_vec(),
                None => compiler_error!(self, node, "'{}' is not a type or trait", self.hir.text(name)),
            };
            members.iter()
                .map(|member| MatchStep::Test(path.to_vec(), ValueTest::Present(Scalar::Str(self.hir.text(*member).to_string()))))
                .collect()
        };

        let mut alts = match shape {
            None => vec![vec![]],
            Some(shape) => self.lower_matcher(shape, path, binders, node)?,
        };
        prepend_step(&base, &mut alts);
        Ok(alts)
    }

    fn lower_array(&self, elements: &'a [HirMatchElem], path: &[Access], binders: &[(Symbol, u8)], node: &HirId<HirStmt>) -> Result<Alternatives<'a>, anyhow::Error> {
        let (prefix, rest, suffix) = split_at_rest(elements);

        // The length is the first test, so an array test always precedes any element load.
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

    pub (super) fn compile_has(&mut self, left: &HirId<HirExpr>, matcher: &HirMatcher, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(left)?;
        self.compile_matcher_test(matcher, node)
    }

    /// Compiles a matcher against the receiver on the stack, consuming it and pushing a boolean.
    /// In binding mode `binders` is `Some` and a binder stores into its slot. In test mode it is
    /// `None` and a binder is a compile error, since the `has` and `~` operators cannot bind.
    fn compile_matcher(&mut self, matcher: &HirMatcher, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match matcher {
            HirMatcher::Wildcard => {
                self.emit(Inst::Pop, node);
                self.emit(Inst::PushTrue, node);
                Ok(())
            },
            HirMatcher::Literal(lit) => {
                self.literal(node, lit)?;
                self.emit(Inst::Equal, node);
                Ok(())
            },
            HirMatcher::Binder(name) => {
                let Some(binders) = binders else { compiler_error!(self, node, "a `has` matcher cannot bind"); };
                self.emit(Inst::StoreLocalPop(slot_of(binders, *name)), node);
                self.emit(Inst::PushTrue, node);
                Ok(())
            },
            HirMatcher::As(name, inner) => {
                let Some(binders) = binders else { compiler_error!(self, node, "a `has` matcher cannot bind"); };
                self.emit(Inst::Dup, node);
                self.emit(Inst::StoreLocalPop(slot_of(binders, *name)), node);
                self.compile_matcher(inner, Some(binders), node)
            },
            HirMatcher::Type { nominal, name, shape } => self.compile_type(*nominal, *name, shape, binders, node),
            // An empty shape still requires a dict or instance, so it is not a vacuous match.
            HirMatcher::Shape(fields) if fields.is_empty() => {
                self.emit(Inst::IsShaped, node);
                Ok(())
            },
            HirMatcher::Shape(fields) => self.compile_test_and(fields.len(), node, &|c, i, n| {
                c.compile_field(&fields[i].key, &fields[i].value, binders, n)
            }),
            HirMatcher::Array(elements) => self.compile_array(elements, binders, node),
            HirMatcher::And(parts) => self.compile_test_and(parts.len(), node, &|c, i, n| c.compile_matcher(&parts[i], binders, n)),
            // Each alternative binds the same names, so it stores into the same slots before the
            // shared continuation.
            HirMatcher::Or(parts) => self.compile_test_or(parts.len(), node, &|c, i, n| c.compile_matcher(&parts[i], binders, n)),
        }
    }

    pub (super) fn compile_matcher_test(&mut self, matcher: &HirMatcher, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.compile_matcher(matcher, None, node)
    }

    fn compile_type(&mut self, nominal: bool, name: Symbol, shape: &Option<Box<HirMatcher>>, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match shape {
            None => self.compile_type_atom(nominal, name, node),
            Some(shape) => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => c.compile_type_atom(nominal, name, n),
                _ => c.compile_matcher(shape, binders, n),
            }),
        }
    }

    fn compile_type_atom(&mut self, nominal: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if nominal {
            let name_ref = self.gc.intern(self.hir.text(name));
            let idx = self.ir.add_constant(Value::from(name_ref))?;
            self.emit(Inst::Is(idx), node);
            return Ok(());
        }
        let members = match self.bindings.surface(name) {
            Some(members) => members.to_vec(),
            None => compiler_error!(self, node, "'{}' is not a type or trait", self.hir.text(name)),
        };
        self.compile_test_and(members.len(), node, &|c, i, n| {
            let idx = c.member_constant(members[i])?;
            c.emit(Inst::HasMember(idx), n);
            Ok(())
        })
    }

    fn compile_array(&mut self, elements: &[HirMatchElem], binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // One AND test per step: the length first, then each element by a front or back index, with
        // a named `..` binding the middle slice. A `..` makes the length test a minimum.
        let steps = array_steps(elements, binders);
        self.compile_test_and(steps.len(), node, &|c, i, n| c.emit_array_step(steps[i], binders, n))
    }

    /// Emits one array step against the array on top of the stack.
    fn emit_array_step(&mut self, step: ArrayStep, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match step {
            ArrayStep::Len(min, exact) => {
                self.emit(Inst::ArrayLen, node);
                let idx = self.ir.add_constant(Value::from(min as f64))?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(if exact { Inst::Equal } else { Inst::GreaterThanEqual }, node);
                Ok(())
            },
            ArrayStep::Front(index, matcher) => {
                let idx = self.ir.add_constant(Value::from(index as f64))?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(Inst::GetIndex, node);
                self.compile_matcher(matcher, binders, node)
            },
            ArrayStep::Back(from_end, matcher) => {
                self.emit(Inst::Dup, node);
                self.emit(Inst::ArrayLen, node);
                let idx = self.ir.add_constant(Value::from(from_end as f64))?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(Inst::Subtract, node);
                self.emit(Inst::GetIndex, node);
                self.compile_matcher(matcher, binders, node)
            },
            ArrayStep::BindRest(prefix_len, suffix_len, slot) => {
                self.emit(Inst::ArrayMiddle(prefix_len, suffix_len), node);
                self.emit(Inst::StoreLocalPop(slot), node);
                self.emit(Inst::PushTrue, node);
                Ok(())
            },
        }
    }

    fn compile_test_and(&mut self, count: usize, node: &HirId<HirExpr>, compile_test: &dyn Fn(&mut Self, usize, &HirId<HirExpr>) -> Result<(), anyhow::Error>) -> Result<(), anyhow::Error> {
        // An empty AND holds for any receiver, so drop it and yield true.
        if count == 0 {
            self.emit(Inst::Pop, node);
            self.emit(Inst::PushTrue, node);
            return Ok(());
        }

        if count == 1 {
            return compile_test(self, 0, node);
        }

        let fail = self.ir.new_label();
        let end = self.ir.new_label();

        // Each test but the last runs on a duplicate so the receiver survives for the next.
        for i in 0..count - 1 {
            self.emit(Inst::Dup, node);
            compile_test(self, i, node)?;
            self.emit(Inst::JumpIfFalse(fail), node);
        }

        compile_test(self, count - 1, node)?;
        self.emit(Inst::Jump(end), node);
        self.ir.bind(fail);
        self.emit(Inst::Pop, node); // a test failed: drop the surviving receiver, yield false
        self.emit(Inst::PushFalse, node);
        self.ir.bind(end);
        Ok(())
    }

    fn compile_test_or(&mut self, count: usize, node: &HirId<HirExpr>, compile_test: &dyn Fn(&mut Self, usize, &HirId<HirExpr>) -> Result<(), anyhow::Error>) -> Result<(), anyhow::Error> {
        if count == 1 {
            return compile_test(self, 0, node);
        }
        let end = self.ir.new_label();
        for i in 0..count {
            let last = i == count - 1;
            let next = self.ir.new_label();
            // Every alternative but the last keeps a copy so a later one can retry the receiver.
            if !last {
                self.emit(Inst::Dup, node);
            }
            compile_test(self, i, node)?;
            self.emit(Inst::JumpIfFalse(next), node);
            if !last {
                self.emit(Inst::Pop, node); // matched: drop the surviving receiver
            }
            self.emit(Inst::PushTrue, node);
            self.emit(Inst::Jump(end), node);
            self.ir.bind(next);
        }
        // Reached only when the last alternative failed. Its receiver was already consumed.
        self.emit(Inst::PushFalse, node);
        self.ir.bind(end);
        Ok(())
    }

    fn compile_field(&mut self, key: &HirLiteral, value: &HirMatcher, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let key_idx = self.key_constant(key)?;
        match value {
            HirMatcher::Wildcard => {
                self.emit(Inst::HasMember(key_idx), node);
                Ok(())
            },
            // `{ k }` shorthand in binding mode: require the key present and bind its value.
            HirMatcher::Binder(name) if binders.is_some() => {
                let slot = slot_of(binders.unwrap(), *name);
                self.emit(Inst::Dup, node);
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.emit(Inst::StoreLocalPop(slot), node);
                self.emit(Inst::HasMember(key_idx), node);
                Ok(())
            },
            HirMatcher::Literal(HirLiteral::Null) => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => { c.emit(Inst::HasMember(key_idx), n); Ok(()) },
                _ => {
                    c.emit(Inst::GetIndexOrNull(key_idx), n);
                    c.emit(Inst::PushNull, n);
                    c.emit(Inst::Equal, n);
                    Ok(())
                },
            }),
            HirMatcher::Literal(lit) => {
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.literal(node, lit)?;
                self.emit(Inst::Equal, node);
                Ok(())
            },
            nested => {
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.compile_matcher(nested, binders, node)
            },
        }
    }

    pub (super) fn compile_binding_matcher(&mut self, matcher: &HirMatcher, binders: &[(Symbol, u8)], node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.compile_matcher(matcher, Some(binders), node)
    }

    /// Interns a member name (a string key) into the constant pool, returning its index.
    fn member_constant(&mut self, name: Symbol) -> Result<u8, anyhow::Error> {
        let name_ref = self.gc.intern(self.hir.text(name));
        self.ir.add_constant(Value::from(name_ref))
    }

    /// Pools a scalar-literal `has` key as its runtime value, returning the constant index.
    fn key_constant(&mut self, key: &HirLiteral) -> Result<u8, anyhow::Error> {
        self.scalar_constant(&key.into())
    }

    /// Pools a scalar matcher literal as its runtime value, returning the constant index.
    fn scalar_constant(&mut self, lit: &Scalar) -> Result<u8, anyhow::Error> {
        let value = match lit {
            Scalar::Null => Value::NULL,
            Scalar::Bool(b) => Value::from(*b),
            Scalar::Num(bits) => Value::from(f64::from_bits(*bits)),
            Scalar::Str(s) => Value::from(self.gc.intern(s)),
        };
        self.ir.add_constant(value)
    }
}

/// Builds the decision tree from the clauses in priority order. The first clause's first
/// discriminating step is selected, preferring a shareable `Test` over a clause-unique `Nested`,
/// and partitions the clauses by its outcome.
fn build_tree<'a>(clauses: &[Clause<'a>]) -> DecisionTree<'a> {
    let Some(first) = clauses.first() else {
        return DecisionTree::Fail;
    };

    // Pick the first clause's first discriminating step. With none, every test held so it is a leaf.
    let pick = first.steps.iter().position(|s| matches!(s, MatchStep::Test(..)))
        .or_else(|| first.steps.iter().position(|s| matches!(s, MatchStep::Nested(..))));
    let Some(idx) = pick else {
        return build_leaf(clauses);
    };

    match &first.steps[idx] {
        MatchStep::Test(path, test) => build_test(clauses, path.clone(), test.clone()),
        MatchStep::Nested(path, matcher) => build_nested(clauses, idx, path.clone(), *matcher, first.binders),
        MatchStep::Bind(..) => unreachable!("pick only selects a Test or Nested step"),
    }
}

/// A clause whose steps all hold is a leaf. A guarded leaf falls to the clauses below it when its
/// guard fails.
fn build_leaf<'a>(clauses: &[Clause<'a>]) -> DecisionTree<'a> {
    let first = &clauses[0];
    let binds = first.steps.iter()
        .filter_map(|step| match step {
            MatchStep::Bind(path, slot) => Some((path.clone(), *slot)),
            _ => None,
        })
        .collect();
    let otherwise = match first.guard {
        Some(_) => Box::new(build_tree(&clauses[1..])),
        // An unguarded leaf's `otherwise` is unreachable. DecisionTree::Fail here is just filler.
        None => Box::new(DecisionTree::Fail),
    };
    DecisionTree::Leaf { binds, guard: first.guard, body: first.body, otherwise }
}

/// Branches on a shared test. A clause needing it takes the true branch with the test removed. A
/// clause with a conflicting test at the same path takes only the false branch. A clause that
/// neither needs nor conflicts with it takes both.
fn build_test<'a>(clauses: &[Clause<'a>], path: Path, test: ValueTest) -> DecisionTree<'a> {
    let mut matched = Vec::new();
    let mut unmatched = Vec::new();
    for clause in clauses {
        if let Some(p) = clause.steps.iter().position(|s| matches!(s, MatchStep::Test(sp, st) if *sp == path && *st == test)) {
            let mut c = clause.clone();
            c.steps.remove(p);
            matched.push(c);
        } else if clause.steps.iter().any(|s| matches!(s, MatchStep::Test(sp, st) if *sp == path && tests_conflict(&test, st))) {
            unmatched.push(clause.clone());
        } else {
            matched.push(clause.clone());
            unmatched.push(clause.clone());
        }
    }
    DecisionTree::Test {
        path,
        test,
        matched: Box::new(build_tree(&matched)),
        unmatched: Box::new(build_tree(&unmatched)),
    }
}

/// Runs a clause-unique nested matcher. Only its owning clause takes the true branch, minus the
/// nested step. The clauses below the owner take both branches since the nested match does not
/// decide them.
fn build_nested<'a>(clauses: &[Clause<'a>], idx: usize, path: Path, matcher: &'a HirMatcher, binders: &'a [(Symbol, u8)]) -> DecisionTree<'a> {
    let mut owner = clauses[0].clone();
    owner.steps.remove(idx);
    let mut matched = vec![owner];
    matched.extend(clauses[1..].iter().cloned());
    let unmatched = clauses[1..].to_vec();
    DecisionTree::Nested {
        path,
        matcher,
        binders,
        matched: Box::new(build_tree(&matched)),
        unmatched: Box::new(build_tree(&unmatched)),
    }
}

/// Whether a matcher must compile as a nested matcher instead of flat tree steps. The decision tree
/// reloads a value's whole path from the scrutinee per step, so a nested pattern would reload its
/// entire path each time the nested pattern's sub-values are read. A nested matcher loads the path
/// once, so that such sub-values can be matched efficiently.
fn needs_nested_matcher(matcher: &HirMatcher) -> bool {
    match matcher {
        // Atoms read their value at most once.
        HirMatcher::Wildcard
        | HirMatcher::Literal(_)
        | HirMatcher::Binder(_)
        | HirMatcher::Type { nominal: true, shape: None, .. } => false,
        // A structural type tests each surface member, a shaped type tests the shape too, and the
        // rest read several sub-values or bind and re-match.
        HirMatcher::Type { nominal: false, .. }
        | HirMatcher::Type { shape: Some(_), .. }
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

/// Whether a matcher fails against null. A shape field whose value rejects null needs no separate
/// presence test: an absent key loads as null, which the value test already rejects. Wildcards,
/// binders, and a `null` literal accept null, so those keep their presence test.
fn matcher_rejects_null(matcher: &HirMatcher) -> bool {
    match matcher {
        HirMatcher::Wildcard | HirMatcher::Binder(_) => false,
        HirMatcher::Literal(HirLiteral::Null) => false,
        HirMatcher::Literal(_) => true,
        HirMatcher::Type { .. } | HirMatcher::Array(_) => true,
        HirMatcher::Shape(_) => true,
        HirMatcher::As(_, inner) => matcher_rejects_null(inner),
        HirMatcher::And(parts) => parts.iter().any(matcher_rejects_null),
        HirMatcher::Or(alternatives) => alternatives.iter().all(matcher_rejects_null),
    }
}

/// Prepends a fixed conjunction of steps to the front of every alternative.
fn prepend_step<'a>(prefix: &[MatchStep<'a>], alts: &mut Alternatives<'a>) {
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

/// Splits array elements at the `..` rest. Returns the fixed prefix before it, the rest element
/// itself, and the fixed suffix after it. With no rest, the prefix holds every element.
fn split_at_rest(elements: &[HirMatchElem]) -> (&[HirMatchElem], Option<&HirMatchElem>, &[HirMatchElem]) {
    match elements.iter().position(|e| matches!(e, HirMatchElem::Rest(_))) {
        Some(p) => (&elements[..p], Some(&elements[p]), &elements[p + 1..]),
        None => (elements, None, &[]),
    }
}

/// Builds the ordered AND steps for an array matcher: the length, the prefix elements from the
/// front, a named rest, then the suffix elements from the back. The rest binds only in binding
/// mode. The length is exact unless a `..` is present.
fn array_steps<'a>(elements: &'a [HirMatchElem], binders: Option<&[(Symbol, u8)]>) -> Vec<ArrayStep<'a>> {
    let (prefix, rest, suffix) = split_at_rest(elements);
    let mut steps = vec![ArrayStep::Len(prefix.len() + suffix.len(), rest.is_none())];
    for (i, elem) in prefix.iter().enumerate() {
        if let HirMatchElem::Elem(matcher) = elem { steps.push(ArrayStep::Front(i, matcher)); }
    }
    if let (Some(binders), Some(HirMatchElem::Rest(Some(name)))) = (binders, rest) {
        steps.push(ArrayStep::BindRest(prefix.len() as u8, suffix.len() as u8, slot_of(binders, *name)));
    }
    for (i, elem) in suffix.iter().enumerate() {
        if let HirMatchElem::Elem(matcher) = elem { steps.push(ArrayStep::Back(suffix.len() - i, matcher)); }
    }
    steps
}

/// The reserved local slot a binder name stores into.
fn slot_of(binders: &[(Symbol, u8)], name: Symbol) -> u8 {
    binders.iter().find(|(n, _)| *n == name).map(|(_, slot)| *slot).expect("a binder's slot was recorded")
}
