//! Linear compilation of a single matcher into a boolean test, for the `~` and `has` operators.
//! Unlike a `match`, there is only one matcher, so its tests are emitted as a straight
//! short-circuit sequence with no decision tree.

use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::bind::TypeLayout;
use crate::middle::hir::{TypeId, HirExpr, HirId, HirLiteral, HirMatchElem, HirMatcher, Symbol};
use crate::middle::ir::Inst;

use super::{slot_of, split_at_rest, Compiler};

/// One AND step in a stack-based array match: the length, an element by front or back index, or a
/// named `..` binding the middle slice. `Back(k)` is the kth element counted from the end.
#[derive(Clone, Copy)]
enum ArrayStep {
    Len(usize, bool),
    Front(usize, HirId<HirMatcher>),
    Back(usize, HirId<HirMatcher>),
    BindRest(u8, u8, u8),
}

impl<'a> Compiler<'a> {
    pub(in crate::middle::codegen) fn compile_matcher_test(&mut self, matcher: &HirId<HirMatcher>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.compile_matcher(matcher, None, node)
    }

    pub(in crate::middle::codegen) fn compile_binding_matcher(&mut self, matcher: &HirId<HirMatcher>, binders: &[(Symbol, u8)], node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.compile_matcher(matcher, Some(binders), node)
    }

    /// Compiles a matcher against the receiver on the stack, consuming it and pushing a boolean.
    /// In binding mode `binders` is `Some` and a binder stores into its slot. In test mode it is
    /// `None` and a binder is a compile error, since the `has` and `~` operators cannot bind.
    fn compile_matcher(&mut self, matcher: &HirId<HirMatcher>, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(matcher) {
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
            HirMatcher::Type { nominal, name, shape } => self.compile_type(matcher, *nominal, *name, shape, binders, node),
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

    fn compile_type(&mut self, matcher: &HirId<HirMatcher>, nominal: bool, name: Symbol, shape: &Option<HirId<HirMatcher>>, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match shape {
            None => self.compile_type_atom(matcher, nominal, name, node),
            Some(shape) => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => c.compile_type_atom(matcher, nominal, name, n),
                _ => c.compile_matcher(shape, binders, n),
            }),
        }
    }

    fn compile_type_atom(&mut self, matcher: &HirId<HirMatcher>, nominal: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if nominal {
            let id = self.type_test_id(matcher, node)?;
            self.emit(Inst::Is(id), node);
            return Ok(());
        }
        let members = self.surface_members(matcher, name, node)?;
        if members.is_empty() {
            self.emit(Inst::IsShaped, node);
            return Ok(());
        }
        self.compile_test_and(members.len(), node, &|c, i, n| {
            let (member, admits) = &members[i];
            let idx = c.member_constant(*member)?;
            let inst = match admits {
                Some((null_allowed, witnesses)) => {
                    let allow = c.witness_id_set(witnesses);
                    Inst::MemberAdmits(idx, *null_allowed, c.ir.add_witness_allow(allow)?)
                },
                None => Inst::HasMember(idx),
            };
            c.emit(inst, n);
            Ok(())
        })
    }

    /// Whether a member admits null, and the witnesses its clause allows. `None` for a method.
    pub(super) fn member_admits(&self, layout: &TypeLayout, member: Symbol) -> Option<(bool, Vec<TypeId>)> {
        if !layout.is_field(member) {
            return None;
        }
        let clause = layout.clause_of(member);
        let container = clause.is_some_and(|c| c.container);
        let owes = |ob: &Symbol| clause.is_some_and(|c| c.owed.contains(ob));

        let witnesses: Vec<TypeId> = match container {
            true => Vec::new(),
            false => self.sigs.object_witnesses().filter(|(ob, _)| owes(ob)).map(|(_, id)| id).collect(),
        };
        Some((layout.is_nullable(member) || (!container && owes(&self.sigs.opt)), witnesses))
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
                self.compile_matcher(&matcher, binders, node)
            },
            ArrayStep::Back(from_end, matcher) => {
                self.emit(Inst::Dup, node);
                self.emit(Inst::ArrayLen, node);
                let idx = self.ir.add_constant(Value::from(from_end as f64))?;
                self.emit(Inst::PushConstant(idx), node);
                self.emit(Inst::Subtract, node);
                self.emit(Inst::GetIndex, node);
                self.compile_matcher(&matcher, binders, node)
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

    fn compile_field(&mut self, key: &HirLiteral, value: &HirId<HirMatcher>, binders: Option<&[(Symbol, u8)]>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let key_idx = self.key_constant(key)?;
        match self.hir.get(value) {
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
            // A test that rejects null already fails on an absent key, which reads as null.
            _ if self.hir.get(value).rejects_null(self.hir) => {
                self.emit(Inst::GetIndexOrNull(key_idx), node);
                self.compile_matcher(value, binders, node)
            },
            _ => self.compile_test_and(2, node, &|c, i, n| match i {
                0 => { c.emit(Inst::HasMember(key_idx), n); Ok(()) },
                _ => {
                    c.emit(Inst::GetIndexOrNull(key_idx), n);
                    c.compile_matcher(value, binders, n)
                },
            }),
        }
    }
}

/// Builds the ordered AND steps for an array matcher: the length, the prefix elements from the
/// front, a named rest, then the suffix elements from the back. The rest binds only in binding
/// mode. The length is exact unless a `..` is present.
fn array_steps(elements: &[HirMatchElem], binders: Option<&[(Symbol, u8)]>) -> Vec<ArrayStep> {
    let (prefix, rest, suffix) = split_at_rest(elements);
    let mut steps = vec![ArrayStep::Len(prefix.len() + suffix.len(), rest.is_none())];
    for (i, elem) in prefix.iter().enumerate() {
        if let HirMatchElem::Elem(matcher) = elem { steps.push(ArrayStep::Front(i, *matcher)); }
    }
    if let (Some(binders), Some(HirMatchElem::Rest(Some(name)))) = (binders, rest) {
        steps.push(ArrayStep::BindRest(prefix.len() as u8, suffix.len() as u8, slot_of(binders, *name)));
    }
    for (i, elem) in suffix.iter().enumerate() {
        if let HirMatchElem::Elem(matcher) = elem { steps.push(ArrayStep::Back(suffix.len() - i, *matcher)); }
    }
    steps
}
