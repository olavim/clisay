use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::anchors::{self, AnchorKey};
use crate::middle::hir::{access_path_steps, BinOp, AccessStep, HirExpr, HirFnDecl, HirId, HirLiteral, Symbol, UnOp};
use crate::middle::ir::{self, Inst, Label};
use crate::middle::bind::{FnKind, Place};
use crate::middle::check::{AssignStrategy, Barrier, Guard};

use super::{Compiler, Returning};
use crate::middle::bind::AnchorPathSlots;

/// How an index/property (`a.b`, `a[b]`) is being accessed.
#[derive(Clone, Copy)]
enum IndexOp {
    Load,
    Assign { write: HirId<HirExpr>, op: Option<BinOp>, rhs: HirId<HirExpr>, discarded: bool },
}

pub(super) enum CallForm {
    Plain,
    Tail,
    Invoke { name: u8, is_dot: u8, receiver: InvokeReceiver },
    InvokeThis { member: u8, receiver: InvokeReceiver },
}

#[derive(Clone, Copy)]
pub(super) struct InvokeReceiver {
    passes_anchor: bool,
    tested: bool,
}

impl InvokeReceiver {
    fn flags(self) -> u8 {
        let passes_anchor = if self.passes_anchor { ir::CALL_PASSES_ANCHOR_RECEIVER } else { 0 };
        let tested = if self.tested { ir::CALL_TESTS_CALLEE } else { 0 };
        passes_anchor | tested
    }
}

impl CallForm {
    fn build_inst(self, proofs: u8, arity: u8, wants_value: bool) -> Inst {
        let wants = if wants_value { ir::CALL_WANTS_VALUE } else { 0 };
        match self {
            CallForm::Plain => Inst::Call(proofs | wants, arity),
            CallForm::Tail => Inst::TailCall(proofs | wants, arity),
            CallForm::Invoke { name, is_dot, receiver } => Inst::Invoke(name, receiver.flags() | wants, arity, is_dot),
            CallForm::InvokeThis { member, receiver } => Inst::InvokeThis(member, receiver.flags() | wants, arity),
        }
    }
}

/// Reading a `Ref` and taking the step a write continues from are the same load.
fn ref_load(for_write: bool) -> Inst {
    match for_write {
        true => Inst::LoadRefForWrite,
        false => Inst::LoadRef,
    }
}

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let height_before = self.frame_stack_height;
        self.expression_inner(expr)?;
        if let Some(height) = height_before {
            let leaves = !matches!(self.hir.get(expr), HirExpr::Block(_)) as usize;
            self.expect_frame_stack_height(height + leaves, "expression end", expr);
        }
        Ok(())
    }

    fn expression_inner(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr)?,
            HirExpr::Unary(op, operand) => self.unary_expression(*op, operand)?,
            HirExpr::Binary(op, left, right) => self.binary_expression(*op, left, right)?,
            HirExpr::Assign(left, right) => self.compile_assign(left, right, false)?,
            HirExpr::CompoundAssign(target, op, value) => self.compile_assign_op(target, Some(*op), value, false)?,
            HirExpr::Call(callee, args) => {
                self.call_expression(callee, args, false)?;
            },
            HirExpr::Index { base, member, is_dot, .. } => match self.hir.path_has_safe_access(expr) {
                true => self.safe_access_path(expr)?,
                false => self.index(base, member, *is_dot, IndexOp::Load)?,
            },
            HirExpr::Literal(lit) => self.literal(expr, lit)?,
            HirExpr::Identifier(_) => {
                let place = self.place(expr);
                self.emit_load(place, expr)?;
            },
            HirExpr::Construct(callee, brace) => self.construct_expression(expr, callee, brace)?,
            HirExpr::Match(scrutinee, matcher) => {
                self.expression(scrutinee)?;
                match self.bindings.match_binders(expr) {
                    Some(binders) => self.compile_binding_matcher(matcher, binders, expr)?,
                    None => self.compile_matcher_test(matcher, expr)?,
                }
            },
            HirExpr::This => self.emit_load(self.place(expr), expr)?,
            HirExpr::Coalesce(left, right) => self.coalesce(left, right)?,
            HirExpr::SafeCall(callee, args) => self.safe_call(callee, args)?,
            HirExpr::Propagate(operand) => self.propagate(operand)?,
            HirExpr::Handle(left, _, handler) => self.handle(expr, left, handler)?,
            HirExpr::Assert(operand) => self.assert(expr, operand)?,
            HirExpr::Anchor(path) => self.emit_anchor_of(path, expr, None, None)?,
            HirExpr::RefValue { holder, .. } => match self.hir.path_has_safe_access(expr) {
                true => self.safe_access_path(expr)?,
                false => self.emit_ref_load(holder, expr)?,
            },
        };

        self.emit_node_guards(expr)
    }

    /// Each runtime check a node carries, in the order the check pass put them in.
    fn emit_node_guards(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        for guard in self.barriers.guards(expr) {
            self.emit_guard(expr, *guard)?;
        }

        // Under check-forcing, the checks the pass proved unnecessary are emitted too.
        for guard in self.barriers.elided(expr) {
            let at = self.ir.next_index();
            self.emit_guard(expr, *guard)?;
            self.ir.mark_forced_from(at);
        }
        Ok(())
    }

    fn emit_guard(&mut self, node: &HirId<HirExpr>, guard: Guard) -> Result<(), anyhow::Error> {
        // `!` asks for its check in the source, so it's the operation rather than a safety guard.
        if self.drop_guards && !matches!(self.hir.get(node), HirExpr::Assert(_)) {
            return Ok(());
        }
        match guard {
            Guard::Boundary => if let Some(barrier) = self.barriers.boundary(node) {
                self.emit_boundary_barrier(node, barrier)?;
            },
            Guard::NonNull => self.emit(Inst::AssertNonNull, node),
        }
        Ok(())
    }

    fn emit_boundary_barrier(&mut self, node: &HirId<HirExpr>, barrier: &Barrier) -> Result<(), anyhow::Error> {
        let witness_set = self.accepted_witness_set(&barrier.allow_witnesses, barrier.null_allowed);
        let witness_set_pool_id = self.ir.intern_witness_set(witness_set)?;
        self.emit(Inst::BarrierGuard(witness_set_pool_id), node);
        Ok(())
    }

    /// Compiles `a ?? b`.
    fn coalesce(&mut self, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        self.emit(Inst::JumpIfCleanOrPop(end), left);
        self.expression(right)?;
        self.bind_label(end);
        Ok(())
    }

    /// Compiles `cb?(args)`.
    fn safe_call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let wants = self.call_wants_result();
        // The path below reads the callee as a value before testing it, which would lose the anchor.
        // So a call that passes an anchor receiver compiles to an invoke, which does the `?` test itself.
        if self.hir.passes_anchor_receiver(callee) && self.invoke_expression(callee, args, wants, true)? {
            return Ok(());
        }
        let end = self.ir.new_label();
        self.expression(callee)?;
        self.emit_safe_access_test(callee, end);
        self.emit_call(args, callee, wants, CallForm::Plain)?;
        self.bind_label(end);
        Ok(())
    }

    /// Compiles `a?!`.
    fn propagate(&mut self, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(operand)?;
        self.emit_propagate_check(operand)
    }

    fn emit_propagate_check(&mut self, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let cont = self.ir.new_label();
        self.emit(Inst::JumpIfClean(cont), operand);
        self.emit_return_with_cleanup(true, operand, |c| c.emit_return(Returning::Value(operand), operand))?;
        self.bind_label(cont);
        Ok(())
    }

    /// Compiles `a!`.
    fn assert(&mut self, node: &HirId<HirExpr>, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(operand)?;
        self.emit_assert_check(node, operand)
    }

    fn emit_assert_check(&mut self, node: &HirId<HirExpr>, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !self.barriers.asserts_witnesses(node) {
            return Ok(());
        }
        let skip = self.ir.new_label();
        self.emit(Inst::JumpIfClean(skip), operand);
        self.emit(Inst::Throw, operand);
        self.bind_label(skip);
        Ok(())
    }

    /// Compiles `a ?? p => h`.
    fn handle(&mut self, node: &HirId<HirExpr>, left: &HirId<HirExpr>, handler: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        let operand_slot = self.next_slot(node)?;
        self.expression(left)?;
        self.emit(Inst::JumpIfClean(end), left);
        self.handle_binder_slots.push((self.bindings.handle_binder(node), operand_slot));
        let compiled = self.expression(handler);
        self.handle_binder_slots.pop();
        compiled?;
        self.emit(Inst::StoreLocalPop(operand_slot), node);
        self.bind_label(end);
        Ok(())
    }

    
    /// A `?.` short-circuits every step after it, so one exit serves the whole chain.
    fn safe_access_path(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.safe_access_step(expr, end)?;
        self.bind_label(end);
        Ok(())
    }

    fn safe_access_step(&mut self, expr: &HirId<HirExpr>, end: Label) -> Result<(), anyhow::Error> {
        let (target, member, is_dot, has_safe_access) = match self.hir.get(expr) {
            HirExpr::RefValue { holder, safe } => {
                let (holder, safe) = (*holder, *safe);
                return self.ref_value_access_step(expr, &holder, safe, end);
            },
            HirExpr::Index { base, member, is_dot, safe } => (*base, *member, *is_dot, *safe),
            _ => unreachable!("a chain step is an access, since its spine reached a guard"),
        };

        self.emit_safe_step_operand(&target, has_safe_access, end)?;
        self.emit_key_or_saved_copy(&member)?;
        self.emit(access_inst(is_dot), &target);
        Ok(())
    }

    fn emit_safe_access_test(&mut self, at: &HirId<HirExpr>, end: Label) {
        self.emit(Inst::JumpIfBad(end), at);
    }

    pub (super) fn expression_stmt(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.statement_value(expr, false)
    }

    /// `say _ = e;` discards its value on purpose, so nothing is checked.
    pub (super) fn discard_stmt(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.statement_value(expr, true)
    }

    fn statement_value(&mut self, expr: &HirId<HirExpr>, discarded_on_purpose: bool) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr),
            HirExpr::Assign(left, right) => self.compile_assign(left, right, true),
            HirExpr::CompoundAssign(target, op, value) => self.compile_assign_op(target, Some(*op), value, true),
            _ => {
                self.discard_result_of(expr);
                self.expression(expr)?;
                self.discard_statement_value(expr, discarded_on_purpose);
                Ok(())
            }
        }
    }

    fn discard_statement_value(&mut self, expr: &HirId<HirExpr>, discarded_on_purpose: bool) {
        let discardable = self.barriers.value_is_discardable(expr);
        if discarded_on_purpose || discardable && !self.force_checks {
            self.emit(Inst::Pop, expr);
            return;
        }
        let at = self.ir.next_index();
        self.emit(Inst::DiscardChecked, expr);
        // Check-forcing puts back the check on a value the pass proved.
        if discardable {
            self.ir.mark_forced_from(at);
        }
    }

    fn unary_expression(&mut self, op: UnOp, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(unop_inst(op), expr);
        Ok(())
    }

    fn construct_expression(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, brace: &[(Symbol, HirId<HirExpr>)]) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
        for (_, value) in brace {
            self.expression(value)?;
        }
        let field_ids = self.bindings.construct_fields(expr).to_vec();
        let fields_idx = self.ir.add_byte_list(field_ids)?;
        self.emit(Inst::Construct(fields_idx), expr);
        Ok(())
    }

    pub(super) fn emit_return<T: 'static>(&mut self, returning: Returning, node: &HirId<T>) {
        if let Some(witness_set_pool_id) = self.return_check_witness_set_pool_id(returning) {
            self.emit(Inst::BarrierGuard(witness_set_pool_id), node);
        }
        self.emit_anchor_copy_outs(node);
        match matches!(returning, Returning::Value(v) if self.might_outlive_return(v)) {
            true => self.emit(Inst::ReturnShared, node),
            false => self.emit(Inst::Return, node),
        }
    }

    pub(super) fn emit_factory_return<T: 'static>(&mut self, node: &HirId<T>) {
        self.emit(Inst::LoadLocal(0), node);
        self.emit_anchor_copy_outs(node);
        self.emit(Inst::ReturnFac, node);
    }

    pub(super) fn return_needs_check(&self, returning: Returning) -> bool {
        let Some(contract) = self.compiled_fns.last().and_then(|f| f.contract) else { return false };
        match self.hands_back(returning) {
            Returning::Value(value) => !self.barriers.return_is_proven(value),
            // Returning nothing is what a void path does, so only a return allowing one takes it.
            Returning::Nothing => !contract.allows_void,
        }
    }

    /// What the return actually hands back. An expression the checker found void leaves nothing
    /// behind, the way falling off the end does.
    fn hands_back<'x>(&self, returning: Returning<'x>) -> Returning<'x> {
        match returning {
            Returning::Value(value) if self.barriers.return_hands_back_nothing(value) => Returning::Nothing,
            other => other,
        }
    }

    fn return_check_witness_set_pool_id(&self, returning: Returning) -> Option<u16> {
        let contract = self.compiled_fns.last()?.contract?;
        // Check-forcing puts back what the pass settled about a value. Returning nothing is not a
        // value, so there is nothing there to put back.
        let forced = self.barriers.forces_return_tests()
            && matches!(self.hands_back(returning), Returning::Value(_));
        let needed = self.return_needs_check(returning) || forced;
        needed.then_some(contract.witness_set_pool_id)
    }

    pub(super) fn might_outlive_return(&self, expr: &HirId<HirExpr>) -> bool {
        self.hir.reads_handed_back(expr).iter().any(|read| match self.hir.get(read) {
            HirExpr::This => false,
            HirExpr::Identifier(_) => self.bindings.names_outliving_binding(read)
                || !matches!(self.bindings.place_of(read), Some(Place::Local(_))),
            // A path read hands back a value its container still contains.
            _ => true,
        })
    }

    /// `&x`
    fn emit_anchor_of(&mut self, path: &HirId<HirExpr>, node: &HirId<HirExpr>, end: Option<Label>, receiver_safe_access: Option<&HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let Some(Place::Local(slot)) = self.bindings.place_of(path) else {
            return self.emit_anchor_path(path, node, end, receiver_safe_access);
        };

        if receiver_safe_access.is_some() {
            self.emit_load(Place::Local(slot), path)?;
            self.emit_safe_access_test(path, safe_access_exit(end));
            self.emit(Inst::Pop, node);
        }

        match self.bindings.anchor_binding(path) {
            Some(_) => self.emit(Inst::LoadLocal(slot), node),
            None => self.emit(Inst::PushSlotAnchor(slot, self.slot_witness_set_pool_id(slot)), node),
        }

        Ok(())
    }

    fn emit_anchor_path(&mut self, path: &HirId<HirExpr>, node: &HirId<HirExpr>, end: Option<Label>, receiver_safe_access: Option<&HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let (root, steps) = access_path_steps(self.hir, path);
        let Some(slots) = self.bindings.anchor_path_slots(node) else {
            compiler_error!(self, node, "a path anchor reserved no slots");
        };

        assert!(!steps.is_empty(), "a path anchor has at least one step");

        self.emit_anchor_path_keys(&root, &steps, &slots, node, end, receiver_safe_access)?;
        self.emit_root_for_write(&root, None)?;
        let dots = self.ir.add_byte_list(steps.iter().map(|step| step.is_dot as u8).collect())?;
        self.emit(Inst::FormAnchorPath(slots.target(), dots), node);
        self.after_forming_path_anchor(node, slots.target())
    }

    /// Evaluates each key of an anchor path into its reserved slot. A safe access is tested on a
    /// read of the container before its key, so a failed one skips the keys after it.
    fn emit_anchor_path_keys(&mut self, root: &HirId<HirExpr>, steps: &[AccessStep], slots: &AnchorPathSlots, node: &HirId<HirExpr>, end: Option<Label>, receiver_safe_access: Option<&HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        // The walk reads every value up to the last one marked with `?`.
        let steps_to_read = match receiver_safe_access {
            Some(_) => Some(steps.len()),
            None => steps.iter().rposition(|step| step.safe.is_some()),
        };
        if steps_to_read.is_some() {
            self.expression(root)?;
        }
        for (at, step) in steps.iter().enumerate() {
            if step.safe.is_some() {
                self.emit_safe_access_test(node, safe_access_exit(end));
            }
            self.emit_step_key(root, step, at == 0, node)?;
            self.emit(Inst::StoreTempPop(slots.key(at)), node);
            if steps_to_read.is_some_and(|count| at < count) {
                self.emit(Inst::LoadLocal(slots.key(at)), node);
                self.emit(access_inst(step.is_dot), node);
            }
        }
        if receiver_safe_access.is_some() {
            self.emit_safe_access_test(node, safe_access_exit(end));
        }
        if steps_to_read.is_some() {
            self.emit(Inst::Pop, node);
        }
        Ok(())
    }

    fn emit_member_key<T: 'static>(&mut self, id: u8, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let idx = self.ir.add_constant(Value::member_key(id))?;
        self.emit(Inst::PushConstant(idx), node);
        Ok(())
    }

    /// Emits a step's operand, then tests it against the step's own `?` when `safe`.
    fn emit_safe_step_operand(&mut self, operand: &HirId<HirExpr>, safe: bool, end: Label) -> Result<(), anyhow::Error> {
        self.emit_access_step_operand(operand, end)?;
        if safe {
            self.emit_safe_access_test(operand, end);
        }
        Ok(())
    }

    fn emit_access_step_operand(&mut self, operand: &HirId<HirExpr>, end: Label) -> Result<(), anyhow::Error> {
        if let HirExpr::Anchor(path) = self.hir.get(operand) {
            return self.emit_anchor_of(path, operand, Some(end), None);
        }
        match self.hir.path_has_safe_access(operand) {
            true => {
                self.safe_access_step(operand, end)?;
                self.emit_node_guards(operand)
            },
            false => self.expression(operand),
        }
    }

    fn ref_value_access_step(&mut self, expr: &HirId<HirExpr>, holder: &HirId<HirExpr>, safe: bool, end: Label) -> Result<(), anyhow::Error> {
        self.emit_safe_step_operand(holder, safe, end)?;
        self.emit(Inst::LoadRef, expr);
        Ok(())
    }

    fn emit_ref_load(&mut self, holder: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.emit_key_or_saved_copy(holder)?;
        self.emit(ref_load(false), node);
        Ok(())
    }

    /// Pushes the keys a write saves for its walk, in source order.
    fn save_write_path_keys(&mut self, write: &HirId<HirExpr>, saved_write_path_keys: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for key in saved_write_path_keys {
            let at = self.push_write_path_key(write, key)?;
            self.saved_write_path_keys.insert(*key, at);
        }
        Ok(())
    }

    /// Pushes a key of a write's path. Returns the frame slot it lands in.
    fn push_write_path_key(&mut self, write: &HirId<HirExpr>, key: &HirId<HirExpr>) -> Result<u8, anyhow::Error> {
        let slot = self.next_slot(key)?;
        self.expression(key)?;
        self.share_if_marked(write, key)?;
        Ok(slot)
    }

    /// Marks the value on top of the stack shared when its write's strategy lists it in `shared`.
    fn share_if_marked(&mut self, write: &HirId<HirExpr>, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if let AssignStrategy::WalkLast { shared, .. } = self.barriers.assign_strategy(write) {
            if shared.contains(expr) {
                let top = self.operand_count(self.known_frame_stack_height() - 1, "a frame", "slots", expr)?;
                self.emit(Inst::ShareLocal(top), expr);
            }
        }
        Ok(())
    }

    fn emit_key_or_saved_copy(&mut self, key: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.saved_write_path_keys.get(key) {
            Some(&slot) => {
                self.emit(Inst::LoadLocal(slot), key);
                Ok(())
            },
            None => self.expression(key),
        }
    }

    fn drop_saved_write_path_keys(&mut self, write: &HirId<HirExpr>, saved_write_path_keys: &[HirId<HirExpr>], discarded: bool) -> Result<(), anyhow::Error> {
        for key in saved_write_path_keys {
            self.saved_write_path_keys.remove(key);
        }
        self.drop_under_top(saved_write_path_keys.len(), discarded, write)
    }

    /// Drops `count` values, leaving the first one when `discarded` is false.
    fn drop_under_top(&mut self, count: usize, discarded: bool, write: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if discarded {
            for _ in 0..=count {
                self.emit(Inst::Pop, write);
            }
        } else if count > 0 {
            let first_key = self.operand_count(self.known_frame_stack_height() - count - 1, "a frame", "slots", write)?;
            self.emit(Inst::StoreTempPop(first_key), write);
            for _ in 1..count {
                self.emit(Inst::Pop, write);
            }
        }
        Ok(())
    }

    fn emit_walk_to_container(&mut self, target: &HirId<HirExpr>, safe_access_step_exit: Option<Label>) -> Result<(), anyhow::Error> {
        if let HirExpr::RefValue { holder, .. } = self.hir.get(target) {
            let holder = *holder;
            self.emit_key_or_saved_copy(&holder)?;

            if self.hir.path_has_safe_access(&holder) {
                self.emit_safe_access_step_test(&holder, safe_access_step_exit)?;
            }

            self.emit(ref_load(true), target);
            return Ok(());
        }

        let HirExpr::Index { base: inner, member, is_dot, safe } = self.hir.get(target) else {
            return self.emit_root_for_write(target, safe_access_step_exit);
        };

        let (inner, member, is_dot, safe) = (*inner, *member, *is_dot, *safe);
        self.emit_walk_to_container(&inner, safe_access_step_exit)?;

        if safe {
            self.emit_safe_access_step_test(&inner, safe_access_step_exit)?;
        }

        // The first step off `this` names its member by id, the way an anchor path does.
        match matches!(self.hir.get(&inner), HirExpr::This) {
            true => self.emit_member_key(self.bindings.member(&inner), target)?,
            false => self.emit_key_or_saved_copy(&member)?,
        }
        self.emit(Inst::LoadStepForWrite(is_dot as u8), target);
        Ok(())
    }

    fn emit_root_for_write(&mut self, root: &HirId<HirExpr>, safe_access_step_exit: Option<Label>) -> Result<(), anyhow::Error> {
        match self.hir.get(root) {
            HirExpr::Assert(inner) | HirExpr::Propagate(inner) => {
                let inner = *inner;
                self.emit_walk_to_container(&inner, safe_access_step_exit)?;
                match self.hir.get(root) {
                    HirExpr::Assert(_) => self.emit_assert_check(root, &inner)?,
                    _ => self.emit_propagate_check(&inner)?,
                }
                return self.emit_node_guards(root);
            },
            _ => {},
        }
        if let Some(Place::Capture(_)) = self.bindings.place_of(root) {
            compiler_error!(self, root, "a write through a capture is refused in the check pass");
        }
        if let Some(Place::Local(slot)) = self.bindings.place_of(root) {
            let inst = match self.bindings.anchor_binding(root) {
                Some(_) => {
                    self.emit_binding_root_check(root)?;
                    Inst::LoadAnchorForWrite(slot)
                },
                None => Inst::LoadLocalForWrite(slot),
            };
            self.emit(inst, root);
            return Ok(());
        }
        self.expression(root)
    }

    fn emit_step_key(&mut self, root: &HirId<HirExpr>, at: &AccessStep, first: bool, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // The first step off `this` names its member by id.
        if !(first && matches!(self.hir.get(root), HirExpr::This)) {
            return self.expression(&at.key);
        }
        self.emit_member_key(self.bindings.member(root), node)
    }

    pub(super) fn anchor_survives_tail_call(&self, arg: &HirId<HirExpr>) -> bool {
        !matches!(self.hir.get(arg), HirExpr::Anchor(_)) || self.forwarded_anchor_slot(arg).is_some()
    }

    /// The slot holding the original anchor when `arg` is `&x` and `x` is an anchor parameter.
    fn forwarded_anchor_slot(&self, arg: &HirId<HirExpr>) -> Option<u8> {
        let HirExpr::Anchor(path) = self.hir.get(arg) else { return None };
        let Some(Place::Local(slot)) = self.bindings.place_of(path) else { return None };
        self.anchor_params.iter().find(|param| param.value_slot == slot).map(|param| param.anchor_slot)
    }

    fn emit_load(&mut self, place: Place, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => match self.bindings.anchor_binding(node) {
                Some(_) => {
                    self.emit_binding_root_check(node)?;
                    self.emit(Inst::LoadAnchor(slot), node);
                },
                None => self.emit(Inst::LoadLocal(slot), node),
            },
            Place::Capture(idx) => self.emit(Inst::LoadCapture(idx), node),
            Place::Global(symbol) => {
                let name = self.gc.intern(self.hir.text(symbol));
                let idx = self.ir.add_constant(Value::from(name))?;
                self.emit(Inst::LoadGlobal(idx), node);
            },
        }
        Ok(())
    }

    fn emit_store(&mut self, place: Place, discarded: bool, node: &HirId<HirExpr>, value: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) if self.bindings.anchor_binding(node).is_some() => {
                self.emit_binding_root_check(node)?;
                self.emit(Inst::StoreAnchor(slot), node);
                if discarded {
                    self.emit(Inst::Pop, node);
                }
            },
            Place::Local(slot) => {
                self.emit(match (discarded, self.barriers.builds_its_value(value)) {
                    (true, true) => Inst::StoreLocalFreshPop(slot),
                    (true, false) => Inst::StoreLocalPop(slot),
                    (false, true) => Inst::StoreLocalFresh(slot),
                    (false, false) => Inst::StoreLocal(slot),
                }, node);
            },
            Place::Capture(_) => compiler_error!(self, node, "a write to a capture is refused in the check pass"),
            Place::Global(_) => unreachable!("assignment to a global is rejected during resolution"),
        }
        Ok(())
    }

    fn compile_assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>, discarded: bool) -> Result<(), anyhow::Error> {
        self.compile_assign_op(lhs, None, rhs, discarded)
    }

    /// `lhs = rhs` or `lhs op= rhs`.
    fn compile_assign_op(&mut self, lhs: &HirId<HirExpr>, op: Option<BinOp>, rhs: &HirId<HirExpr>, discarded: bool) -> Result<(), anyhow::Error> {
        match self.hir.get(lhs) {
            HirExpr::Identifier(_) => {
                let place = self.place(lhs);
                self.push_assigned_value(lhs, op, rhs, lhs)?;
                self.share_if_marked(lhs, rhs)?;
                self.emit_store(place, discarded, lhs, rhs)
            },
            // `this = v` on `&var this`
            HirExpr::This => {
                let place = self.place(lhs);
                self.push_assigned_value(lhs, op, rhs, lhs)?;
                self.emit_store(place, discarded, lhs, rhs)
            },
            HirExpr::Index { base, member, is_dot, .. } => {
                let (obj, member, is_dot) = (*base, *member, *is_dot);
                let index_op = IndexOp::Assign { write: *lhs, op, rhs: *rhs, discarded };
                self.index(&obj, &member, is_dot, index_op)
            },
            HirExpr::RefValue { holder, .. } => {
                let holder = *holder;

                let skipped = self.hir.path_has_safe_access(&holder).then(|| self.ir.new_label());
                match skipped {
                    Some(skipped) => self.emit_access_step_operand(&holder, skipped)?,
                    None => self.expression(&holder)?,
                }

                if let Some(op) = op {
                    self.emit(Inst::Dup, lhs);
                    self.emit(Inst::LoadRef, lhs);
                    self.emit_compound_assign_value(op, rhs, lhs)?;
                } else {
                    self.expression(rhs)?;
                }

                self.emit(if discarded { Inst::StoreRefPop } else { Inst::StoreRef }, lhs);

                if let Some(skipped) = skipped {
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), lhs);
                    self.bind_label(skipped);
                    if discarded {
                        self.emit(Inst::Pop, lhs);
                    }
                    self.bind_label(end);
                }

                Ok(())
            },
            _ => compiler_error!(self, lhs, "Invalid assignment")
        }
    }

    fn binary_expression(&mut self, op: BinOp, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if op.yields_an_operand() {
            return self.logical_expression(op, left, right);
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(binop_inst(op), right);
        Ok(())
    }

    fn logical_expression(&mut self, op: BinOp, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        let short_circuit = match op {
            BinOp::And => Inst::JumpIfFalseOrPop(end),
            BinOp::Or => Inst::JumpIfTrueOrPop(end),
            _ => unreachable!("logical_expression called with a non-logical operator"),
        };
        self.emit(short_circuit, left);
        self.expression(right)?;
        self.bind_label(end);
        Ok(())
    }

    fn index(&mut self, target: &HirId<HirExpr>, member_expr_id: &HirId<HirExpr>, is_dot: bool, op: IndexOp) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(target), HirExpr::This) {
            let member_id = self.bindings.member(target);
            return self.index_member_by_id(target, member_id, op);
        }

        let member_name = self.member_name_constant(member_expr_id, is_dot)?;
        match op {
            IndexOp::Load => {
                self.expression(target)?;
                match member_name {
                    Some(name) => self.emit(Inst::GetMember(name), target),
                    None => {
                        self.emit_key_or_saved_copy(member_expr_id)?;
                        self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
                    },
                }
            },
            IndexOp::Assign { write, op, rhs, discarded } => self.compile_path_write(&write, member_name, op, &rhs, discarded)?,
        }
        Ok(())
    }

    /// Compiles a write through a path, such as `a[i].b = v`. A `?.` on the path skips the store
    /// when it finds a witness.
    fn compile_path_write(&mut self, write: &HirId<HirExpr>, member_name: Option<u8>, op: Option<BinOp>, rhs: &HirId<HirExpr>, discarded: bool) -> Result<(), anyhow::Error> {
        let HirExpr::Index { base: target, member, is_dot, safe } = self.hir.get(write) else {
            compiler_error!(self, write, "a walked write ends in a member");
        };

        let (target, member, is_dot, safe) = (*target, *member, *is_dot, *safe);

        let AssignStrategy::WalkLast { saved_write_path_keys, .. } = self.barriers.assign_strategy(write) else {
            compiler_error!(self, &target, "a write into a value no binding holds should be refused");
        };

        let entry = self.known_frame_stack_height();
        let checked = self.hir.safe_access_checked_steps(write).first().copied();
        let exits = checked.map(|_| (self.ir.new_label(), self.ir.new_label()));

        // The keys the checked step reads come before the check, so each runs once.
        let checked_keys = checked.map(|checked| self.hir.path_keys(&checked)).unwrap_or_default();
        let (before, after): (Vec<_>, Vec<_>) = saved_write_path_keys.iter().partition(|key| checked_keys.contains(key));
        self.save_write_path_keys(write, &before)?;

        if let (Some(checked), Some((skipped, _))) = (checked, exits) {
            self.emit_safe_step_operand(&checked, true, skipped)?;
            self.emit(Inst::Pop, &checked);
        }

        self.save_write_path_keys(write, &after)?;

        // A member named by a literal travels in the store's operand, not on the stack.
        let key = match member_name {
            Some(_) => None,
            None => Some(self.push_write_path_key(write, &member)?),
        };

        // An update reads the old value through the key already pushed.
        if let Some(key) = key {
            self.saved_write_path_keys.insert(member, key);
        }
        self.push_assigned_value(write, op, rhs, &target)?;
        self.saved_write_path_keys.remove(&member);

        self.share_if_marked(write, rhs)?;
        let walk_exit = exits.map(|(_, skipped_in_walk)| skipped_in_walk);
        self.emit_walk_to_container(&target, walk_exit)?;

        if safe {
            self.emit_safe_access_step_test(&target, walk_exit)?;
        }

        let store = match member_name {
            Some(name) => Inst::SetMember(name),
            None if is_dot => Inst::SetProperty,
            None => Inst::SetIndex,
        };

        self.emit_store_inst(store, &target, rhs);
        self.drop_saved_write_path_keys(write, saved_write_path_keys, discarded)?;

        // A failed check leaves what it checked on top of whatever the write had pushed by then.
        if let Some((skipped, skipped_in_walk)) = exits {
            let end = self.ir.new_label();
            for exit in [skipped_in_walk, skipped] {
                self.emit(Inst::Jump(end), write);
                self.bind_label(exit);
                let pushed = self.known_frame_stack_height() - entry - 1;
                self.drop_under_top(pushed, discarded, write)?;
            }
            self.bind_label(end);
        }

        Ok(())
    }

    /// Checks a safe access the walk passes, leaving by the write's exit when it fails.
    fn emit_safe_access_step_test(&mut self, at: &HirId<HirExpr>, safe_access_step_exit: Option<Label>) -> Result<(), anyhow::Error> {
        let Some(exit) = safe_access_step_exit else {
            compiler_error!(self, at, "a safe access step outside a write through one");
        };
        self.emit_safe_access_test(at, exit);
        Ok(())
    }

    fn member_name_constant(&mut self, member: &HirId<HirExpr>, is_dot: bool) -> Result<Option<u8>, anyhow::Error> {
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return Ok(None) };
        if !is_dot || self.saved_write_path_keys.contains_key(member) {
            return Ok(None);
        }
        let name = self.gc.intern(name.clone());
        Ok(Some(self.ir.add_constant(Value::from(name))?))
    }

    fn emit_compound_assign_value(&mut self, op: BinOp, rhs: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !op.yields_an_operand() {
            self.expression(rhs)?;
            self.emit(binop_inst(op), node);
            return Ok(());
        }
        let end = self.ir.new_label();
        self.emit(match op {
            BinOp::And => Inst::JumpIfFalseOrPop(end),
            _ => Inst::JumpIfTrueOrPop(end),
        }, node);
        self.expression(rhs)?;
        self.bind_label(end);
        Ok(())
    }

    fn push_assigned_value(&mut self, write: &HirId<HirExpr>, op: Option<BinOp>, rhs: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match op {
            Some(op) => {
                self.expression(write)?;
                self.emit_compound_assign_value(op, rhs, node)
            },
            None => self.expression(rhs),
        }
    }

    fn index_member_by_id(&mut self, target_expr: &HirId<HirExpr>, member_id: u8, op: IndexOp) -> Result<(), anyhow::Error> {
        match op {
            IndexOp::Load => {
                self.expression(target_expr)?;
                self.emit(Inst::GetField(member_id), target_expr);
            },
            IndexOp::Assign { write, op, rhs, discarded } => {
                self.push_assigned_value(&write, op, &rhs, target_expr)?;
                self.share_if_marked(&write, &rhs)?;
                self.emit_root_for_write(target_expr, None)?;
                self.emit_field_store(target_expr, member_id, &rhs, discarded);
            },
        }
        Ok(())
    }

    fn emit_field_store(&mut self, target_expr: &HirId<HirExpr>, member_id: u8, value: &HirId<HirExpr>, discarded: bool) {
        self.emit_store_inst(match discarded {
            true => Inst::SetFieldPop(member_id),
            false => Inst::SetField(member_id),
        }, target_expr, value);
    }

    /// Compiles a call, and says whether it became a tail call.
    pub(super) fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], tail: bool) -> Result<bool, anyhow::Error> {
        let wants = self.call_wants_result();
        // There's no tail form of INVOKE yet.
        if self.invoke_expression(callee, args, wants, false)? {
            return Ok(false);
        }

        let call_form = match tail {
            true => CallForm::Tail,
            false => CallForm::Plain,
        };
        self.with_safe_access_exit(self.hir.path_has_safe_access(callee), |c, end| {
            match end {
                Some(end) => c.emit_access_step_operand(callee, end)?,
                None => c.expression(callee)?,
            }
            c.emit_call(args, callee, wants, call_form)
        })?;
        Ok(tail)
    }

    /// Compiles a call to a member as one invoke, and says whether the callee had that shape.
    fn invoke_expression(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], wants: bool, tested: bool) -> Result<bool, anyhow::Error> {
        let receiver = InvokeReceiver { passes_anchor: self.hir.passes_anchor_receiver(callee), tested };

        // `this.m()` resolves its member here. It carries a root that a plain call cannot.
        if let Some((target, this)) = self.as_this_invoke(callee) {
            let member_id = self.bindings.member(&this);
            self.expression(&target)?;
            self.emit_call(args, callee, wants, CallForm::InvokeThis { member: member_id, receiver })?;
            return Ok(true);
        }

        // Fuse `recv.name(args)` into a single INVOKE.
        let Some((target, name, is_dot, safe)) = self.as_method_invoke(callee) else { return Ok(false) };
        let name_ref = self.gc.intern(name);
        let idx = self.ir.add_constant(Value::from(name_ref))?;
        let form = CallForm::Invoke { name: idx, is_dot: is_dot as u8, receiver };
        self.with_safe_access_exit(self.hir.path_has_safe_access(callee), |c, end| {
            match (c.hir.get(&target), end) {
                (HirExpr::Anchor(path), _) => c.emit_anchor_of(path, &target, end, safe.then_some(callee))?,
                (_, Some(end)) => c.emit_safe_step_operand(&target, safe, end)?,
                (_, None) => c.expression(&target)?,
            }
            c.emit_call(args, callee, wants, form)
        })?;
        Ok(true)
    }

    /// Emits `body` with an exit label for its safe accesses, when it has any.
    fn with_safe_access_exit(&mut self, has_safe_access: bool, body: impl FnOnce(&mut Self, Option<Label>) -> Result<(), anyhow::Error>) -> Result<(), anyhow::Error> {
        let end = has_safe_access.then(|| self.ir.new_label());
        body(self, end)?;
        if let Some(end) = end {
            self.bind_label(end);
        }
        Ok(())
    }

    fn call_wants_result(&mut self) -> bool {
        !std::mem::take(&mut self.next_call_discards_result)
    }

    pub(super) fn discard_result_of(&mut self, expr: &HirId<HirExpr>) {
        self.next_call_discards_result = matches!(self.hir.get(expr), HirExpr::Call(..) | HirExpr::SafeCall(..));
    }

    fn emit_call(&mut self, args: &[HirId<HirExpr>], node: &HirId<HirExpr>, wants: bool, form: CallForm) -> Result<(), anyhow::Error> {
        let tail = matches!(form, CallForm::Tail);
        for arg in args {
            match tail.then(|| self.forwarded_anchor_slot(arg)).flatten() {
                Some(slot) => self.emit(Inst::LoadLocal(slot), arg),
                None => self.expression(arg)?,
            }
        }
        if args.len() > u8::MAX as usize {
            compiler_error!(self, node, "a call takes at most 255 arguments");
        }
        let passed = self.hir.call_anchors(node, args);
        self.emit_call_root_checks(&passed)?;
        self.emit_anchor_overlap_checks(&passed, node)?;
        let mut proofs = 0;

        // Under check-forcing a call checks what the pass proved, so a refusal refutes the proof.
        if self.barriers.args_settled(node) && !self.force_checks {
            proofs |= ir::CALL_ARGS_SETTLED;
        }
        if self.barriers.arg_kinds_proven(node) && !self.force_checks {
            proofs |= ir::CALL_KINDS_PROVEN;
        }

        if tail {
            self.emit_anchor_copy_outs(node);
        }

        let at = self.ir.next_index();
        self.emit(form.build_inst(proofs, args.len() as u8, wants), node);

        if self.barriers.args_settled(node) && self.force_checks {
            self.ir.mark_forced_from(at);
        }

        Ok(())
    }

    fn emit_anchor_overlap_checks(&mut self, args: &[HirId<HirExpr>], node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        for pair in anchors::path_overlaps(self.hir, self.bindings, args) {
            let apart = self.ir.new_label();
            for (x, y) in pair.maybe_overlapping_keys {
                self.emit(Inst::LoadLocal(self.key_slot(&x)), node);
                self.emit(Inst::LoadLocal(self.key_slot(&y)), node);
                self.emit(Inst::Equal, node);
                self.emit(Inst::JumpIfFalse(apart), node);
            }
            self.emit_throw_message("two anchors in one call name the same storage".to_string(), node)?;
            self.bind_label(apart);
        }
        Ok(())
    }

    fn key_slot(&self, key: &AnchorKey) -> u8 {
        self.bindings.anchor_path_slots(&key.anchor)
            .expect("a path anchor reserved its slots")
            .key(key.step)
    }

    /// The receiver of `this.m(args)` or `&this.m(args)`, and the `this` inside it.
    fn as_this_invoke(&self, callee: &HirId<HirExpr>) -> Option<(HirId<HirExpr>, HirId<HirExpr>)> {
        let HirExpr::Index { base: target, .. } = self.hir.get(callee) else { return None };
        self.hir.as_this(target).map(|this| (*target, this))
    }

    fn as_method_invoke(&self, callee: &HirId<HirExpr>) -> Option<(HirId<HirExpr>, String, bool, bool)> {
        let HirExpr::Index { base: target, member, is_dot, safe } = self.hir.get(callee) else { return None };
        if self.hir.as_this(target).is_some() {
            return None;
        }
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return None };
        Some((*target, name.clone(), *is_dot, *safe))
    }

    fn lambda(&mut self, expr: &HirId<HirExpr>, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, (*expr).into(), decl, kind)?;
        self.emit(Inst::BuildClosure(const_idx), expr);
        return Ok(());
    }

    pub (super) fn literal(&mut self, expr: &HirId<HirExpr>, literal: &HirLiteral) -> Result<(), anyhow::Error> {
        match literal {
            HirLiteral::Number(num) => {
                let idx = self.ir.add_constant(Value::from(*num))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            HirLiteral::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.ir.add_constant(Value::from(str))?;
                self.emit(Inst::PushConstant(idx), expr);
            },
            HirLiteral::Null => { self.emit(Inst::PushNull, expr); },
            HirLiteral::Boolean(true) => { self.emit(Inst::PushTrue, expr); },
            HirLiteral::Boolean(false) => { self.emit(Inst::PushFalse, expr); },
            HirLiteral::Array(elements) => {
                let count = self.operand_count(elements.len(), "an array literal", "elements", expr)?;
                for element in elements {
                    self.expression(element)?;
                }
                self.emit(Inst::Array(count), expr);
            },
            HirLiteral::Dict(pairs) => {
                let count = self.operand_count(pairs.len(), "a dict literal", "entries", expr)?;
                for (key, value) in pairs {
                    self.expression(key)?;
                    self.expression(value)?;
                }
                self.emit(Inst::Dict(count), expr);
            },
            HirLiteral::Lambda(decl) => self.lambda(expr, decl, FnKind::Function)?
        };

        return Ok(());
    }
}

fn binop_inst(op: BinOp) -> Inst {
    match op {
        BinOp::Add => Inst::Add,
        BinOp::Subtract => Inst::Subtract,
        BinOp::Multiply => Inst::Multiply,
        BinOp::Divide => Inst::Divide,
        BinOp::LeftShift => Inst::LeftShift,
        BinOp::RightShift => Inst::RightShift,
        BinOp::LessThan => Inst::LessThan,
        BinOp::LessThanEqual => Inst::LessThanEqual,
        BinOp::GreaterThan => Inst::GreaterThan,
        BinOp::GreaterThanEqual => Inst::GreaterThanEqual,
        BinOp::Equal => Inst::Equal,
        BinOp::NotEqual => Inst::NotEqual,
        BinOp::And | BinOp::Or => unreachable!("logical ops compile to short-circuit branches"),
        BinOp::BitAnd => Inst::BitAnd,
        BinOp::BitOr => Inst::BitOr,
        BinOp::BitXor => Inst::BitXor,
    }
}

fn unop_inst(op: UnOp) -> Inst {
    match op {
        UnOp::Negate => Inst::Negate,
        UnOp::BitNot => Inst::BitNot,
        UnOp::Not => Inst::Not,
    }
}

fn access_inst(is_dot: bool) -> Inst {
    match is_dot {
        true => Inst::GetProperty,
        false => Inst::GetIndex,
    }
}

fn safe_access_exit(end: Option<Label>) -> Label {
    end.expect("only a method call's receiver is anchored through `?`, and it has an exit label")
}
