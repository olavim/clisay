use crate::compiler_error;
use crate::core::value::Value;
use crate::middle::hir::{BinOp, HirExpr, HirFnDecl, HirId, HirLiteral, Symbol, TypeId, UnOp};
use crate::middle::ir::{Inst, Label, WRITE_ROOT_LOCAL, WRITE_ROOT_NONE, WRITE_ROOT_RECEIVER, WRITE_ROOT_RECEIVER_UP, WRITE_ROOT_STASH, WRITE_ROOT_UNSHARED, WRITE_ROOT_UPVALUE};
use crate::middle::bind::{FnKind, Place, Receiver};
use crate::middle::check::{Barrier, Guard, WitnessSet};

use super::{Compiler, WriteOwnershipHolderPlace, PathRoot};

#[derive(Clone, Copy)]
enum IndexOp {
    Load,
    Store { rhs: HirId<HirExpr>, discarded: bool },
    Update { op: BinOp, rhs: HirId<HirExpr>, discarded: bool },
}

impl<'a> Compiler<'a> {
    pub (super) fn expression(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let before = self.frame_slot_count;
        self.expression_inner(expr)?;
        // An expression leaves exactly one value.
        self.frame_slot_count = before + 1;
        Ok(())
    }

    fn expression_inner(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr)?,
            HirExpr::Unary(op, operand) => self.unary_expression(*op, operand)?,
            HirExpr::Binary(op, left, right) => self.binary_expression(*op, left, right)?,
            HirExpr::Assign(left, right) => self.compile_assign(left, right, false)?,
            HirExpr::CompoundAssign(target, op, value) => self.compile_assign_op(target, Some(*op), value, false)?,
            HirExpr::Call(callee, args) => self.call_expression(callee, args, false, false)?,
            HirExpr::Index(target, member, is_dot) => self.index(target, member, *is_dot, IndexOp::Load)?,
            HirExpr::Literal(lit) => self.literal(expr, lit)?,
            HirExpr::Identifier(_) => {
                let place = self.place(expr);
                self.emit_load(place, expr)?;
            },
            // A plain brace seals inline (seal flag 1).
            HirExpr::Construct(callee, brace) => self.construct_expression(expr, callee, brace, 1)?,
            HirExpr::Mut(inner) => self.mut_expression(expr, inner)?,
            HirExpr::Match(scrutinee, matcher) => {
                self.expression(scrutinee)?;
                match self.bindings.match_binders(expr) {
                    Some(binders) => self.compile_binding_matcher(matcher, binders, expr)?,
                    None => self.compile_matcher_test(matcher, expr)?,
                }
            },
            HirExpr::This => self.emit_load(self.place(expr), expr)?,
            HirExpr::Coalesce(left, right) => self.coalesce(expr, left, right)?,
            HirExpr::SafeAccess(target, member, is_dot) => self.safe_access(expr, target, member, *is_dot)?,
            HirExpr::SafeCall(callee, args) => self.safe_call(expr, callee, args)?,
            HirExpr::Propagate(operand) => self.propagate(expr, operand)?,
            HirExpr::Handle(left, _, handler) => self.handle(expr, left, handler)?,
            HirExpr::Assert(operand) => self.assert(expr, operand)?,
        };

        // Indexing consumes the root. The store receives the root while it is still on the stack.
        if self.stash_root == Some(*expr) {
            self.stash_root = None;
            self.emit(Inst::StashRoot, expr);
        }

        // Each runtime check this node carries, in the order the check pass put them in.
        for guard in self.barriers.guards(expr) {
            self.emit_guard(expr, *guard)?;
        }

        // Under check-forcing, the checks the pass proved unnecessary are emitted too.
        for guard in self.barriers.elided(expr) {
            let at = self.ir.next_index();
            self.emit_guard(expr, *guard)?;
            self.ir.mark_elisions_from(at);
        }
        Ok(())
    }

    fn mark_path_root(&mut self, target: &HirId<HirExpr>) {
        let (root, kind) = self.path_root(target);
        // A node that is its own root would be compared against itself. The move rule keeps a
        // container from ever holding itself, so the store asks a different question for it.
        if kind == PathRoot::Unnamed && root != *target {
            debug_assert!(self.stash_root.is_none(), "a path root was marked and never stashed");
            self.stash_root = Some(root);
        }
    }

    fn path_root(&self, node: &HirId<HirExpr>) -> (HirId<HirExpr>, PathRoot) {
        let mut current = *node;
        loop {
            match self.hir.get(&current) {
                HirExpr::Index(target, _, _) | HirExpr::SafeAccess(target, _, _) => current = *target,
                HirExpr::Assert(inner) | HirExpr::Propagate(inner) | HirExpr::Mut(inner) => current = *inner,
                HirExpr::Identifier(_) | HirExpr::This => return (current, match self.place(&current) {
                    Place::Local(slot) => PathRoot::Local(slot),
                    Place::Upvalue(idx) => PathRoot::Upvalue(idx),
                    _ => PathRoot::Unnamed,
                }),
                _ => return (current, PathRoot::Unnamed),
            }
        }
    }

    fn root_holder(&self, node: &HirId<HirExpr>) -> Option<WriteOwnershipHolderPlace> {
        match self.path_root(node).1 {
            PathRoot::Local(slot) => Some(WriteOwnershipHolderPlace::Local(slot)),
            PathRoot::Upvalue(idx) => Some(WriteOwnershipHolderPlace::Upvalue(idx)),
            PathRoot::Unnamed => None,
        }
    }

    fn receiver_root_operands(receiver: Receiver) -> (u8, u8) {
        match receiver {
            Receiver::Slot => (WRITE_ROOT_RECEIVER, 0),
            Receiver::Upvalue(idx) => (WRITE_ROOT_RECEIVER_UP, idx),
        }
    }

    fn write_root_operands(&self, node: &HirId<HirExpr>) -> (u8, u8) {
        let (kind, operand) = self.root_operands(node);
        match self.barriers.store_is_unshared(node) {
            true => (kind | WRITE_ROOT_UNSHARED, operand),
            false => (kind, operand),
        }
    }

    fn root_operands(&self, node: &HirId<HirExpr>) -> (u8, u8) {
        let (root, place) = self.path_root(node);
        // A path rooted in `this` reaches through the receiver.
        let receiver = matches!(self.hir.get(&root), HirExpr::This);
        match place {
            PathRoot::Local(slot) if receiver => (WRITE_ROOT_RECEIVER, slot),
            PathRoot::Upvalue(idx) if receiver => (WRITE_ROOT_RECEIVER_UP, idx),
            PathRoot::Local(slot) => (WRITE_ROOT_LOCAL, slot),
            PathRoot::Upvalue(idx) => (WRITE_ROOT_UPVALUE, idx),
            // The path consumed a root and it is on the stash. A path that is its own root
            // reaches the target directly.
            PathRoot::Unnamed if root != *node => (WRITE_ROOT_STASH, 0),
            PathRoot::Unnamed => (WRITE_ROOT_NONE, 0),
        }
    }

    fn emit_guard(&mut self, node: &HirId<HirExpr>, guard: Guard) -> Result<(), anyhow::Error> {
        // `!` asks for its check in the source, so it's the operation rather than a safety guard.
        if self.drop_guards && guard != Guard::NonNull {
            return Ok(());
        }
        match guard {
            Guard::Boundary => if let Some(barrier) = self.barriers.boundary(node) {
                self.emit_boundary_barrier(node, barrier)?;
            },
            Guard::NonNull => self.emit(Inst::AssertNonNull, node),
            // The receiving container takes the element's writer slot, so the aggregate it came
            // from keeps reading it and stops writing it.
            Guard::StoreIntoContainer => match self.receiving_slot {
                Some(WriteOwnershipHolderPlace::Local(slot)) => self.emit(Inst::TransferWriteOwnership(slot), node),
                Some(WriteOwnershipHolderPlace::Upvalue(idx)) => self.emit(Inst::TransferWriteOwnershipUp(idx), node),
                Some(WriteOwnershipHolderPlace::Stack(depth)) => self.emit(Inst::TransferWriteOwnershipAt(depth), node),
                None => {},
            },
            Guard::Immutable => self.emit(Inst::AssertImmutable, node),
        }
        Ok(())
    }

    fn emit_boundary_barrier(&mut self, node: &HirId<HirExpr>, barrier: &Barrier) -> Result<(), anyhow::Error> {
        let allow = self.accepted_witness_set(&barrier.allow_witnesses, barrier.null_allowed);
        let idx = self.ir.add_witness_allow(allow)?;
        self.emit(Inst::BarrierGuard(idx), node);
        Ok(())
    }

    fn emit_is_jumps(&mut self, node: &HirId<HirExpr>, witnesses: &[TypeId], target: Label) {
        for &id in witnesses {
            self.emit(Inst::JumpIfIs(target, id), node);
        }
    }

    fn emit_witness_jumps(&mut self, node: &HirId<HirExpr>, set: &WitnessSet, target: Label) -> Result<(), anyhow::Error> {
        if set.null {
            self.emit(Inst::JumpIfNull(target), node);
        }
        self.emit_is_jumps(node, &set.witnesses, target);
        Ok(())
    }

    /// Compiles `a ?? b`.
    fn coalesce(&mut self, node: &HirId<HirExpr>, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        match self.barriers.witness_set(node) {
            Some(set) if set.contains_user_witnesses => {
                let fallback = self.ir.new_label();
                self.emit_witness_jumps(left, set, fallback)?;
                self.emit(Inst::Jump(end), left);
                self.ir.bind(fallback);
                self.emit(Inst::Pop, left);
            },
            Some(_) => {
                self.emit(Inst::JumpIfClean(end), left);
                self.emit(Inst::Pop, left);
            },
            None => {
                self.emit(Inst::JumpIfNotNullOrPop(end), left);
            },
        }
        self.expression(right)?;
        self.ir.bind(end);
        Ok(())
    }

    /// Compiles `cb?(args)`.
    fn safe_call(&mut self, node: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(callee)?;
        self.chain_guard(node, callee, end)?;
        for arg in args {
            self.expression(arg)?;
        }
        self.emit(Inst::Call(args.len() as u8), callee);
        self.ir.bind(end);
        Ok(())
    }

    fn emit_clean_jump(&mut self, node: &HirId<HirExpr>, at: &HirId<HirExpr>, clean: Label) -> Result<(), anyhow::Error> {
        match self.barriers.witness_set(node) {
            // A user witness is a type test, so its jumps fire on a bad value. That leaves
            // `clean` needing a jump of its own.
            Some(set) if set.contains_user_witnesses => {
                let bad = self.ir.new_label();
                self.emit_witness_jumps(at, set, bad)?;
                self.emit(Inst::Jump(clean), at);
                self.ir.bind(bad);
            },
            _ => self.emit(Inst::JumpIfClean(clean), at),
        }
        Ok(())
    }

    /// Compiles `a?!`.
    fn propagate(&mut self, node: &HirId<HirExpr>, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let cont = self.ir.new_label();
        self.expression(operand)?;
        self.emit_clean_jump(node, operand, cont)?;
        self.emit_defers_over_return_value(operand)?;
        self.emit(Inst::Return, operand);
        self.ir.bind(cont);
        Ok(())
    }

    /// Compiles `a!`.
    fn assert(&mut self, node: &HirId<HirExpr>, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(operand)?;
        let Some(set) = self.barriers.witness_set(node) else { return Ok(()) };
        let throw_it = self.ir.new_label();
        let skip = self.ir.new_label();
        self.emit_is_jumps(operand, &set.witnesses, throw_it);

        if set.null {
            self.emit(Inst::AssertNonNull, operand);
        }

        self.emit(Inst::Jump(skip), operand);
        self.ir.bind(throw_it);
        self.emit(Inst::Throw, operand);
        self.ir.bind(skip);
        Ok(())
    }

    /// Compiles `a ?? p => h`.
    fn handle(&mut self, node: &HirId<HirExpr>, left: &HirId<HirExpr>, handler: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(left)?;
        let operand_slot = self.operand_count(self.frame_slot_count - 1, "a frame", "slots", node)?;
        self.emit_clean_jump(node, left, end)?;
        self.handle_binder_slots.push((self.bindings.handle_binder(node), operand_slot));
        let compiled = self.expression(handler);
        self.handle_binder_slots.pop();
        compiled?;
        // An upvalue over the binder has to be closed before the result lands on its slot.
        if self.bindings.is_captured(node.index()) {
            self.emit(Inst::CloseSlotUpvalue(operand_slot), node);
        }
        self.emit(Inst::StoreLocalPop(operand_slot), node);
        self.ir.bind(end);
        Ok(())
    }

    /// Compiles `a?.b` / `a?[i]`.
    fn safe_access(&mut self, node: &HirId<HirExpr>, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool) -> Result<(), anyhow::Error> {
        let end = self.ir.new_label();
        self.expression(target)?;
        self.chain_guard(node, target, end)?;
        self.expression(member)?;
        self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
        self.ir.bind(end);
        Ok(())
    }

    fn chain_guard(&mut self, node: &HirId<HirExpr>, at: &HirId<HirExpr>, end: Label) -> Result<(), anyhow::Error> {
        match self.barriers.witness_set(node) {
            Some(set) if set.contains_user_witnesses => self.emit_witness_jumps(at, set, end)?,
            Some(_) => self.emit(Inst::JumpIfBad(end), at),
            None => self.emit(Inst::JumpIfNull(end), at),
        }
        Ok(())
    }

    pub (super) fn expression_stmt(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr),
            // An assignment statement stores in discard context, so the store op itself drops the value.
            HirExpr::Assign(left, right) => self.compile_assign(left, right, true),
            HirExpr::CompoundAssign(target, op, value) => self.compile_assign_op(target, Some(*op), value, true),
            // Any other expression leaves a value that the statement discards.
            _ => {
                self.expression(expr)?;
                self.emit(Inst::Pop, expr);
                Ok(())
            }
        }
    }

    fn unary_expression(&mut self, op: UnOp, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(unop_inst(op), expr);
        Ok(())
    }

    fn construct_expression(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, brace: &[(Symbol, HirId<HirExpr>)], seal: u8) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
        for (_, value) in brace {
            self.expression(value)?;
        }
        let field_ids = self.bindings.construct_fields(expr).to_vec();
        let fields_idx = self.ir.add_construct_fields(field_ids)?;
        self.emit(Inst::Construct(fields_idx, seal), expr);
        Ok(())
    }

    fn mut_expression(&mut self, expr: &HirId<HirExpr>, inner: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(inner) {
            // `mut K{..}` builds the brace unsealed (seal flag 0), so it stays mutable.
            HirExpr::Construct(callee, brace) => return self.construct_expression(inner, callee, brace, 0),
            // `mut [..]` and `mut {..}` build unsealed.
            HirExpr::Literal(literal @ (HirLiteral::Array(_) | HirLiteral::Dict(_))) => {
                return self.container_literal(inner, literal, 0);
            },
            // `mut K(..)` is a factory call left unsealed via CALL_MUT.
            HirExpr::Call(callee, args) if self.barriers.is_construction(inner) => {
                return self.call_expression(callee, args, true, false);
            },
            _ => {},
        }
        self.expression(inner)?;
        self.emit(Inst::Mut, expr);
        Ok(())
    }

    fn emit_receiver(&mut self, receiver: Receiver, node: &HirId<HirExpr>) {
        match receiver {
            Receiver::Slot => self.emit(Inst::LoadLocal(0), node),
            Receiver::Upvalue(idx) => self.emit(Inst::LoadUpvalue(idx), node),
        }
    }

    fn emit_load(&mut self, place: Place, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match place {
            Place::Local(slot) => self.emit(Inst::LoadLocal(slot), node),
            Place::Upvalue(idx) => self.emit(Inst::LoadUpvalue(idx), node),
            Place::Field(id, receiver) => {
                self.emit_receiver(receiver, node);
                self.emit(Inst::GetField(id), node);
            },
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
            Place::Local(slot) => {
                self.emit(if discarded { Inst::StoreLocalPop(slot) } else { Inst::StoreLocal(slot) }, node);
            },
            Place::Upvalue(idx) => {
                let store = if discarded { Inst::StoreUpvaluePop(idx) } else { Inst::StoreUpvalue(idx) };
                self.emit_store_inst(store, node, value);
            },
            Place::Field(id, receiver) => {
                self.emit_receiver(receiver, node);
                let (kind, operand) = Self::receiver_root_operands(receiver);
                self.emit_store_inst(match discarded {
                    true => Inst::SetFieldPop(id, kind, operand),
                    false => Inst::SetField(id, kind, operand),
                }, node, value);
            },
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
                if let Some(op) = op {
                    self.expression(lhs)?;
                    self.emit_compound_assign_value(op, rhs, lhs)?;
                    return self.emit_store(place, discarded, lhs, rhs);
                }
                self.expression(rhs)?;
                self.emit_store(place, discarded, lhs, rhs)?;
                Ok(())
            },
            HirExpr::Index(obj, member, is_dot) => {
                let (obj, member, is_dot) = (*obj, *member, *is_dot);
                let index_op = match op {
                    Some(op) => IndexOp::Update { op, rhs: *rhs, discarded },
                    None => IndexOp::Store { rhs: *rhs, discarded },
                };
                self.index(&obj, &member, is_dot, index_op)
            },
            _ => compiler_error!(self, lhs, "Invalid assignment")
        }
    }

    fn binary_expression(&mut self, op: BinOp, left: &HirId<HirExpr>, right: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // `&&`/`||` short-circuit and yield an operand, so they compile to a
        // conditional jump rather than a binary op.
        if let BinOp::And | BinOp::Or = op {
            return self.logical_expression(op, left, right);
        }

        // Canonical lowering; `optimize` fuses `local <op> const` forms.
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
        self.ir.bind(end);
        Ok(())
    }

    fn index(&mut self, target: &HirId<HirExpr>, member_expr_id: &HirId<HirExpr>, is_dot: bool, op: IndexOp) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(target), HirExpr::This) {
            let member_id = self.bindings.member(target);
            return self.index_member_by_id(target, member_id, op);
        }

        match op {
            IndexOp::Load => {
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
            },
            IndexOp::Store { rhs, discarded } => {
                self.mark_path_root(target);
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.expression(&rhs)?;
                self.emit_path_store(target, is_dot, &rhs, discarded);
            },
            IndexOp::Update { op, rhs, discarded } => {
                self.mark_path_root(target);
                self.expression(target)?;
                self.expression(member_expr_id)?;
                self.emit(Inst::Dup2, target);
                self.emit(if is_dot { Inst::GetProperty } else { Inst::GetIndex }, target);
                self.emit_compound_assign_value(op, &rhs, target)?;
                self.emit_path_store(target, is_dot, &rhs, discarded);
            },
        }
        Ok(())
    }

    fn emit_compound_assign_value(&mut self, op: BinOp, rhs: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !matches!(op, BinOp::And | BinOp::Or) {
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
        self.ir.bind(end);
        Ok(())
    }

    fn emit_path_store(&mut self, target: &HirId<HirExpr>, is_dot: bool, value: &HirId<HirExpr>, discarded: bool) {
        let (root_kind, root_operand) = self.write_root_operands(target);
        let store = match is_dot {
            true => Inst::SetProperty(root_kind, root_operand),
            false => Inst::SetIndex(root_kind, root_operand),
        };
        self.emit_store_inst(store, target, value);
        if discarded {
            self.emit(Inst::Pop, target);
        }
    }

    fn index_member_by_id(&mut self, target_expr: &HirId<HirExpr>, member_id: u8, op: IndexOp) -> Result<(), anyhow::Error> {
        match op {
            IndexOp::Load => {
                self.expression(target_expr)?;
                self.emit(Inst::GetField(member_id), target_expr);
            },
            IndexOp::Store { rhs, discarded } => {
                self.expression(&rhs)?;
                self.mark_path_root(target_expr);
                self.expression(target_expr)?;
                self.emit_field_store(target_expr, member_id, &rhs, discarded);
            },
            IndexOp::Update { op, rhs, discarded } => {
                self.expression(target_expr)?;
                self.emit(Inst::GetField(member_id), target_expr);
                self.emit_compound_assign_value(op, &rhs, target_expr)?;
                self.emit_load(self.place(target_expr), target_expr)?;
                self.emit_field_store(target_expr, member_id, &rhs, discarded);
            },
        }
        Ok(())
    }

    fn emit_field_store(&mut self, target_expr: &HirId<HirExpr>, member_id: u8, value: &HirId<HirExpr>, discarded: bool) {
        let (kind, operand) = self.write_root_operands(target_expr);
        self.emit_store_inst(match discarded {
            true => Inst::SetFieldPop(member_id, kind, operand),
            false => Inst::SetField(member_id, kind, operand),
        }, target_expr, value);
    }

    pub(super) fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], mutable: bool, tail: bool) -> Result<(), anyhow::Error> {
        // `this.m()` resolves its member here. It carries a root that a plain call cannot.
        if let Some(target) = self.as_this_invoke(callee) {
            let member_id = self.bindings.member(&target);
            self.expression(&target)?;
            let holder = self.root_holder(&target);
            let saved = std::mem::replace(&mut self.receiving_slot, holder);
            let compiled = args.iter().try_for_each(|arg| self.expression(arg));
            self.receiving_slot = saved;
            compiled?;
            let (kind, operand) = self.write_root_operands(&target);
            self.emit(Inst::InvokeThis(member_id, args.len() as u8, kind, operand), callee);
            return Ok(());
        }

        // Fuse `recv.name(args)` into a single INVOKE.
        if let Some((target, name, is_dot)) = self.as_method_invoke(callee) {
            self.mark_path_root(&target);
            self.expression(&target)?;

            let receiver = self.root_holder(&target);
            let saved = std::mem::replace(&mut self.receiving_slot, receiver);
            let compiled = args.iter().enumerate().try_for_each(|(i, arg)| {
                if receiver.is_none() {
                    self.receiving_slot = Some(WriteOwnershipHolderPlace::Stack(i as u8 + 1));
                }
                self.expression(arg)
            });

            self.receiving_slot = saved;
            compiled?;

            let name_ref = self.gc.intern(name);
            let idx = self.ir.add_constant(Value::from(name_ref))?;
            // A method that writes its receiver asks the write-ownership question.
            let (kind, operand) = self.write_root_operands(&target);
            self.emit(Inst::Invoke(idx, args.len() as u8, kind, operand, is_dot as u8), callee);
            return Ok(());
        }

        self.expression(callee)?;

        for arg in args {
            self.expression(arg)?;
        }

        // An opaque call that must keep an argument asserts the callee borrows it, not consumes it.
        if let Some(positions) = self.barriers.survive(callee).filter(|_| !self.drop_guards) {
            let entries = positions.iter()
                .map(|&(p, _)| (p, self.hir.pos(&args[p as usize]).clone()))
                .collect();
            // The obligation rides along so the failure can name what the caller still has to do.
            let owed: Box<[_]> = positions.iter()
                .filter_map(|&(p, owed)| owed.map(|o| (p, self.hir.text(o).into())))
                .collect();
            let owed_idx = self.ir.add_owed_names(owed)?;
            let idx = self.ir.add_survive_positions(entries)?;
            self.emit(Inst::AssertNoRetain(args.len() as u8, owed_idx, idx), callee);
        }

        let n = args.len() as u8;
        self.emit(match (tail, mutable) {
            (true, _) => Inst::TailCall(n),
            (false, true) => Inst::CallMut(n),
            (false, false) => Inst::Call(n),
        }, callee);

        Ok(())
    }

    fn as_this_invoke(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirExpr>> {
        let HirExpr::Index(target, _, _) = self.hir.get(callee) else { return None };
        matches!(self.hir.get(target), HirExpr::This).then_some(*target)
    }

    fn as_method_invoke(&self, callee: &HirId<HirExpr>) -> Option<(HirId<HirExpr>, String, bool)> {
        let HirExpr::Index(target, member, is_dot) = self.hir.get(callee) else { return None };
        if matches!(self.hir.get(target), HirExpr::This) {
            return None;
        }
        let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) else { return None };
        Some((*target, name.clone(), *is_dot))
    }

    fn lambda(&mut self, expr: &HirId<HirExpr>, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        let const_idx = self.function(expr, (*expr).into(), decl, kind, self.lambda_masks(expr, decl))?;
        self.emit(Inst::PushClosure(const_idx), expr);
        return Ok(());
    }

    /// Emits a runtime check that an immutable container holds no mutable element.
    fn seal_check(&mut self, expr: &HirId<HirExpr>) {
        if self.barriers.needs_seal_check(expr) && !self.drop_guards {
            self.emit(Inst::SealCheck, expr);
        }
    }


    fn container_literal(&mut self, expr: &HirId<HirExpr>, literal: &HirLiteral, seal: u8) -> Result<(), anyhow::Error> {
        match literal {
            HirLiteral::Array(elements) => {
                let count = self.operand_count(elements.len(), "an array literal", "elements", expr)?;
                for element in elements {
                    self.expression(element)?;
                }
                self.emit(Inst::Array(count, seal), expr);
            },
            HirLiteral::Dict(pairs) => {
                let count = self.operand_count(pairs.len(), "a dict literal", "entries", expr)?;
                for (key, value) in pairs {
                    self.expression(key)?;
                    self.expression(value)?;
                }
                self.emit(Inst::Dict(count, seal), expr);
            },
            _ => unreachable!("only an array or dict literal builds a container"),
        }
        if seal != 0 {
            self.seal_check(expr);
        }
        Ok(())
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
            HirLiteral::Array(_) | HirLiteral::Dict(_) => self.container_literal(expr, literal, 1)?,
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
