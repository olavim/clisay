//! Local and capture placement: where each name lives at runtime.

use anyhow::{anyhow, bail};

use fnv::{FnvHashMap, FnvHashSet};

use crate::core::objects::CaptureLocation;
use crate::middle::hir::{access_path_root, access_path_steps, Hir, HirExpr, HirFnDecl, HirId, HirMatcher, HirStmt, Symbol};
use crate::middle::walk::{children_of, visit_body, Child};

use super::{AnchorParam, FnFrame, FnKind, FrameTemps, Local, Place, AnchorPathSlots, Resolver};

/// Each `try` with a `finally` under `node`, with how many such `try`s enclose it.
fn finally_depths(hir: &Hir, node: Child, depth: usize, out: &mut Vec<(HirId<HirStmt>, usize)>) {
    let mut depth = depth;
    if let Child::Stmt(s) = node {
        if matches!(hir.get(&s), HirStmt::Try(_, _, Some(_))) {
            out.push((s, depth));
            depth += 1;
        }
    }
    for child in children_of(hir, node) {
        finally_depths(hir, child, depth, out);
    }
}

#[derive(Default)]
pub(super) struct BodyFacts {
    defers: bool,
    /// Each path anchor in the body, with its path.
    path_anchors: Vec<(HirId<HirExpr>, HirId<HirExpr>)>,
    /// Each local anchor binding, with the name it stands for when bound to a slot anchor.
    anchor_bindings: FnvHashMap<Symbol, Option<Symbol>>,
    /// The names the body may rebind.
    rebound: FnvHashSet<Symbol>,
    this_rebound: bool,
    /// The names the body may write.
    written_roots: FnvHashSet<Symbol>,
}

impl BodyFacts {
    /// Whether a path anchor starting at `root` with `steps` steps may need a slot to save its
    /// root's value.
    fn may_save_root(&self, hir: &Hir, root: &HirId<HirExpr>, steps: usize) -> bool {
        match hir.get(root) {
            HirExpr::Identifier(name) => self.anchor_bindings.contains_key(name) || (steps > 1 && self.may_rebind(*name)),
            _ => steps > 1 && self.this_rebound,
        }
    }

    /// Whether the body may rebind `name`. After `say var b = &x`, a write to `b` rebinds `x` too.
    fn may_rebind(&self, name: Symbol) -> bool {
        let mut names = vec![name];
        let mut at = 0;
        while at < names.len() {
            if self.rebound.contains(&names[at]) {
                return true;
            }
            let aliases: Vec<Symbol> = self.anchor_bindings.iter()
                .filter(|(alias, aliased)| **aliased == Some(names[at]) && !names.contains(alias))
                .map(|(alias, _)| *alias)
                .collect();
            names.extend(aliases);
            at += 1;
        }
        false
    }
}

impl<'a> Resolver<'a> {
    pub(super) fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub(super) fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) {
        // Recorded before the locals go, so codegen can hand the runtime the stack height to expect.
        self.record_exit_frame_stack_height(node_id);
        self.scope_depth -= 1;
        while self.type_scope.last().is_some_and(|t| t.depth > self.scope_depth) {
            let gone = self.type_scope.pop().expect("just checked");
            self.type_index.remove(&gone.name);
        }

        let mut dying = 0;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            dying += 1;
            self.locals.pop();
        }
        if dying > 0 {
            self.bindings.cleanups.insert(node_id.index(), dying);
        }
    }

    fn mark_captured(&mut self, index: usize) {
        self.locals[index].is_captured = true;
        if let Some(decl) = self.locals[index].decl {
            self.bindings.captured.insert(decl);
        }
    }

    fn push_local(&mut self, name: Option<Symbol>, decl: Option<usize>) -> Result<usize, anyhow::Error> {
        if self.locals.len() - self.local_offset() >= u8::MAX as usize {
            return Err(self.too_many_locals());
        }
        self.locals.push(Local { name, depth: self.scope_depth, is_captured: false, decl, anchor: None, function: None });
        Ok(self.locals.len() - 1)
    }

    fn too_many_locals(&self) -> anyhow::Error {
        let Some(frame) = self.fn_frames.last() else {
            return anyhow!("Too many variables in scope");
        };
        self.error(format!("Too many variables in '{}'", self.hir.text(frame.name)), &frame.body)
    }

    pub(super) fn record_frame_stack_height<T: 'static>(&mut self, id: &HirId<T>) {
        let height = self.frame_slot(self.locals.len());
        self.bindings.frame_stack_heights.insert(id.index(), height);
    }

    fn record_exit_frame_stack_height<T: 'static>(&mut self, scope: &HirId<T>) {
        let height = self.frame_slot(self.locals.len());
        self.bindings.exit_frame_stack_heights.insert(scope.index(), height);
    }

    fn local_offset(&self) -> usize {
        self.fn_frames.last().map_or(0, |frame| frame.local_offset)
    }

    fn frame_slot(&self, index: usize) -> u8 {
        (index - self.local_offset()) as u8
    }

    pub(super) fn declare_local(&mut self, name: Symbol, decl: usize) -> Result<u8, anyhow::Error> {
        #[cfg(debug_assertions)]
        self.bindings.note_declaration(decl);
        let index = self.push_local(Some(name), Some(decl))?;
        Ok(self.frame_slot(index))
    }

    pub(super) fn declare_function(&mut self, stmt: HirId<HirStmt>) {
        if let Some(local) = self.locals.last_mut() {
            local.function = Some(stmt);
        }
    }

    pub(super) fn declare_anchor_binding(&mut self, anchor: HirId<HirExpr>) {
        if let Some(local) = self.locals.last_mut() {
            local.anchor = Some(anchor);
        }
    }

    /// Reserves an unnamed stack slot, returning its frame-relative index.
    pub(super) fn declare_temp(&mut self) -> Result<u8, anyhow::Error> {
        let index = self.push_local(None, None)?;
        Ok(self.frame_slot(index))
    }

    pub(super) fn declare_matcher_binders(&mut self, matcher: &HirId<HirMatcher>, decl: usize) -> Result<Vec<(Symbol, u8)>, anyhow::Error> {
        let mut binders = Vec::new();
        for name in self.hir.get(matcher).binders(self.hir) {
            binders.push((name, self.declare_local(name, decl)?));
        }
        Ok(binders)
    }

    pub(super) fn reserve_frame_temp_slots(&mut self, body: &HirId<HirExpr>, facts: &BodyFacts) -> Result<(), anyhow::Error> {
        let mut finally_trys = Vec::new();
        finally_depths(self.hir, Child::Expr(*body), 0, &mut finally_trys);

        if !facts.defers && finally_trys.is_empty() && facts.path_anchors.is_empty() {
            return Ok(());
        }

        let locals_before = self.locals.len();

        let defer_slot = match facts.defers {
            true => Some(self.declare_temp()?),
            false => None,
        };
        self.reserve_path_anchor_slots(&facts)?;

        let depths = finally_trys.iter().map(|(_, depth)| depth + 1).max().unwrap_or(0);
        let depth_slots = (0..depths).map(|_| self.declare_temp()).collect::<Result<Vec<_>, _>>()?;
        for (try_stmt, depth) in finally_trys {
            self.bindings.finally_slots.insert(try_stmt, depth_slots[depth]);
        }
        let count = self.locals.len() - locals_before;
        self.bindings.frame_temps.insert(*body, FrameTemps { defer_slot, count });
        Ok(())
    }

    pub(super) fn scan_body_facts(&self, body: &HirId<HirExpr>) -> BodyFacts {
        let mut facts = BodyFacts::default();

        visit_body(self.hir, body, &mut |child| {
            if let Child::Stmt(s) = child {
                facts.defers |= matches!(self.hir.get(&s), HirStmt::Defer(_));
                if let HirStmt::Say(decl) = self.hir.get(&s) {
                    if let Some(HirExpr::Anchor(path)) = decl.value.map(|v| self.hir.get(&v)) {
                        let aliased = match self.hir.get(path) {
                            HirExpr::Identifier(root) => Some(*root),
                            _ => None,
                        };
                        facts.anchor_bindings.insert(decl.name, aliased);
                    }
                }
            }
            let Child::Expr(e) = child else { return };
            for name in self.hir.rebound_names(&e) {
                match self.hir.get(&name) {
                    HirExpr::Identifier(name) => { facts.rebound.insert(*name); },
                    HirExpr::This => facts.this_rebound = true,
                    _ => {},
                }
            }
            let written = match self.hir.get(&e) {
                HirExpr::Assign(lhs, _) | HirExpr::CompoundAssign(lhs, _, _) => Some(*lhs),
                HirExpr::Anchor(path) => {
                    if !matches!(self.hir.get(path), HirExpr::Identifier(_) | HirExpr::This) {
                        facts.path_anchors.push((e, *path));
                    }
                    Some(*path)
                },
                _ => None,
            };
            if let Some(HirExpr::Identifier(name)) = written.map(|path| self.hir.get(&access_path_root(self.hir, &path))) {
                facts.written_roots.insert(*name);
            }
        });
        facts
    }

    fn reserve_path_anchor_slots(&mut self, facts: &BodyFacts) -> Result<(), anyhow::Error> {
        for (node, path) in facts.path_anchors.iter() {
            let (root, steps) = access_path_steps(self.hir, path);
            let steps = steps.len();
            let first_slot = self.declare_temp()?;
            for _ in 1..AnchorPathSlots::width(steps) {
                self.declare_temp()?;
            }
            let saved_root = match facts.may_save_root(self.hir, &root, steps) {
                true => Some(self.declare_temp()?),
                false => None,
            };
            self.bindings.anchor_path_slots.insert(*node, AnchorPathSlots::new(first_slot, steps, saved_root));
        }
        Ok(())
    }

    pub(super) fn resolve_local(&self, name: Symbol) -> Option<u8> {
        let start = self.local_offset();
        self.find_local(name).map(|i| (i - start) as u8)
    }

    fn find_local(&self, name: Symbol) -> Option<usize> {
        let start = self.local_offset();
        let Some((visible, body_start)) = self.defer_scope else {
            return self.resolve_local_in_range(name, start, self.locals.len());
        };
        self.resolve_local_in_range(name, body_start, self.locals.len())
            .or_else(|| self.resolve_local_in_range(name, start, visible.max(start)))
    }

    fn resolve_local_in_range(&self, name: Symbol, start: usize, end: usize) -> Option<usize> {
        (start..end).rev().find(|&i| self.locals[i].name == Some(name))
    }

    fn resolve_capture(&mut self, name: Symbol) -> Result<Option<(u8, usize)>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }
        let max_type_frame = self.resolve_member_type(name);
        self.resolve_frame_capture(name, self.fn_frames.len() - 1, max_type_frame)
    }

    fn resolve_frame_capture(&mut self, name: Symbol, frame_idx: usize, max_type_frame: Option<u8>) -> Result<Option<(u8, usize)>, anyhow::Error> {
        let type_frame = self.fn_frames[frame_idx].type_frame;

        // A member-resolvable name must not capture past the type frame that owns it.
        if let Some(max) = max_type_frame {
            if type_frame.map_or(true, |cf| cf < max) {
                return Ok(None);
            }
        }

        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;

        if let Some(i) = self.resolve_local_in_range(name, range_start, range_end) {
            self.mark_captured(i);
            return Ok(Some((self.add_capture((i - range_start) as u8, true, frame_idx)?, i)));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some((idx, local)) = self.resolve_frame_capture(name, frame_idx - 1, max_type_frame)? {
            return Ok(Some((self.add_capture(idx, false, frame_idx)?, local)));
        }

        Ok(None)
    }

    fn add_capture(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        let captures = &mut self.fn_frames[frame_idx].captures;
        if let Some(i) = captures.iter().position(|u| u.location == location && u.is_local == is_local) {
            return Ok(i as u8);
        }
        if captures.len() >= u8::MAX as usize {
            bail!("Too many captures");
        }
        captures.push(CaptureLocation { location, is_local });
        Ok((captures.len() - 1) as u8)
    }

    fn resolve_member_type(&self, name: Symbol) -> Option<u8> {
        for i in (0..self.type_frames.len()).rev() {
            if self.type_frames[i].layout.resolve(name).is_some() {
                return Some(i as u8);
            }
        }
        None
    }

    fn record_decl(&mut self, node: &HirId<HirExpr>, local: usize) {
        if let Some(decl) = self.locals[local].decl {
            self.bindings.decls.insert(*node, decl);
        }
        if let Some(function) = self.locals[local].function {
            self.bindings.function_refs.insert(*node, function);
        }
    }

    pub(super) fn resolve_place(&mut self, name: Symbol, node: &HirId<HirExpr>) -> Result<Place, anyhow::Error> {
        let start = self.local_offset();
        let place = if let Some(i) = self.find_local(name) {
            self.record_decl(node, i);
            if let Some(anchor) = self.locals[i].anchor {
                self.bindings.anchor_bindings.insert(*node, anchor);
            }
            Place::Local((i - start) as u8)
        } else if let Some((idx, local)) = self.resolve_capture(name)? {
            self.record_decl(node, local);
            Place::Capture(idx)
        } else {
            Place::Global(name)
        };
        Ok(place)
    }

    /// Records where `this` reads from.
    pub(super) fn resolve_this(&mut self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(place) = self.receiver_place()? else {
            return Err(self.error("Cannot use 'this' outside of a type method", node));
        };
        self.bindings.places.insert(*node, place);
        Ok(())
    }

    fn receiver_place(&mut self) -> Result<Option<Place>, anyhow::Error> {
        let Some(owner) = self.fn_frames.iter().rposition(|frame| frame.owns_receiver) else {
            return Ok(None);
        };
        match owner == self.fn_frames.len() - 1 {
            true => Ok(Some(Place::Local(self.fn_frames[owner].receiver_copy_slot.unwrap_or(0)))),
            false => Ok(Some(Place::Capture(self.capture_this(owner)?))),
        }
    }

    fn capture_this(&mut self, owner: usize) -> Result<u8, anyhow::Error> {
        // A `&var this` receiver keeps its anchor in slot 0, so a closure captures the copy instead.
        let slot = self.fn_frames[owner].receiver_copy_slot.unwrap_or(0);
        self.mark_captured(self.fn_frames[owner].local_offset + slot as usize);
        let mut idx = self.add_capture(slot, true, owner + 1)?;
        for frame in (owner + 2)..self.fn_frames.len() {
            idx = self.add_capture(idx, false, frame)?;
        }
        Ok(idx)
    }

    pub(super) fn function(&mut self, decl: &HirFnDecl, kind: FnKind, declared_by: Option<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        // A function's callee slot is named for recursion; a method/factory's
        // slot 0 is `this`, addressed positionally and never resolved by name.
        let self_name = match kind {
            FnKind::Function => Some(decl.name),
            _ => None,
        };

        self.scope_depth += 1;
        let local_offset = self.push_local(self_name, None)?;
        if let Some(stmt) = declared_by {
            self.declare_function(stmt);
        }
        self.fn_frames.push(FnFrame {
            captures: Vec::new(),
            name: decl.name,
            local_offset,
            type_frame: self.type_frames.last().map(|_| self.type_frames.len() as u8 - 1),
            owns_receiver: !matches!(kind, FnKind::Function),
            receiver_copy_slot: None,
            body: decl.body,
        });

        // A pattern's binders have to sit after every parameter slot so the frame's arguments stay
        // contiguous, so the patterns wait for a second pass.
        let mut patterned = Vec::new();
        let mut anchored = Vec::new();

        for param in &decl.params {
            let HirExpr::Identifier(param_name) = self.hir.get(&param.name) else {
                unreachable!("parser guarantees parameters are identifiers");
            };

            self.bindings.parameter_decls.insert(param.name.index());

            if param.anchor {
                anchored.push((*param_name, param.name, self.declare_temp()?));
                continue;
            }

            let slot = self.declare_local(*param_name, param.name.index())?;
            self.bindings.places.insert(param.name, Place::Local(slot));

            if param.pattern.is_some() {
                patterned.push(param);
            }
        }

        // An anchor parameter's name is a copy of the value at the anchor, copied in on entry and
        // out on every exit.
        let mut params = Vec::new();
        let facts = self.scan_body_facts(&decl.body);

        for (name, node, anchor_slot) in anchored {
            let value_slot = self.declare_local(name, node.index())?;
            self.bindings.places.insert(node, Place::Local(value_slot));
            let written = facts.written_roots.contains(&name);
            if written {
                self.declare_temp()?;
            }
            params.push(AnchorParam { anchor_slot, value_slot, written, captured: false });
        }

        if decl.receiver.as_ref().is_some_and(|receiver| receiver.anchor) {
            let value_slot = self.declare_temp()?;
            self.declare_temp()?;
            self.fn_frames.last_mut().expect("just checked").receiver_copy_slot = Some(value_slot);
            params.push(AnchorParam { anchor_slot: 0, value_slot, written: true, captured: false });
        }

        if !params.is_empty() {
            self.bindings.anchor_params.insert(decl.body, params);
        }

        // The binders live for the whole body. The frame teardown reclaims them.
        for param in patterned {
            let pattern = param.pattern.as_ref().expect("only patterned parameters were collected");
            self.resolve_matcher_types(pattern);
            let binders = self.declare_matcher_binders(pattern, pattern.index())?;
            self.bindings.match_binders.insert(param.name, binders);
        }

        self.reserve_frame_temp_slots(&decl.body, &facts)?;
        self.record_frame_stack_height(&decl.body);
        self.expression(&decl.body)?;

        let frame = self.fn_frames.pop().unwrap();
        if let Some(params) = self.bindings.anchor_params.get_mut(&decl.body) {
            for param in params {
                param.captured = self.locals[frame.local_offset + param.value_slot as usize].is_captured;
            }
        }
        self.scope_depth -= 1;
        // The frame's callee slot and params (the body's own locals are popped by its block scope).
        self.locals.truncate(frame.local_offset);

        self.bindings.captures.insert(frame.body, frame.captures);
        Ok(())
    }
}
