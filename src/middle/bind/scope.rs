//! Local and upvalue placement: where each name lives at runtime.

use anyhow::bail;

use crate::core::objects::UpvalueLocation;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirMatcher, Symbol};

use super::{FnFrame, FnKind, Local, Place, Receiver, Resolver};

impl<'a> Resolver<'a> {
    pub(super) fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub(super) fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) {
        self.scope_depth -= 1;
        while self.type_scope.last().is_some_and(|t| t.depth > self.scope_depth) {
            let gone = self.type_scope.pop().expect("just checked");
            self.type_index.remove(&gone.name);
        }

        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        let mut cleanups = Vec::new();
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                cleanups.push(super::Cleanup::CloseUpvalue((self.locals.len() - 1) as u8 - local_offset));
            } else {
                cleanups.push(super::Cleanup::Pop);
            }
            self.locals.pop();
        }
        if !cleanups.is_empty() {
            self.bindings.cleanups.insert(node_id.index(), cleanups);
        }
    }

    /// Records that a nested body names this local, so it outlives its own frame's control.
    fn mark_captured(&mut self, index: usize) {
        self.locals[index].is_captured = true;
        if let Some(decl) = self.locals[index].decl {
            self.bindings.captured.insert(decl);
        }
    }

    /// Pushes a local and answers its absolute index. Every local is introduced through here.
    fn push_local(&mut self, name: Option<Symbol>, decl: Option<usize>) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }
        self.locals.push(Local { name, depth: self.scope_depth, is_captured: false, decl });
        Ok((self.locals.len() - 1) as u8)
    }

    /// The index the current frame's slots are counted from.
    fn local_offset(&self) -> u8 {
        self.fn_frames.last().map_or(0, |frame| frame.local_offset)
    }

    /// Declares a binding. `decl` is the node it comes from.
    pub(super) fn declare_local(&mut self, name: Symbol, decl: usize) -> Result<u8, anyhow::Error> {
        #[cfg(debug_assertions)]
        self.bindings.note_declaration(decl);

        // Duplicate-name collisions across the whole namespace are caught earlier, in `middle::names`.
        let index = self.push_local(Some(name), Some(decl))?;
        Ok(index - self.local_offset())
    }

    /// Reserves an unnamed stack slot, returning its frame-relative index.
    pub(super) fn declare_temp(&mut self) -> Result<u8, anyhow::Error> {
        let index = self.push_local(None, None)?;
        Ok(index - self.local_offset())
    }

    /// Declares a matcher's binders as locals, pairing each with the slot it stores into.
    pub(super) fn declare_binders(&mut self, matcher: &HirId<HirMatcher>, decl: usize) -> Result<Vec<(Symbol, u8)>, anyhow::Error> {
        let mut binders = Vec::new();
        for name in self.hir.get(matcher).binders(self.hir) {
            binders.push((name, self.declare_local(name, decl)?));
        }
        Ok(binders)
    }

    pub(super) fn resolve_local(&self, name: Symbol) -> Option<u8> {
        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        self.resolve_local_in_range(name, local_offset, self.locals.len() as u8)
    }

    fn resolve_local_in_range(&self, name: Symbol, start: u8, end: u8) -> Option<u8> {
        for i in (start..end).rev() {
            if self.locals[i as usize].name == Some(name) {
                return Some(i - start);
            }
        }
        None
    }

    fn resolve_upvalue(&mut self, name: Symbol) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }
        let max_type_frame = self.resolve_member_type(name);
        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1, max_type_frame)
    }

    fn resolve_frame_upvalue(&mut self, name: Symbol, frame_idx: usize, max_type_frame: Option<u8>) -> Result<Option<u8>, anyhow::Error> {
        let type_frame = self.fn_frames[frame_idx].type_frame;

        // A member-resolvable name must not capture past the type frame that owns it: stop if this
        // frame is outside that type (no type frame, or one nested shallower than the owner).
        if let Some(max) = max_type_frame {
            if type_frame.map_or(true, |cf| cf < max) {
                return Ok(None);
            }
        }

        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;

        if let Some(idx) = self.resolve_local_in_range(name, range_start, range_end) {
            self.mark_captured((range_start + idx) as usize);
            return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1, max_type_frame)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        let upvalues = &mut self.fn_frames[frame_idx].upvalues;
        if let Some(i) = upvalues.iter().position(|u| u.location == location && u.is_local == is_local) {
            return Ok(i as u8);
        }
        if upvalues.len() >= u8::MAX as usize {
            bail!("Too many upvalues");
        }
        upvalues.push(UpvalueLocation { location, is_local });
        Ok((upvalues.len() - 1) as u8)
    }

    fn resolve_member_type(&self, name: Symbol) -> Option<u8> {
        for i in (0..self.type_frames.len()).rev() {
            if self.type_frames[i].layout.resolve(name).is_some() {
                return Some(i as u8);
            }
        }
        None
    }

    pub(super) fn resolve_place(&mut self, name: Symbol, node: &HirId<HirExpr>) -> Result<Place, anyhow::Error> {
        let place = if let Some(slot) = self.resolve_local(name) {
            Place::Local(slot)
        } else if let Some((id, receiver)) = self.this_field(name)? {
            Place::Field(id, receiver)
        } else if let Some(idx) = self.resolve_upvalue(name)? {
            Place::Upvalue(idx)
        } else {
            self.deny_private(name, node)?;
            Place::Global(name)
        };
        Ok(place)
    }

    /// Records where `this` reads from.
    pub(super) fn resolve_this(&mut self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(receiver) = self.receiver_place()? else {
            return Err(self.error("Cannot use 'this' outside of a type method", node));
        };
        let place = match receiver {
            Receiver::Slot => Place::Local(0),
            Receiver::Upvalue(idx) => Place::Upvalue(idx),
        };
        self.bindings.places.insert(*node, place);
        Ok(())
    }

    /// A bare name that names a field of the enclosing type, with where its receiver sits. A bare
    /// field is `this.<name>`, so a nested body captures the receiver exactly as a written `this` does.
    fn this_field(&mut self, name: Symbol) -> Result<Option<(u8, Receiver)>, anyhow::Error> {
        let Some(id) = self.this_field_id(name) else { return Ok(None) };
        Ok(self.receiver_place()?.map(|receiver| (id, receiver)))
    }

    /// Where the receiver sits for the body being resolved. The receiver is slot 0 of the nearest
    /// method or factory frame, so a nested body reaches it as an upvalue like any captured local.
    fn receiver_place(&mut self) -> Result<Option<Receiver>, anyhow::Error> {
        let Some(owner) = self.fn_frames.iter().rposition(|frame| frame.owns_receiver) else {
            return Ok(None);
        };
        match owner == self.fn_frames.len() - 1 {
            true => Ok(Some(Receiver::Slot)),
            false => Ok(Some(Receiver::Upvalue(self.capture_this(owner)?))),
        }
    }

    /// Chains one upvalue per frame between the receiver's owner and the body naming it.
    fn capture_this(&mut self, owner: usize) -> Result<u8, anyhow::Error> {
        self.mark_captured(self.fn_frames[owner].local_offset as usize);
        let mut idx = self.add_upvalue(0, true, owner + 1)?;
        for frame in (owner + 2)..self.fn_frames.len() {
            idx = self.add_upvalue(idx, false, frame)?;
        }
        Ok(idx)
    }

    pub(super) fn function(&mut self, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        // A function's callee slot is named for recursion; a method/factory's
        // slot 0 is `this`, addressed positionally and never resolved by name.
        let self_name = match kind {
            FnKind::Function => Some(decl.name),
            _ => None,
        };

        self.scope_depth += 1;
        let local_offset = self.push_local(self_name, None)?;
        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset,
            type_frame: self.type_frames.last().map(|_| self.type_frames.len() as u8 - 1),
            owns_receiver: !matches!(kind, FnKind::Function),
            body: decl.body,
        });

        // A pattern's binders have to sit after every parameter slot so the frame's arguments stay
        // contiguous, so the patterns wait for a second pass.
        let mut patterned = Vec::new();
        for param in &decl.params {
            let HirExpr::Identifier(param_name) = self.hir.get(&param.name) else {
                unreachable!("parser guarantees parameters are identifiers");
            };
            let slot = self.declare_local(*param_name, param.name.index())?;
            if param.pattern.is_some() {
                // The entry step loads the parameter by name, like any other identifier.
                self.bindings.places.insert(param.name, Place::Local(slot));
                patterned.push(param);
            }
        }

        // The binders live for the whole body. The frame teardown reclaims them.
        for param in patterned {
            let pattern = param.pattern.as_ref().expect("only patterned parameters were collected");
            self.resolve_matcher_types(pattern);
            let binders = self.declare_binders(pattern, pattern.index())?;
            self.bindings.match_binders.insert(param.name, binders);
        }

        self.expression(&decl.body)?;

        let frame = self.fn_frames.pop().unwrap();
        self.scope_depth -= 1;
        // The frame's callee slot and params (the body's own locals are popped by its block scope).
        self.locals.truncate(frame.local_offset as usize);

        self.bindings.upvalues.insert(frame.body, frame.upvalues);
        Ok(())
    }
}
