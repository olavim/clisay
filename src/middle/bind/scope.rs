//! Local and upvalue placement: where each name lives at runtime.

use anyhow::bail;

use crate::core::objects::UpvalueLocation;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, Symbol};

use super::{FnFrame, FnKind, Local, Place, Resolver};

impl<'a> Resolver<'a> {
    pub(super) fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub(super) fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) {
        self.scope_depth -= 1;
        let mut cleanups = Vec::new();
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                cleanups.push(super::Cleanup::CloseUpvalue(self.locals.len() as u8 - 1));
            } else {
                cleanups.push(super::Cleanup::Pop);
            }
            self.locals.pop();
        }
        if !cleanups.is_empty() {
            self.bindings.cleanups.insert(node_id.index(), cleanups);
        }
    }

    pub(super) fn declare_local(&mut self, name: Symbol) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        // Duplicate-name collisions across the whole namespace are caught earlier, in `middle::names`.
        self.locals.push(Local { name: Some(name), depth: self.scope_depth, is_captured: false });

        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        Ok((self.locals.len() - 1) as u8 - local_offset)
    }

    /// Reserves an unnamed stack slot, returning its frame-relative index.
    pub(super) fn declare_temp(&mut self) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }
        self.locals.push(Local { name: None, depth: self.scope_depth, is_captured: false });
        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        Ok((self.locals.len() - 1) as u8 - local_offset)
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
            self.locals[(range_start + idx) as usize].is_captured = true;
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
        } else if let Some(id) = self.this_field_id(name) {
            Place::Field(id)
        } else if let Some(idx) = self.resolve_upvalue(name)? {
            Place::Upvalue(idx)
        } else {
            self.deny_private(name, node)?;
            Place::Global(name)
        };
        Ok(place)
    }

    pub(super) fn function(&mut self, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        // A function's callee slot is named for recursion; a method/initializer's
        // slot 0 is `this`, addressed positionally and never resolved by name.
        let self_name = match kind {
            FnKind::Function => Some(decl.name),
            _ => None,
        };

        self.scope_depth += 1;
        let local_offset = self.locals.len() as u8;
        self.locals.push(Local { name: self_name, depth: self.scope_depth, is_captured: false });
        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset,
            type_frame: self.type_frames.last().map(|_| self.type_frames.len() as u8 - 1),
            body: decl.body,
        });

        for param in &decl.params {
            let HirExpr::Identifier(param_name) = self.hir.get(&param.name) else {
                unreachable!("parser guarantees parameters are identifiers");
            };
            self.declare_local(*param_name)?;
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
