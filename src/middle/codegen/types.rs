use crate::core::objects::{BuiltinLayout, TypeMember, ObjType, ObjFn, ObjString};
use crate::core::value::Value;
use crate::middle::hir::{HirTypeDecl, HirId, HirStmt, TypeId};
use crate::middle::ir::Inst;
use crate::middle::bind::{FnKind, TypeLayout};

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn type_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &Box<HirTypeDecl>) -> Result<(), anyhow::Error> {
        // Build the type from the resolver-computed member layout.
        let layout = self.bindings.type_layout(stmt);

        if let Some(builtin) = decl.builtin {
            // The layout ships in the chunk, so it is ordered by member id rather than by however
            // the member map iterates.
            let mut members: Vec<(String, TypeMember)> = layout.members.iter()
                .filter(|(_, m)| !layout.non_public.contains(&m.id()))
                .map(|(&sym, &m)| (self.hir.text(sym).to_string(), m))
                .collect();
            members.sort_by_key(|(_, m)| m.id());
            self.ir.set_builtin_layout(builtin, BuiltinLayout {
                id: decl.id,
                members,
                field_count: layout.fields.len() as u8,
                factory_id: layout.factory_id,
                member_count: layout.member_count,
            });
            return Ok(());
        }
        let slot = self.bindings.slot(stmt);
        let type_name = self.gc.intern(self.hir.text(layout.name));
        let mut ty = ObjType::new(type_name);
        for (&sym, &member) in &layout.members {
            let name = self.gc.intern(self.hir.text(sym));
            ty.members.insert(name, member);
        }
        ty.id = decl.id;
        ty.field_count = layout.fields.len() as u8;
        ty.member_count = layout.member_count;

        // Each `gives` delegate is verified at construction, so carry its field id, its name for the
        // message, and the trait declaration the field must provide.
        let mut gives = Vec::with_capacity(decl.gives.len());
        for &(field, trait_sym, trait_id) in &decl.gives {
            let TypeMember::Field(id) = layout.members[&field] else { unreachable!("gives delegate is a field") };
            let field_ref = self.gc.intern(self.hir.text(field));
            let trait_ref = self.gc.intern(self.hir.text(trait_sym));
            gives.push((id, field_ref, trait_ref, trait_id));
        }
        ty.gives = gives.into_boxed_slice();

        // `x is T`: this type provides its own declaration and every transitively `with`-mixed trait.
        for (_, id) in &decl.provides {
            ty.provided.insert(*id);
        }
        // The same set under codegen's dense numbering, for the barrier test.
        let provided: Vec<TypeId> = decl.provides.iter().map(|(_, id)| *id).collect();
        ty.witness_ids = self.witness_id_set(&provided);
        ty.field_accepts = self.field_accepts(layout)?;

        // Compile the factory into its slot. A factory-less type has none, so its `factory_id`
        // stays None and `K()` on it finds no factory to call.
        let mut captures = false;
        if let HirStmt::Fn(_) = self.hir.get(&decl.init) {
            ty.factory_id = Some(layout.factory_id);
            let init_ptr = self.compile_fn(&decl.init, FnKind::Factory)?;
            captures |= !unsafe { &*init_ptr }.upvalues.is_empty();
            ty.methods.insert(layout.factory_id, init_ptr.into());
        }

        // Methods carry a `Type.method` display name so stack traces and arity errors
        // can name the owning type,.
        let type_text = self.hir.text(layout.name).to_string();
        for stmt_id in &decl.methods {
            let method_text = self.hir.text(self.fn_decl(stmt_id).name);
            let name = self.gc.intern(method_text);
            let display = self.gc.intern(format!("{type_text}.{method_text}"));
            captures |= self.install_method(&mut ty, stmt_id, name, display)?;
        }

        // Drop non-public members (private/`inner`, and the per-trait renamed `"<Trait>.<name>"`
        // slots) from the runtime name map: they're reached only internally by member id, so keeping
        // them out means external `obj.x` simply doesn't find them.
        let non_public = &self.bindings.type_layout(stmt).non_public;
        ty.members.retain(|_, member| !non_public.contains(&member.id()));

        ty.build_template();
        let ty = self.gc.alloc(ty);
        let idx = self.ir.add_constant(Value::from(ty))?;
        // A capturing method needs a closure over the frame that declared the type. Such a type is
        // built per execution. One that captures nothing is the same object every time.
        self.emit(if captures { Inst::BuildType(idx) } else { Inst::PushType(idx) }, stmt);

        // Store the type into the reserved slot and discard the placeholder.
        self.emit(Inst::StoreLocal(slot), stmt);
        self.emit(Inst::Pop, stmt);

        Ok(())
    }

    /// What each field accepts, by field id.
    fn field_accepts(&mut self, layout: &TypeLayout) -> Result<Box<[u16]>, anyhow::Error> {
        let mut out = Vec::with_capacity(layout.fields.len());
        for &id in &layout.fields {
            let owed = layout.clauses.get(&id).map(|c| c.owed.clone()).unwrap_or_default();
            out.push(self.accepts_index(&owed, layout.nullable.contains(&id))?);
        }
        Ok(out.into_boxed_slice())
    }

    fn compile_fn(&mut self, stmt: &HirId<HirStmt>, kind: FnKind) -> Result<*mut ObjFn, anyhow::Error> {
        let decl = self.fn_decl(stmt);
        let const_idx = self.function(stmt, (*stmt).into(), decl, kind)?;
        let func_const = self.ir.constants()[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    /// Installs a method, answering whether it captures anything from an enclosing scope.
    fn install_method(&mut self, ty: &mut ObjType, stmt: &HirId<HirStmt>, name: *mut ObjString, display: *mut ObjString) -> Result<bool, anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        unsafe { (*function_ptr).name = display; }
        let TypeMember::Method(id) = ty.resolve(name).unwrap() else { unreachable!() };
        ty.methods.insert(id, function_ptr.into());
        Ok(!unsafe { &*function_ptr }.upvalues.is_empty())
    }
}
