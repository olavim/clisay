use crate::core::objects::{TypeMember, ObjType, ObjFn, ObjString};
use crate::core::value::Value;
use crate::middle::hir::{HirTypeDecl, HirId, HirStmt};
use crate::middle::ir::Inst;
use crate::middle::bind::FnKind;

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn type_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &Box<HirTypeDecl>) -> Result<(), anyhow::Error> {
        let slot = self.bindings.slot(stmt);

        // Build the type from the resolver-computed member layout.
        let layout = self.bindings.type_layout(stmt);
        let type_name = self.gc.intern(self.hir.text(layout.name));
        let mut ty = ObjType::new(type_name);
        for (&sym, &member) in &layout.members {
            let name = self.gc.intern(self.hir.text(sym));
            ty.members.insert(name, member);
        }
        for &field_id in &layout.fields {
            ty.fields.insert(field_id);
        }
        ty.member_count = layout.member_count;

        // Each `gives` delegate is verified at construction, so carry its field id and trait name.
        let mut gives = Vec::with_capacity(decl.gives.len());
        for &(field, trait_sym) in &decl.gives {
            let TypeMember::Field(id) = layout.members[&field] else { unreachable!("gives delegate is a field") };
            let field_ref = self.gc.intern(self.hir.text(field));
            let trait_ref = self.gc.intern(self.hir.text(trait_sym));
            gives.push((id, field_ref, trait_ref));
        }
        ty.gives = gives.into_boxed_slice();

        // `x is T`: this type provides its own name and every transitively `with`-mixed trait.
        for name in &decl.provides {
            let name_ref = self.gc.intern(self.hir.text(*name));
            ty.provided.insert(name_ref);
        }

        // Compile the factory into its slot. A factory-less type has none, so its `init_id`
        // stays None and `K()` on it finds no factory to call.
        if let HirStmt::Fn(_) = self.hir.get(&decl.init) {
            ty.init_id = Some(layout.init_id);
            let init_ptr = self.compile_fn(&decl.init, FnKind::Initializer)?;
            ty.methods.insert(layout.init_id, init_ptr.into());
        }

        // Methods carry a `Type.method` display name so stack traces and arity errors
        // can name the owning type,.
        let type_text = self.hir.text(layout.name).to_string();
        for stmt_id in &decl.methods {
            let method_text = self.hir.text(self.fn_decl(stmt_id).name);
            let name = self.gc.intern(method_text);
            let display = self.gc.intern(format!("{type_text}.{method_text}"));
            self.install_method(&mut ty, stmt_id, name, display)?;
        }

        // Drop non-public members (private/`inner`, and the per-trait renamed `"<Trait>.<name>"`
        // slots) from the runtime name map: they're reached only internally by member id, so keeping
        // them out means external `obj.x` simply doesn't find them.
        let non_public = &self.bindings.type_layout(stmt).non_public;
        ty.members.retain(|_, member| {
            let (TypeMember::Field(id) | TypeMember::Method(id)) = member;
            !non_public.contains(id)
        });

        ty.build_template();
        let ty = self.gc.alloc(ty);
        let idx = self.ir.add_constant(Value::from(ty))?;
        self.types.insert(type_name, ty);
        self.emit(Inst::PushType(idx), stmt);

        // Store the type into the reserved slot and discard the placeholder.
        self.emit(Inst::StoreLocal(slot), stmt);
        self.emit(Inst::Pop, stmt);

        Ok(())
    }

    fn compile_fn(&mut self, stmt: &HirId<HirStmt>, kind: FnKind) -> Result<*mut ObjFn, anyhow::Error> {
        let decl = self.fn_decl(stmt);
        let const_idx = self.function(stmt, decl, kind, self.persist_mask(stmt))?;
        let func_const = self.ir.constants()[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    fn install_method(&mut self, ty: &mut ObjType, stmt: &HirId<HirStmt>, name: *mut ObjString, display: *mut ObjString) -> Result<(), anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        unsafe { (*function_ptr).name = display; }
        let TypeMember::Method(id) = ty.resolve(name).unwrap() else { unreachable!() };
        ty.methods.insert(id, function_ptr.into());
        Ok(())
    }
}
