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
        ty.init_id = Some(layout.init_id);

        // `x is T`: this type provides its own name and every transitively `with`-mixed trait.
        for name in &decl.provides {
            let name_ref = self.gc.intern(self.hir.text(*name));
            ty.provided.insert(name_ref);
        }

        // Compile the initializer/method bodies into their slots.
        let init_ptr = self.compile_fn(&decl.init, FnKind::Initializer)?;
        ty.methods.insert(ty.init_id.unwrap(), init_ptr.into());

        for stmt_id in &decl.methods {
            let name = self.gc.intern(self.hir.text(self.fn_decl(stmt_id).name));
            self.install_method(&mut ty, stmt_id, name)?;
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
        let const_idx = self.function(stmt, decl, kind)?;
        let func_const = self.ir.constants()[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    /// Compiles a method and installs its `ObjFn` at the member id.
    fn install_method(&mut self, ty: &mut ObjType, stmt: &HirId<HirStmt>, name: *mut ObjString) -> Result<(), anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        let TypeMember::Method(id) = ty.resolve(name).unwrap() else { unreachable!() };
        ty.methods.insert(id, function_ptr.into());
        Ok(())
    }
}
