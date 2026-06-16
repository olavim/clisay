use crate::core::objects::{ClassMember, ObjType, ObjFn, ObjString};
use crate::core::value::Value;
use crate::middle::hir::{HirTypeDecl, HirId, HirStmt};
use crate::middle::ir::Inst;
use crate::middle::resolve::FnKind;

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn class_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &Box<HirTypeDecl>) -> Result<(), anyhow::Error> {
        let slot = self.bindings.slot(stmt);

        // Build the class from the resolver-computed member layout.
        let layout = self.bindings.class_layout(stmt);
        let class_name = self.gc.intern(self.hir.text(layout.name));
        let superclass = layout.superclass;
        let mut class = ObjType::new(class_name);
        for (&sym, &member) in &layout.members {
            let name = self.gc.intern(self.hir.text(sym));
            class.members.insert(name, member);
        }
        for &field_id in &layout.fields {
            class.fields.insert(field_id);
        }
        class.member_count = layout.member_count;
        class.getter_id = layout.getter_id;
        class.setter_id = layout.setter_id;
        class.init_id = Some(layout.init_id);

        // Inherit the superclass's compiled methods.
        if let Some(super_sym) = superclass {
            let super_name = self.gc.intern(self.hir.text(super_sym));
            let super_class = self.classes[&super_name];
            class.methods = unsafe { &*super_class }.methods.clone();
        }

        // Compile the accessor/initializer/method bodies into their slots.
        if let Some(stmt_id) = &decl.getter {
            let ptr = self.compile_fn(stmt_id, FnKind::Method)?;
            class.methods.insert(class.getter_id.unwrap(), ptr.into());
        }
        if let Some(stmt_id) = &decl.setter {
            let ptr = self.compile_fn(stmt_id, FnKind::Method)?;
            class.methods.insert(class.setter_id.unwrap(), ptr.into());
        }
        let init_ptr = self.compile_fn(&decl.init, FnKind::Initializer)?;
        class.methods.insert(class.init_id.unwrap(), init_ptr.into());

        for stmt_id in &decl.methods {
            let name = self.gc.intern(self.hir.text(self.fn_decl(stmt_id).name));
            self.install_method(&mut class, stmt_id, name)?;
        }

        class.build_template();
        let class = self.gc.alloc(class);
        let idx = self.ir.add_constant(Value::from(class))?;
        self.classes.insert(class_name, class);
        self.emit(Inst::PushClass(idx), stmt);

        // Store the class into the reserved slot and discard the placeholder.
        self.emit(Inst::SetLocal(slot), stmt);
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
    fn install_method(&mut self, class: &mut ObjType, stmt: &HirId<HirStmt>, name: *mut ObjString) -> Result<(), anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        let ClassMember::Method(id) = class.resolve(name).unwrap() else { unreachable!() };
        class.methods.insert(id, function_ptr.into());
        Ok(())
    }
}
