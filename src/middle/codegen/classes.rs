use crate::ast::{AstId, ClassDecl, Stmt};
use crate::core::objects::{ClassMember, ObjClass, ObjFn, ObjString};
use crate::core::value::Value;
use crate::middle::ir::Inst;
use crate::middle::resolve::FnKind;

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn class_declaration(&mut self, stmt: &AstId<Stmt>, decl: &Box<ClassDecl>) -> Result<(), anyhow::Error> {
        let slot = self.bindings.slot(stmt);

        // Build the class from the resolver-computed member layout.
        let layout = self.bindings.class_layout(stmt);
        let class_name = self.gc.intern(self.ast.text(layout.name));
        let superclass = layout.superclass;
        let mut class = ObjClass::new(class_name);
        for (&sym, &member) in &layout.members {
            let name = self.gc.intern(self.ast.text(sym));
            class.members.insert(name, member);
        }
        // The accessor/initializer convention is a runtime detail: expose them
        // under the `@get`/`@set`/`@init` names the VM dispatches on.
        if let Some(id) = layout.getter_id {
            class.members.insert(self.gc.preset_identifiers.get, ClassMember::Method(id));
        }
        if let Some(id) = layout.setter_id {
            class.members.insert(self.gc.preset_identifiers.set, ClassMember::Method(id));
        }
        class.members.insert(self.gc.preset_identifiers.init, ClassMember::Method(layout.init_id));
        for &field_id in &layout.fields {
            class.fields.insert(field_id);
        }
        class.member_count = layout.member_count;

        // Inherit the superclass's compiled methods.
        if let Some(super_sym) = superclass {
            let super_name = self.gc.intern(self.ast.text(super_sym));
            let super_class = self.classes[&super_name];
            class.methods = unsafe { &*super_class }.methods.clone();
        }

        if let Some(stmt_id) = &decl.getter {
            let getter = self.gc.preset_identifiers.get;
            self.install_method(&mut class, stmt_id, getter)?;
        }
        if let Some(stmt_id) = &decl.setter {
            let setter = self.gc.preset_identifiers.set;
            self.install_method(&mut class, stmt_id, setter)?;
        }

        let init_ptr = self.compile_fn(&decl.init, FnKind::Initializer)?;
        let init = self.gc.preset_identifiers.init;
        let ClassMember::Method(init_id) = class.resolve(init).unwrap() else { unreachable!() };
        class.methods.insert(init_id, init_ptr.into());

        for stmt_id in &decl.methods {
            let name = self.gc.intern(self.ast.text(self.fn_decl(stmt_id).name));
            self.install_method(&mut class, stmt_id, name)?;
        }

        let class = self.gc.alloc(class);
        let idx = self.ir.add_constant(Value::from(class))?;
        self.classes.insert(class_name, class);
        self.emit(Inst::PushClass(idx), stmt);

        // Store the class into the reserved slot and discard the placeholder.
        self.emit(Inst::SetLocal(slot), stmt);
        self.emit(Inst::Pop, stmt);

        Ok(())
    }

    fn compile_fn(&mut self, stmt: &AstId<Stmt>, kind: FnKind) -> Result<*mut ObjFn, anyhow::Error> {
        let decl = self.fn_decl(stmt);
        let const_idx = self.function(stmt, decl, kind)?;
        let func_const = self.ir.constants()[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    /// Compiles a method and installs its `ObjFn` at the member id.
    fn install_method(&mut self, class: &mut ObjClass, stmt: &AstId<Stmt>, name: *mut ObjString) -> Result<(), anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        let ClassMember::Method(id) = class.resolve(name).unwrap() else { unreachable!() };
        class.methods.insert(id, function_ptr.into());
        Ok(())
    }
}
