use crate::parser::{AstId, ClassDecl, Stmt};
use crate::runtime::objects::{ClassMember, ObjClass, ObjFn, ObjString};
use crate::runtime::opcode;
use crate::runtime::value::Value;

use anyhow::anyhow;

use super::{ClassCompilation, ClassFrame, Compiler, FnKind};

impl<'a> Compiler<'a> {
    pub (super) fn class_declaration(&mut self, stmt: &AstId<Stmt>, decl: &Box<ClassDecl>) -> Result<(), anyhow::Error> {
        // The slot was reserved by declaration hoisting before any body in this scope
        // was compiled, which is what lets forward references resolve.
        let name = self.gc.intern(&decl.name);
        let slot = self.resolve_local(name)
            .expect("class declarations are reserved by hoist_declarations before compilation");
        self.enter_scope();

        let superclass = match &decl.superclass {
            Some(name) => {
                let class_comp = self.classes.get(&self.gc.intern(name))
                    .ok_or(anyhow!("Class '{}' not declared", name))?;
                Some(*class_comp)
            },
            None => None
        };

        let mut frame = ClassFrame {
            class: ObjClass::new(self.gc.intern(&decl.name)),
            superclass
        };

        let mut next_member_id: u8 = 0;

        if let Some(superclass_comp) = frame.superclass {
            let superclass = unsafe { &*superclass_comp.class };
            frame.class.members = superclass.members.clone();
            frame.class.fields = superclass.fields.clone();
            frame.class.methods = superclass.methods.clone();
            next_member_id = superclass_comp.next_member_id;
        }

        // Declare fields and methods before compiling init and method bodies
        for field in &decl.fields {
            frame.class.members.insert(self.gc.intern(field), ClassMember::Field(next_member_id));
            frame.class.fields.insert(next_member_id);
            next_member_id += 1;
        }

        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            frame.class.members.insert(self.gc.intern(&method.name), ClassMember::Method(next_member_id));
            next_member_id += 1;
        }

        if let Some(_) = &decl.getter {
            frame.class.members.insert(self.gc.preset_identifiers.get, ClassMember::Method(next_member_id));
            next_member_id += 1;
        }

        if let Some(_) = &decl.setter {
            frame.class.members.insert(self.gc.preset_identifiers.set, ClassMember::Method(next_member_id));
            next_member_id += 1;
        }

        // Push class frame to make the current class visible in method bodies
        self.class_frames.push(frame);

        if let Some(stmt_id) = &decl.getter {
            self.install_method(stmt_id, self.gc.preset_identifiers.get)?;
        }

        if let Some(stmt_id) = &decl.setter {
            self.install_method(stmt_id, self.gc.preset_identifiers.set)?;
        }

        // Compile and assign class initializer function
        let init_func_ptr = self.compile_fn(&decl.init, FnKind::Initializer)?;
        let frame = self.class_frames.last_mut().unwrap();
        frame.class.members.insert(self.gc.preset_identifiers.init, ClassMember::Method(next_member_id));
        frame.class.methods.insert(next_member_id, init_func_ptr.into());
        next_member_id += 1;

        // Compile and add methods to class object
        for stmt_id in &decl.methods {
            let name = self.gc.intern(&self.fn_decl(stmt_id).name);
            self.install_method(stmt_id, name)?;
        }

        let mut class = self.class_frames.pop().unwrap().class;
        class.member_count = next_member_id;
        self.exit_scope(stmt);

        let class = self.gc.alloc(class);
        let idx = self.chunk.add_constant(Value::from(class))?;
        self.classes.insert(self.gc.intern(&decl.name), ClassCompilation { class, next_member_id });
        self.emit_operand(opcode::PUSH_CLASS, idx, stmt);

        // Store the class into the reserved slot and discard the placeholder.
        self.emit_operand(opcode::SET_LOCAL, slot, stmt);
        self.emit(opcode::POP, stmt);

        Ok(())
    }

    fn compile_fn(&mut self, stmt: &AstId<Stmt>, kind: FnKind) -> Result<*mut ObjFn, anyhow::Error> {
        let decl = self.fn_decl(stmt);
        let const_idx = self.function(stmt, decl, kind)?;
        let func_const = self.chunk.constants[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    /// Compiles a method and installs it into the current class frame.
    fn install_method(&mut self, stmt: &AstId<Stmt>, name: *mut ObjString) -> Result<(), anyhow::Error> {
        let function_ptr = self.compile_fn(stmt, FnKind::Method)?;
        let frame = self.class_frames.last_mut().unwrap();
        let ClassMember::Method(id) = frame.class.resolve(name).unwrap() else { unreachable!(); };
        frame.class.methods.insert(id, function_ptr.into());
        Ok(())
    }
}