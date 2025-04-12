use crate::parser::{ASTId, ClassDecl, Stmt};
use crate::vm::objects::{ClassMember, ObjClass, ObjFn};
use crate::vm::opcode;
use crate::vm::value::Value;

use anyhow::anyhow;

use super::{ClassCompilation, ClassFrame, Compiler, FnKind};

impl<'a> Compiler<'a> {
    pub (super) fn class_declaration(&mut self, stmt: &ASTId<Stmt>, decl: &Box<ClassDecl>) -> Result<(), anyhow::Error> {
        let name = self.gc.intern(&decl.name);
        self.declare_local(name, false)?;
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
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            frame.class.members.insert(self.gc.intern(&decl.name), ClassMember::Method(next_member_id));
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
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            let ClassMember::Method(id) = frame.class.resolve(self.gc.preset_identifiers.get).unwrap() else { unreachable!(); };
            frame.class.methods.insert(id, function_ptr.into());
        }

        if let Some(stmt_id) = &decl.setter {
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            let ClassMember::Method(id) = frame.class.resolve(self.gc.preset_identifiers.set).unwrap() else { unreachable!(); };
            frame.class.methods.insert(id, function_ptr.into());
        }

        // Compile and assign class initializer function
        let init_func_ptr = self.initializer(&decl.init)?;
        let frame = self.class_frames.last_mut().unwrap();
        frame.class.members.insert(self.gc.preset_identifiers.init, ClassMember::Method(next_member_id));
        frame.class.methods.insert(next_member_id, init_func_ptr.into());
        next_member_id += 1;

        // Compile and add methods to class object
        for stmt_id in &decl.methods {
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            let ClassMember::Method(id) = frame.class.resolve(self.gc.intern(&decl.name)).unwrap() else { unreachable!(); };
            frame.class.methods.insert(id, function_ptr.into());
        }

        let class = self.class_frames.pop().unwrap().class;
        self.exit_scope(stmt);

        let class = self.gc.alloc(class);
        let idx = self.chunk.add_constant(Value::from(class))?;
        self.classes.insert(self.gc.intern(&decl.name), ClassCompilation { class, next_member_id });
        self.emit(opcode::PUSH_CLASS, stmt);
        self.emit(idx, stmt);
        
        Ok(())
    }

    fn initializer(&mut self, stmt: &ASTId<Stmt>) -> Result<*mut ObjFn, anyhow::Error> {
        let Stmt::Fn(decl) = self.ast.get(&stmt) else { unreachable!(); };
        let const_idx = self.function(stmt, decl, FnKind::Initializer)?;
        let func_const = self.chunk.constants[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    fn class_method(&mut self, stmt: &ASTId<Stmt>) -> Result<*mut ObjFn, anyhow::Error> {
        let Stmt::Fn(decl) = self.ast.get(stmt) else { unreachable!(); };
        let const_idx = self.function(stmt, decl, FnKind::Method)?;
        let func_const = self.chunk.constants[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }
}