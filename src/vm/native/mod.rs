use std::collections::HashMap;

use super::gc::Gc;
use super::objects::{ClassMember, ObjClass, ObjNativeFn, ObjString};

pub mod array;
pub mod result;

pub trait NativeType {
    fn get_name(&self) -> &'static str;
    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn>;

    fn build_class(&self, gc: &mut Gc) -> ObjClass {
        let mut class = ObjClass::new(gc.intern(self.get_name()));
        let methods = self.instance_methods(gc);

        let mut member_id = 0;
        for (name, method) in methods {
            class.members.insert(name, ClassMember::Method(member_id));
            class.methods.insert(member_id, gc.alloc(method).into());
            member_id += 1;
        }

        class
    }
}