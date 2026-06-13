use super::gc::Gc;
use super::objects::{ClassMember, ObjClass, ObjNativeFn, ObjString};

pub mod array;

pub trait NativeType {
    fn get_name(&self) -> &'static str;

    /// Regular (named) instance methods.
    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)>;

    /// The indexing getter (`obj[k]`).
    fn getter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }

    /// The indexing setter (`obj[k] = v`).
    fn setter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }

    fn build_class(&self, gc: &mut Gc) -> ObjClass {
        let mut class = ObjClass::new(gc.intern(self.get_name()));

        let mut member_id = 0;
        for (name, method) in self.methods(gc) {
            class.members.insert(name, ClassMember::Method(member_id));
            class.methods.insert(member_id, gc.alloc(method).into());
            member_id += 1;
        }
        if let Some(getter) = self.getter(gc) {
            class.methods.insert(member_id, gc.alloc(getter).into());
            class.getter_id = Some(member_id);
            member_id += 1;
        }
        if let Some(setter) = self.setter(gc) {
            class.methods.insert(member_id, gc.alloc(setter).into());
            class.setter_id = Some(member_id);
            member_id += 1;
        }
        class.member_count = member_id;
        class.build_template();

        class
    }
}
