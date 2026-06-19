use super::gc::Gc;
use super::objects::{TypeMember, ObjType, ObjNativeFn, ObjString};

pub mod array;
pub mod dict;

pub trait NativeType {
    fn get_name(&self) -> &'static str;

    /// Regular (named) instance methods.
    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)>;

    /// The indexing getter (`obj[k]`).
    fn getter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }

    /// The indexing setter (`obj[k] = v`).
    fn setter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }

    fn build_type(&self, gc: &mut Gc) -> ObjType {
        let mut ty = ObjType::new(gc.intern(self.get_name()));

        let mut member_id = 0;
        for (name, method) in self.methods(gc) {
            ty.members.insert(name, TypeMember::Method(member_id));
            ty.methods.insert(member_id, gc.alloc(method).into());
            member_id += 1;
        }
        if let Some(getter) = self.getter(gc) {
            ty.methods.insert(member_id, gc.alloc(getter).into());
            ty.getter_id = Some(member_id);
            member_id += 1;
        }
        if let Some(setter) = self.setter(gc) {
            ty.methods.insert(member_id, gc.alloc(setter).into());
            ty.setter_id = Some(member_id);
            member_id += 1;
        }
        ty.member_count = member_id;
        ty.build_template();

        ty
    }
}
