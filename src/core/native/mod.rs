use super::gc::Gc;
use super::objects::{TypeMember, ObjType, ObjNativeFn, ObjString};

pub mod array;
pub mod dict;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NativeType {
    Array,
    Dict,
    String,
}

impl NativeType {
    pub fn name(self) -> &'static str {
        match self {
            NativeType::Array => "Array",
            NativeType::Dict => "Dict",
            NativeType::String => "String",
        }
    }

    pub fn keys_can_shadow_members(self) -> bool {
        self == NativeType::Dict
    }

    pub fn method_wants_anchor_receiver(self, method_name: &str) -> bool {
        matches!((self, method_name), (NativeType::Array, "push") | (NativeType::Dict, "remove"))
    }
}

pub trait NativeTypeBuilder {
    fn kind(&self) -> NativeType;

    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)>;
    fn getter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }
    fn setter(&self, _gc: &mut Gc) -> Option<ObjNativeFn> { None }

    fn build_type(&self, gc: &mut Gc) -> ObjType {
        let kind = self.kind();
        let mut ty = ObjType::new(gc.intern(kind.name()));

        let mut member_id = 0;
        for (name, method) in self.methods(gc) {
            let method = ObjNativeFn { wants_anchor_receiver: kind.method_wants_anchor_receiver(unsafe { &(*name).value }), ..method };
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
