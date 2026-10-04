use crate::core::gc::Gc;
use crate::core::host::Host;

use crate::core::objects::{self, NativeFn, ObjNativeFn, ObjString};
use crate::core::value::{DictKey, Value};

use super::{NativeType, NativeTypeBuilder};

pub struct NativeDict;

impl NativeDict {
    fn size(host: &mut dyn Host, target: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        host.push(Value::from(dict.entries.len() as f64));
        Ok(())
    }

    fn contains_key(host: &mut dyn Host, target: Value, key: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        host.push(if dict.entries.contains_key(&DictKey(key)) { Value::TRUE } else { Value::FALSE });
        Ok(())
    }

    fn remove(host: &mut dyn Host, target: Value, key: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &mut *target.as_object().as_dict_ptr() };
        let mut removed = dict.entries.remove(&DictKey(key)).unwrap_or(Value::NULL);
        objects::detach_replaced(&dict.header, || removed);
        if objects::is_on_anchor_path(removed) {
            removed = host.share(removed);
        }
        host.push(removed);
        Ok(())
    }
}

impl NativeTypeBuilder for NativeDict {
    fn kind(&self) -> NativeType {
        NativeType::Dict
    }

    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)> {
        let size = gc.intern("size");
        let contains_key = gc.intern("containsKey");
        let remove = gc.intern("remove");
        vec![
            (size, ObjNativeFn::new(size, 0, (|host, target, _args| Self::size(host, target)) as NativeFn)),
            (contains_key, ObjNativeFn::new(contains_key, 1, (|host, target, args| Self::contains_key(host, target, args[0])) as NativeFn)),
            (remove, ObjNativeFn::new(remove, 1, (|host, target, args| Self::remove(host, target, args[0])) as NativeFn)),
        ]
    }
}
