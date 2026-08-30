use crate::core::objects::{self, ObjInstance, ObjUpvalue};
use crate::core::value::Value;

use super::Vm;

pub(super) struct AcceptedSlotWrite { into: *mut Value, value: Value }
pub(super) struct AcceptedUpvalueWrite { upvalue: *mut ObjUpvalue, value: Value }
pub(super) struct AcceptedFieldWrite { instance: *mut ObjInstance, field: u8, value: Value }

impl Vm {
    #[inline]
    pub(super) fn accept_slot_write(&mut self, base: *mut Value, slot: u8, ip: *const u8, top: *mut Value, value: Value) -> Result<AcceptedSlotWrite, anyhow::Error> {
        // Nearly every value is one no destination can refuse, and settling that reads no memory.
        if objects::may_carry_witness(value) {
            self.stack.set_top(top);
            self.ip = ip;
            let accepts = self.slot_accepts_at(slot, self.code_index_at(ip));
            self.check_value_accepted(value, accepts)?;
        }
        Ok(AcceptedSlotWrite { into: unsafe { base.add(slot as usize) }, value })
    }

    #[inline]
    pub(super) fn write_slot(&mut self, accepted: AcceptedSlotWrite) {
        unsafe { *accepted.into = accepted.value };
    }

    pub(super) fn accept_upvalue_write(&mut self, upvalue: *mut ObjUpvalue, value: Value) -> Result<AcceptedUpvalueWrite, anyhow::Error> {
        if objects::may_carry_witness(value) {
            self.check_value_accepted(value, unsafe { (*upvalue).accepts })?;
        }
        Ok(AcceptedUpvalueWrite { upvalue, value })
    }

    pub(super) fn write_upvalue(&mut self, accepted: AcceptedUpvalueWrite) {
        objects::record_escape(accepted.value);
        unsafe { *(*accepted.upvalue).location = accepted.value };
    }

    #[inline]
    pub(super) fn accept_field_write(&mut self, instance: *mut ObjInstance, field: u8, value: Value) -> Result<AcceptedFieldWrite, anyhow::Error> {
        if objects::may_carry_witness(value) {
            let accepts = self.field_accepts(instance, field);
            self.check_value_accepted(value, accepts)?;
        }
        Ok(AcceptedFieldWrite { instance, field, value })
    }

    #[inline]
    pub(super) fn write_field(&mut self, accepted: AcceptedFieldWrite) {
        unsafe { &mut *accepted.instance }.set(accepted.field, accepted.value);
    }
}
