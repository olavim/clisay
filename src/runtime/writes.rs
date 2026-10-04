use crate::core::objects::{self, ObjInstance};
use crate::core::value::Value;

use super::Vm;

pub(super) struct AcceptedSlotWrite { into: *mut Value, pub(super) value: Value }
pub(super) struct AcceptedFieldWrite { instance: *mut ObjInstance, field: u8, pub(super) value: Value }

impl Vm {
    #[inline]
    pub(super) fn check_slot_accepts(&mut self, slot: u8, ip: *const u8, top: *mut Value, value: Value) -> Result<(), anyhow::Error> {
        match objects::needs_accepts_check(value) {
            true => self.check_slot_accepts_value(slot, ip, top, value),
            false => Ok(()),
        }
    }

    #[inline(never)]
    fn check_slot_accepts_value(&mut self, slot: u8, ip: *const u8, top: *mut Value, value: Value) -> Result<(), anyhow::Error> {
        self.stack.set_top(top);
        self.ip = ip;
        let witness_set_pool_id = self.slot_witness_set_pool_id_at(slot, self.code_index_at(ip));
        self.check_value_accepted(value, witness_set_pool_id)
    }

    pub(super) fn accept_slot_write(&mut self, stack_start: *mut Value, slot: u8, ip: *const u8, top: *mut Value, value: Value) -> Result<AcceptedSlotWrite, anyhow::Error> {
        self.check_slot_accepts(slot, ip, top, value)?;

        let value = self.share(value);
        Ok(AcceptedSlotWrite { into: unsafe { stack_start.add(slot as usize) }, value })
    }

    #[inline]
    pub(super) fn write_slot(&mut self, accepted: AcceptedSlotWrite) {
        unsafe { *accepted.into = accepted.value };
    }

    pub(super) fn accept_anchor_write(&mut self, anchor: Value, value: Value) -> Result<AcceptedSlotWrite, anyhow::Error> {
        if objects::needs_accepts_check(value) {
            self.check_value_accepted(value, anchor.anchor_witness_set_pool_id())?;
        }
        let value = self.share(value);
        Ok(AcceptedSlotWrite { into: self.anchor_slot_addr(anchor), value })
    }

    #[inline]
    pub(super) fn accept_field_write(&mut self, instance: *mut ObjInstance, field: u8, value: Value) -> Result<AcceptedFieldWrite, anyhow::Error> {
        if objects::needs_accepts_check(value) {
            debug_assert!(!value.is_anchor(), "an anchor never reaches a field");
            let witness_set_pool_id = self.field_witness_set_pool_id(instance, field);
            self.check_value_accepted(value, witness_set_pool_id)?;
        }
        let value = self.share_into(Value::from(instance), value);
        Ok(AcceptedFieldWrite { instance, field, value })
    }

    #[inline]
    pub(super) fn write_field(&mut self, accepted: AcceptedFieldWrite) {
        unsafe { ObjInstance::set(accepted.instance, accepted.field, accepted.value) };
    }
}
