use crate::core::objects::ObjectKind;
use crate::core::value::{Value, ValueKind};
use crate::middle::ir::{self, NULL_WITNESS_ID};

use super::{throw_value, Vm};

impl Vm {
    pub(super) fn read_witness_set_pool_id(&mut self) -> u16 {
        u16::from_le_bytes([self.read_next(), self.read_next()])
    }

    pub(super) fn accepts_null(&self, witness_set_pool_id: u16) -> bool {
        self.chunk.witness_set_pool[witness_set_pool_id as usize].first() == Some(&NULL_WITNESS_ID)
    }

    pub(super) fn check_value_accepted(&mut self, value: Value, witness_set_pool_id: u16) -> Result<(), anyhow::Error> {
        if self.accepts_value(value, witness_set_pool_id) {
            return Ok(());
        }
        match value.is_null() {
            true => self.error("unexpected null"),
            false => throw_value(value),
        }
    }

    pub(super) fn accepts_value(&self, value: Value, witness_set_pool_id: u16) -> bool {
        if witness_set_pool_id == ir::NO_WITNESS_SET {
            return true;
        }
        match value.is_null() {
            true => self.accepts_null(witness_set_pool_id),
            false => !self.carries_disallowed_witness(value, witness_set_pool_id),
        }
    }

    pub(super) fn carries_disallowed_witness(&self, value: Value, witness_set_pool_id: u16) -> bool {
        let ValueKind::Object(ObjectKind::Instance) = value.kind() else { return false };
        let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
        if ty.witness_ids.is_empty() {
            return false;
        }
        let witness_set = &self.chunk.witness_set_pool[witness_set_pool_id as usize];
        ty.witness_ids.iter().any(|id| !witness_set.contains(id))
    }
}
