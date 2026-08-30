use crate::core::objects::ObjectKind;
use crate::core::value::{Value, ValueKind};
use crate::middle::ir::{self, NULL_WITNESS_ID};

use super::Vm;

impl Vm {
    pub(super) fn read_allowed_witnesses(&mut self) -> u16 {
        u16::from_le_bytes([self.read_next(), self.read_next()])
    }

    pub(super) fn accepts_null(&self, allowed: u16) -> bool {
        self.chunk.witness_allows[allowed as usize].first() == Some(&NULL_WITNESS_ID)
    }

    pub(super) fn check_value_accepted(&mut self, value: Value, allowed: u16) -> Result<(), anyhow::Error> {
        if self.accepts_value(value, allowed) {
            return Ok(());
        }
        match value.is_null() {
            true => self.error("unexpected null"),
            false => self.throw_value(value),
        }
    }

    pub(super) fn accepts_value(&self, value: Value, allowed: u16) -> bool {
        if allowed == ir::SLOT_ACCEPTS_ANYTHING {
            return true;
        }
        match value.is_null() {
            true => self.accepts_null(allowed),
            false => !self.carries_disallowed_witness(value, allowed),
        }
    }

    pub(super) fn carries_disallowed_witness(&self, value: Value, allowed: u16) -> bool {
        let ValueKind::Object(ObjectKind::Instance) = value.kind() else { return false };
        let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
        if ty.witness_ids.is_empty() {
            return false;
        }
        let allow = &self.chunk.witness_allows[allowed as usize];
        ty.witness_ids.iter().any(|id| !allow.contains(id))
    }
}
