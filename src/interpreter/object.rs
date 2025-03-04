use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{class::Class, function::Function, resolver::SymbolId, value::Value};

pub struct Object(pub Rc<Class>, pub RefCell<HashMap<usize, Value>>);

impl Object {
    pub fn resolve_symbol(&self, member: impl Into<String>, sid: &Option<SymbolId>) -> Option<SymbolId> {
        match sid {
            Some(sid) => Some(sid.clone()),
            None => self.0.get_symbol(&member.into()).cloned()
        }
    }

    pub fn get_field(&self, symbol: &usize) -> Option<Value> {
        self.1.borrow().get(symbol).cloned()
    }

    pub fn get_method(&self, symbol: &usize) -> Option<&Function> {
        self.0.get_method(symbol)
    }

    pub fn assign(&self, sid: &SymbolId, value: Value) -> Result<(), String> {
        if self.0.get_method(&sid.symbol).is_some() {
            return Err(format!("Cannot assign to instance method '{}'", sid.name));
        }

        return match self.1.borrow_mut().get_mut(&sid.symbol) {
            Some(field) => {
                *field = value;
                Ok(())
            },
            None => Err(format!("Instance does not contain field '{}'", sid.name))
        }
    }

    pub fn insert_symbol(&self, symbol: &usize, value: Value) {
        self.1.borrow_mut().insert(*symbol, value);
    }
}
