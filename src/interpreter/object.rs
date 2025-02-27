use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{class::{Class, ClassMethod}, resolver::SymbolId, value::Value};

pub struct Object(pub Rc<Class>, pub RefCell<HashMap<usize, Value>>);

impl Object {
    pub fn resolve_symbol(&self, member: &String, sid: &Option<SymbolId>) -> Option<usize> {
        match sid {
            Some(sid) => Some(sid.symbol),
            None => self.0.get_symbol(&member).cloned()
        }
    }

    pub fn get_field(&self, symbol: &usize) -> Option<Value> {
        self.1.borrow().get(symbol).cloned()
    }

    pub fn get_method(&self, symbol: &usize) -> Option<&ClassMethod> {
        self.0.get_method(symbol)
    }

    pub fn assign(&self, symbol: &usize, value: Value) {
        let _ = self.1.borrow_mut().insert(*symbol, value).unwrap().clone();
    }

    pub fn insert_symbol(&self, symbol: &usize, value: Value) {
        self.1.borrow_mut().insert(*symbol, value);
    }
}
