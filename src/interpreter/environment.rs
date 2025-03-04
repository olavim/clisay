use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{resolver::SymbolId, value::Value};

pub struct Environment {
    pub parent: Option<Rc<Environment>>,
    pub locals: RefCell<HashMap<usize, Value>>
}

impl Environment {
    pub fn new() -> Environment {
        return Environment { parent: None, locals: RefCell::new(HashMap::new()) };
    }

    pub fn from(parent: &Rc<Environment>) -> Environment {
        return Environment { parent: Some(parent.clone()), locals: RefCell::new(HashMap::new()) };
    }

    pub fn insert(&self, key: usize, value: Value) {
        self.locals.borrow_mut().insert(key, value);
    }

    pub fn assign(&self, sid: &SymbolId, value: Value) -> Result<(), String> {
        let mut locals = self.locals.borrow_mut();

        match locals.get(&sid.symbol) {
            Some(Value::BuiltinFunction(_, _)) => Err(format!("Invalid assignment: {} refers to a builtin function", sid.name)),
            Some(Value::Class(_, _)) => Err(format!("Invalid assignment: {} refers to a type", sid.name)),
            Some(_) => {
                locals.insert(sid.symbol, value);
                Ok(())
            },
            None => {
                if let Some(env) = &self.parent {
                    return env.assign(sid, value);
                }
                Ok(())
            }
        }
    }

    pub fn get(&self, key: &usize) -> Option<Value> {
        return self.locals.borrow().get(key).cloned().or_else(|| self.parent.as_ref().and_then(|parent| parent.as_ref().get(key)));
    }
}