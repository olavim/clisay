use std::{cell::RefCell, rc::Rc};

use super::{resolver::SymbolId, value::Value};

pub struct Environment {
    pub parent: Option<Rc<Environment>>,
    pub locals: RefCell<Vec<Value>>,
    depth: u32
}

impl Environment {
    pub fn new() -> Environment {
        let mut locals = Vec::with_capacity(10);
        locals.push(Value::Void);
        return Environment { parent: None, locals: RefCell::new(locals), depth: 0 };
    }

    pub fn from(parent: &Rc<Environment>) -> Environment {
        let mut locals = Vec::with_capacity(10);
        locals.push(Value::Void);
        return Environment { parent: Some(parent.clone()), locals: RefCell::new(locals), depth: parent.depth + 1 };
    }

    pub fn insert(&self, key: usize, value: Value) {
        if key >= self.locals.borrow().len() {
            self.locals.borrow_mut().resize(key + 1, Value::Void);
        }
        self.locals.borrow_mut()[key] = value;
    }

    pub fn assign(&self, sid: &SymbolId, value: Value) -> Result<(), String> {
        let mut obj = self;
        for _ in 0..(self.depth - sid.depth) {
            obj = obj.parent.as_ref().unwrap();
        }

        let mut locals = obj.locals.borrow_mut();

        match locals[sid.symbol] {
            Value::BuiltinFunction(_, _) => Err(format!("Invalid assignment: {} refers to a builtin function", sid.name)),
            Value::Class(_, _) => Err(format!("Invalid assignment: {} refers to a type", sid.name)),
            _ => {
                locals[sid.symbol] = value;
                Ok(())
            }
        }
    }

    pub fn get(&self, sid: &SymbolId) -> Value {
        // let depth = self.get_depth(sid);
        // println!("{}, {}: {}", sid.name, sid.depth, depth);

        let mut obj = self;
        for _ in 0..(self.depth - sid.depth) {
            obj = obj.parent.as_ref().unwrap();
        }

        return obj.locals.borrow()[sid.symbol].clone();
    }

    pub fn get_this(&self) -> Value {
        let mut obj = self;
        while let Value::Void = &obj.locals.borrow()[0] {
            if let Some(parent) = &obj.parent {
                obj = parent.as_ref();
            } else {
                return Value::Void;
            }
        }

        return obj.locals.borrow()[0].clone();
    }

    // fn get_depth(&self, sid: &SymbolId) -> u32 {
    //     if self.locals.borrow().contains_key(&sid.symbol) {
    //         return self.depth;
    //     }

    //     return match &self.parent {
    //         Some(parent) => parent.get_depth(sid),
    //         None => 999
    //     };
    // }
}