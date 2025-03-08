use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{class::Class, environment::Environment, function::Function, resolver::SymbolId, value::Value};

pub struct Object(pub Rc<Class>, pub RefCell<HashMap<usize, Value>>);

impl Object {
    pub fn get_method(obj: &Rc<Object>, symbol: &usize) -> Option<Value> {
        match obj.get_class_method(symbol) {
            Some((class, method)) => {
                let closure = Rc::new(Environment::from(&class.env));
                closure.insert(0, Value::Object(obj.clone(), class.clone()));
                Some(Value::Function(closure, Rc::new(method.clone())))
            },
            None => None
        }
    }

    pub fn get_field(&self, symbol: &usize) -> Option<Value> {
        self.1.borrow().get(symbol).cloned()
    }

    fn get_class_method(&self, symbol: &usize) -> Option<(&Rc<Class>, &Function)> {
        let mut current_class = Some(&self.0);
        while let Some(class) = current_class {
            match class.methods.get(symbol) {
                Some(method) => return Some((class, method)),
                None => current_class = class.superclass.as_ref()
            }
        }
        None
    }

    pub fn assign(&self, symbol: &usize, member_name: &String, value: Value) -> Result<(), String> {
        if self.get_class_method(&symbol).is_some() {
            return Err(format!("Cannot assign to instance method '{}'", member_name));
        }

        return match self.1.borrow_mut().get_mut(&symbol) {
            Some(field) => {
                *field = value;
                Ok(())
            },
            None => Err(format!("Instance does not contain field '{}'", member_name))
        }
    }

    pub fn insert_symbol(&self, symbol: &usize, value: Value) {
        self.1.borrow_mut().insert(*symbol, value);
    }
}
