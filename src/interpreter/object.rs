use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{class::Class, environment::Environment, function::Function, value::Value};

pub struct Object(pub Rc<Class>, pub RefCell<HashMap<usize, Value>>);

impl Object {
    pub fn get_method(obj: &Rc<Object>, symbol: &usize) -> Option<Value> {
        match obj.get_class_method(symbol) {
            Some((method, class, class_depth)) => {
                let closure = Environment::from(&class.env);
                closure.insert(0, Value::Object(obj.clone(), class_depth));
                Some(Value::Function(Rc::new(closure), method.clone()))
            },
            None => None
        }
    }

    pub fn get_class(&self, depth: usize) -> &Rc<Class> {
        let mut current_class = &self.0;
        for _ in 0..depth {
            current_class = current_class.superclass.as_ref().unwrap();
        }
        current_class
    }

    pub fn get_field(&self, symbol: &usize) -> Option<Value> {
        self.1.borrow().get(symbol).cloned()
    }

    fn get_class_method(&self, symbol: &usize) -> Option<(&Rc<Function>, &Rc<Class>, usize)> {
        let mut depth = 0;
        let mut current_class = Some(&self.0);
        while let Some(class) = current_class {
            match class.decl.methods.get(symbol) {
                Some(method) => return Some((method, class, depth)),
                None => current_class = class.superclass.as_ref()
            }
            depth += 1;
        }
        None
    }

    pub fn assign(&self, symbol: &usize, member_name: &str, value: Value) -> Result<(), String> {
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
