use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{callable::Callable, environment::Environment, expression::Expression, function::Function, object::Object, resolver::SymbolId, value::Value, EvalResult};

#[derive(Clone)]
pub struct ClassDeclaration {
    pub super_sid: SymbolId,
    pub superclass_sid: Option<SymbolId>,
    pub init: Function,
    pub fields: Vec<SymbolId>,
    pub methods: Vec<Function>
}

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub init: Function,
    pub symbols: HashMap<String, SymbolId>,
    pub methods: HashMap<usize, Function>,
    pub fields: Vec<usize>
}

impl Class {
    pub fn get_method(&self, symbol: &usize) -> Option<&Function> {
        match self.methods.get(symbol) {
            Some(method) => return Some(method),
            None => match &self.superclass {
                Some(superclass) => superclass.get_method(symbol),
                None => None
            }
        }
    }
    
    pub fn get_symbol(&self, member: &String) -> Option<&SymbolId> {
        match self.symbols.get(member) {
            Some(sid) => return Some(sid),
            None => match &self.superclass {
                Some(superclass) => superclass.get_symbol(member),
                None => None
            }
        }
    }
}

impl Callable for Rc<Class> {
    fn call(&self, env: &Rc<Environment>, args: Vec<Value>, expr: &Expression) -> EvalResult {
        let instance = Rc::new(Object(self.clone(), RefCell::new(HashMap::new())));

        let mut current_class = Some(&instance.0);
        while let Some(class) = &current_class {
            for symbol in &class.fields {
                instance.insert_symbol(symbol, Value::Void);
            }
            current_class = class.superclass.as_ref();
        }

        let closure = Rc::new(Environment::from(env));
        closure.insert(0, Value::Object(instance.clone()));

        self.init.call(&closure, args, expr)?;

        return Ok(Some(Value::Object(instance)));
    }
}
