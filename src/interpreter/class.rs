use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{callable::Callable, environment::Environment, expression::Expression, function::Function, object::Object, resolver::SymbolId, value::Value, EvalResult};

#[derive(Clone)]
pub struct ClassDeclaration {
    pub superclass_sid: Option<SymbolId>,
    pub init: Rc<Function>,
    pub symbols: HashMap<String, usize>,
    pub methods: HashMap<usize, Rc<Function>>,
    pub fields: Vec<usize>,
}

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub decl: Rc<ClassDeclaration>,
    pub env: Rc<Environment>
}

impl Class {    
    pub fn get_symbol(&self, member: &str) -> Option<&usize> {
        match self.decl.symbols.get(member) {
            Some(symbol) => return Some(symbol),
            None => match &self.superclass {
                Some(superclass) => superclass.get_symbol(member),
                None => None
            }
        }
    }
}

impl Callable for Rc<Class> {
    fn call(&self, env: &Rc<Environment>, closure_env: &Rc<Environment>, args: &Vec<Expression>, expr: &Expression) -> EvalResult {
        let instance = Rc::new(Object(self.clone(), RefCell::new(HashMap::new())));

        let mut current_class = Some(&instance.0);
        while let Some(class) = &current_class {
            for symbol in &class.decl.fields {
                instance.insert_symbol(symbol, Value::Void);
            }
            current_class = class.superclass.as_ref();
        }

        let value = Value::Object(instance, 0);
        let closure = Rc::new(Environment::from(closure_env));
        closure.insert(0, value.clone());

        self.decl.init.call(env, &closure, args, expr)?;

        return Ok(Some(value));
    }
}
