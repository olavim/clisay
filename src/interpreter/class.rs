use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{callable::Callable, environment::Environment, expression::Expression, function::Function, object::Object, resolver::SymbolId, statement::Statement, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub struct ClassInit {
    pub this_sid: SymbolId,
    pub superclass: Option<Rc<Class>>,
    pub super_args: Vec<Expression>,
    pub func: Function
}

#[derive(Clone)]
pub struct ClassMethod(pub SymbolId, pub Vec<SymbolId>, pub Box<Statement>);

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<Class>>,
    pub init: ClassInit,
    pub symbols: HashMap<String, usize>,
    pub methods: HashMap<usize, ClassMethod>,
    pub fields: Vec<usize>
}

impl Class {
    pub fn get_method(&self, symbol: &usize) -> Option<&ClassMethod> {
        match self.methods.get(symbol) {
            Some(method) => return Some(method),
            None => match &self.superclass {
                Some(superclass) => superclass.get_method(symbol),
                None => None
            }
        }
    }
    
    pub fn get_symbol(&self, member: &String) -> Option<&usize> {
        match self.symbols.get(member) {
            Some(method) => return Some(method),
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
        closure.insert(self.init.this_sid.symbol, Value::Object(instance.clone()));
        self.init.call(&closure, args, expr)?;

        return Ok(Some(Value::Object(instance)));
    }
}

impl Callable for ClassInit {
    fn call(&self, env: &Rc<Environment>, args: Vec<Value>, expr: &Expression) -> EvalResult {
        let instance = match env.get(&self.this_sid.symbol) {
            Some(Value::Object(obj_ref)) => obj_ref.clone(),
            _ => unreachable!("this is not an object")
        };

        if let Some(superclass) = &self.superclass {
            let super_args = eval_args(env, &self.super_args)?;
            let env_ref = Rc::new(Environment::from(env));
            env_ref.insert(superclass.init.this_sid.symbol, Value::Object(instance.clone()));
            superclass.init.call(&env_ref, super_args, expr)?;
        }

        return self.func.call(&env, args, expr);
    }
}

fn eval_args(env: &Rc<Environment>, args: &Vec<Expression>) -> Result<Vec<Value>, RuntimeException> {
    let mut arg_values = Vec::new();
    for arg in args {
        match arg.evaluate(env)? {
            Some(value) => arg_values.push(value),
            None => arg_values.push(Value::Void)
        }
    }
    return Ok(arg_values);
}