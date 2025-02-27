use std::{cell::RefCell, fmt, rc::Rc, time::SystemTime};

use super::{callable::Callable, environment::Environment, expression::Expression, resolver::SymbolId, statement::Statement, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<SymbolId>,
    pub body: Box<Statement>
}

impl Function {
    pub fn new(name: String, params: Vec<SymbolId>, body: Box<Statement>) -> Function {
        return Function { name, params, body };
    }
}

impl Callable for Function {
    fn call(&self, env: &Rc<Environment>, args: Vec<Value>, expr: &Expression) -> EvalResult {
        if self.params.len() != args.len() {
            let msg = format!("Expected {} arguments, got {}", self.params.len(), args.len());
            return Err(RuntimeException::new(msg, expr));
        }

        for (param, arg) in self.params.iter().zip(args) {
            env.insert(param.symbol, arg.clone());
        }

        match self.body.evaluate(env) {
            Ok(Some(value)) => return Ok(Some(value)),
            Ok(None) => return Ok(Some(Value::Void)),
            Err(e) => return Err(RuntimeException::from(&self.name, expr, e))
        };
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.name);
    }
}

#[derive(Clone)]
pub enum BuiltinFunction {
    Print(Rc<RefCell<Vec<String>>>),
    Time
}

impl Callable for BuiltinFunction {
    fn call(&self, _env: &Rc<Environment>, args: Vec<Value>, _expr: &Expression) -> EvalResult {
        return match &self {
            BuiltinFunction::Print(out) => {
                let value = format!("{}", &args[0]);
                println!("{}", value);
                out.as_ref().borrow_mut().push(value);
                Ok(Some(Value::Void))
            },
            BuiltinFunction::Time => {
                let time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64;
                Ok(Some(Value::Number(time)))
            }
        };
    }
}