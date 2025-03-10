use std::{cell::RefCell, fmt, rc::Rc, time::SystemTime};

use super::{callable::Callable, environment::Environment, expression::Expression, resolver::SymbolId, statement::Statement, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub struct Function {
    pub sid: SymbolId,
    pub params: Vec<SymbolId>,
    pub body: Box<Statement>
}

impl Function {
    pub fn new(sid: SymbolId, params: Vec<SymbolId>, body: Box<Statement>) -> Function {
        return Function { sid, params, body };
    }
}

impl Callable for Function {
    fn call(&self, env: &Rc<Environment>, closure_env: &Rc<Environment>, args: &Vec<Expression>, expr: &Expression) -> EvalResult {
        if self.params.len() != args.len() {
            let msg = format!("{} expects {} arguments, but got {}", self.sid.name, self.params.len(), args.len());
            return Err(RuntimeException::new(msg, expr));
        }

        let closure = Rc::new(Environment::from(closure_env));
        for (param, arg) in self.params.iter().zip(args) {
            let value = arg.evaluate(env)?.unwrap();
            closure.insert(param.symbol, value);
        }

        match self.body.evaluate(&closure)? {
            Some(value) => return Ok(Some(value)),
            None => return Ok(Some(Value::Void))
        };
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.sid.name);
    }
}

#[derive(Clone)]
pub enum BuiltinFunction {
    Print(Rc<RefCell<Vec<String>>>),
    Time
}

impl Callable for BuiltinFunction {
    fn call(&self, env: &Rc<Environment>, _closure_env: &Rc<Environment>, args: &Vec<Expression>, _expr: &Expression) -> EvalResult {
        return match &self {
            BuiltinFunction::Print(out) => {
                let value = args[0].evaluate(env)?.unwrap();
                let value_str = format!("{}", value.stringify());
                println!("{}", value_str);
                out.as_ref().borrow_mut().push(value_str);
                Ok(Some(Value::Void))
            },
            BuiltinFunction::Time => {
                let time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64;
                Ok(Some(Value::Number(time)))
            }
        };
    }
}