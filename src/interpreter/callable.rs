use std::rc::Rc;

use super::{environment::Environment, expression::Expression, value::Value, EvalResult};

pub trait Callable {
    fn call(&self, env: &Rc<Environment>, args: Vec<Value>, expr: &Expression) -> EvalResult;
}
