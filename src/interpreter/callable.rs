use std::rc::Rc;

use super::{environment::Environment, expression::Expression, EvalResult};

pub trait Callable {
    fn call(&self, env: &Rc<Environment>, closure_env: &Rc<Environment>, args: &Vec<Expression>, expr: &Expression) -> EvalResult;
}
