use std::rc::Rc;

use super::{class::Class, environment::Environment, expression::Expression, function::Function, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub enum Statement {
    Compound(Vec<Statement>),
    Expression(Expression),
    Return(Option<Expression>),
    Say(SymbolId, Option<Expression>),
    Fn(SymbolId, Vec<SymbolId>, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Class(SymbolId, Rc<Class>)
}

impl Evaluatable for Statement {
    fn evaluate(&self, env: &Rc<Environment>) -> EvalResult {
        return match self {
            Statement::Compound(statements) => {
                let mut result = None;
                for statement in statements {
                    result = statement.evaluate(env)?;
                    if result.is_some() {
                        break;
                    }
                }
                return Ok(result);
            },
            Statement::Expression(expr) => {
                expr.evaluate(env)?;
                Ok(None)
            },
            Statement::Return(expr) => {
                return match expr {
                    Some(expr) => expr.evaluate(env),
                    None => Ok(Some(Value::Void))
                };
            },
            Statement::Say(sid, expr) => {
                let value = match expr {
                    Some(expr) => expr.evaluate(env)?.unwrap(),
                    None => Value::Void
                };
                env.insert(sid.symbol, value.clone());
                return Ok(None);
            },
            Statement::Fn(sid, params, body) => {
                let func = Function::new(sid.name.clone(), params.clone(), body.clone());
                let closure = Rc::new(Environment::from(env));
                env.insert(sid.symbol, Value::Function(closure, Rc::new(func)));
                return Ok(None);
            },
            Statement::If(expr, then, otherwise) => {
                match expr.evaluate(env)?.unwrap() {
                    Value::Boolean(true) => then.evaluate(env),
                    Value::Boolean(false) => match otherwise {
                        Some(otherwise) => otherwise.evaluate(env),
                        None => Ok(None)
                    },
                    val => Err(RuntimeException::new(format!("Expected boolean, got {}", val), expr))
                }
            },
            Statement::While(cond, body) => {
                loop {
                    match cond.evaluate(env)?.unwrap() {
                        Value::Boolean(true) => {},
                        Value::Boolean(false) => break,
                        val => return Err(RuntimeException::new(format!("Expected boolean, got {}", val), cond))
                    }

                    if let Some(val) = body.evaluate(env)? {
                        return Ok(Some(val));
                    }
                }
                return Ok(None);
            },
            Statement::Class(sid, class) => {
                let closure = Rc::new(Environment::from(env));
                env.insert(sid.symbol, Value::Class(closure, class.clone()));
                return Ok(None);
            }
        };
    }
}