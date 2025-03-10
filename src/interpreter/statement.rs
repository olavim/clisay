use std::{collections::HashMap, rc::Rc};

use super::{class::{Class, ClassDeclaration}, environment::Environment, expression::Expression, function::Function, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub enum Statement {
    Block(Box<Statement>),
    Compound(Vec<Statement>),
    Expression(Expression),
    Return(Option<Expression>),
    Say(SymbolId, Option<Expression>),
    Fn(Rc<Function>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Class(SymbolId, ClassDeclaration)
}

impl Evaluatable for Statement {
    fn evaluate(&self, env: &Rc<Environment>) -> EvalResult {
        return match self {
            Statement::Block(statement) => {
                let closure = Rc::new(Environment::from(env));
                return Ok(statement.evaluate(&closure)?);
            },
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
                env.insert(sid.symbol, value);
                return Ok(None);
            },
            Statement::Fn(func) => {
                env.insert(func.sid.symbol, Value::Function(env.clone(), func.clone()));
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
            Statement::Class(sid, class_decl) => {
                let superclass = match &class_decl.superclass_sid {
                    Some(superclass_sid) => match env.get(superclass_sid) {
                        Value::Class(_, superclass) => Some(superclass),
                        _ => unreachable!()
                    },
                    None => None
                };

                let class = Class {
                    name: sid.name.clone(),
                    superclass,
                    decl: Rc::new(class_decl.clone()),
                    env: env.clone()
                };

                let closure = Rc::new(Environment::from(env));
                env.insert(sid.symbol, Value::Class(closure, Rc::new(class)));
                return Ok(None);
            }
        };
    }
}