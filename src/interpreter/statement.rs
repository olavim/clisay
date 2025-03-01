use std::{collections::HashMap, rc::Rc};

use super::{class::{Class, ClassDeclaration}, environment::Environment, expression::Expression, function::Function, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub enum Statement {
    Compound(Vec<Statement>),
    Expression(Expression),
    Return(Option<Expression>),
    Say(SymbolId, Option<Expression>),
    Fn(SymbolId, Vec<SymbolId>, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Class(SymbolId, ClassDeclaration)
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
                let func = Function::new(sid.clone(), params.clone(), body.clone());
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
            Statement::Class(sid, class_decl) => {
                let mut symbols: HashMap<String, SymbolId> = HashMap::new();
                let mut methods: HashMap<usize, Function> = HashMap::new();
                let mut fields: Vec<usize> = Vec::new();

                for field_sid in &class_decl.fields {
                    symbols.insert(field_sid.name.clone(), field_sid.clone());
                    fields.push(field_sid.symbol);
                }
                for method in &class_decl.methods {
                    symbols.insert(method.sid.name.clone(), method.sid.clone());
                    methods.insert(method.sid.symbol, method.clone());
                }

                let superclass = match &class_decl.superclass_sid {
                    Some(superclass_sid) => match env.get(&superclass_sid.symbol) {
                        Some(Value::Class(_, superclass)) => Some(superclass.clone()),
                        _ => unreachable!()
                    },
                    None => None
                };

                if let Some(sid) = &class_decl.superclass_sid {
                    symbols.insert(String::from("super"), sid.clone());
                }

                let class = Class {
                    name: sid.name.clone(),
                    superclass,
                    init: class_decl.init.clone(),
                    symbols,
                    fields,
                    methods
                };

                let closure = Rc::new(Environment::from(env));
                env.insert(sid.symbol, Value::Class(closure, Rc::new(class)));
                return Ok(None);
            }
        };
    }
}