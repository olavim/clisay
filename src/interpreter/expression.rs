use std::rc::Rc;

use crate::{lexer::SourcePosition, parser::{BinaryOperator, UnaryOperator}};

use super::{callable::Callable, environment::Environment, function::Function, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

#[derive(Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub pos: SourcePosition
}

impl Expression {
    pub fn new(kind: ExpressionKind, pos: SourcePosition) -> Expression {
        return Expression { kind, pos };
    }
}

#[derive(Clone)]
pub enum ExpressionKind {
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    MemberAccess(Box<Expression>, String, Option<SymbolId>),
    Identifier(SymbolId),
    This(SymbolId),
    Number(f64),
    String(String),
    Boolean(bool)
}

impl Evaluatable for Expression {
    fn evaluate(&self, env: &Rc<Environment>) -> EvalResult {
        return match &self.kind {
            ExpressionKind::Ternary(cond, left, right) => {
                match cond.evaluate(env)?.unwrap() {
                    Value::Boolean(true) => left.evaluate(env),
                    Value::Boolean(false) => right.evaluate(env),
                    val => Err(RuntimeException::new(format!("Expected boolean, got {}", val), left))
                }
            },
            ExpressionKind::Binary(op, left, right) => {
                let rval = right.evaluate(env)?.unwrap();
    
                if let BinaryOperator::Assign(None) = op {
                    match &left.kind {
                        ExpressionKind::Identifier(sid) => {
                            env.assign(sid.symbol, rval.clone());
                            return Ok(Some(rval));
                        },
                        ExpressionKind::MemberAccess(obj_expr, member, sid) => {
                            let obj = obj_expr.evaluate(env)?.unwrap();
                            match obj {
                                Value::Object(obj_ref) => {
                                    let symbol = match obj_ref.resolve_symbol(member, sid) {
                                        Some(symbol) => symbol,
                                        None => return Err(RuntimeException::new(format!("{} has no member {}", obj_ref.0.name, member), obj_expr))
                                    };
                                    obj_ref.assign(&symbol, rval.clone());
                                    return Ok(Some(rval));
                                },
                                _ => return Err(RuntimeException::new(format!("{} is not an object", obj), obj_expr))
                            }
                        },
                        _ => return Err(RuntimeException::new("Cannot assign to <>", left))
                    }
                }
    
                let lval = left.evaluate(env)?.unwrap();
                let result = match (op, lval.clone(), rval.clone()) {
                    (BinaryOperator::Plus, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                    (BinaryOperator::Minus, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
                    (BinaryOperator::Multiply, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
                    (BinaryOperator::Divide, Value::Number(l), Value::Number(r)) => Value::Number(l / r),
                    (BinaryOperator::LessThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l < r),
                    (BinaryOperator::LessThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l <= r),
                    (BinaryOperator::GreaterThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l > r),
                    (BinaryOperator::GreaterThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l >= r),
                    (BinaryOperator::BitAnd, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) & (r as i64)) as f64),
                    (BinaryOperator::BitOr, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) | (r as i64)) as f64),
                    (BinaryOperator::BitXor, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) ^ (r as i64)) as f64),
                    (BinaryOperator::LeftShift, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) << (r as i64)) as f64),
                    (BinaryOperator::RightShift, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) >> (r as i64)) as f64),
                    (BinaryOperator::LogicalAnd, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l && r),
                    (BinaryOperator::LogicalOr, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l || r),
                    (BinaryOperator::LogicalEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l == r),
                    (BinaryOperator::LogicalEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l == r),
                    (BinaryOperator::LogicalEqual, Value::String(l), Value::String(r)) => Value::Boolean(l == r),
                    (BinaryOperator::LogicalNotEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l != r),
                    (BinaryOperator::LogicalNotEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l != r),
                    (BinaryOperator::LogicalNotEqual, Value::String(l), Value::String(r)) => Value::Boolean(l != r),
                    _ => return Err(RuntimeException::new(format!("Invalid binary operation: {} {} {}", lval, op, rval), left))
                };
    
                return Ok(Some(result));
            },
            ExpressionKind::Unary(op, expr) => {
                let val = expr.evaluate(env)?.unwrap();
                let result = match (op, val.clone()) {
                    (UnaryOperator::Negative, Value::Number(n)) => Value::Number(-n),
                    (UnaryOperator::LogicalNot, Value::Boolean(b)) => Value::Boolean(!b),
                    (UnaryOperator::BitNot, Value::Number(n)) => Value::Number(!(n as i64) as f64),
                    _ => return Err(RuntimeException::new(format!("Invalid unary operation: {}{}", op, val), expr))
                };
                return Ok(Some(result));
            },
            ExpressionKind::Call(expr, args) => eval_call(env, expr, args),
            ExpressionKind::MemberAccess(expr, member, sid) => {
                let value = expr.evaluate(env)?.unwrap();
                let obj = match value.clone() {
                    Value::Object(obj_ref) => obj_ref,
                    _ => return Err(RuntimeException::new(format!("{} is not an object", value), expr))
                };
                let symbol = match obj.resolve_symbol(member, sid) {
                    Some(symbol) => symbol,
                    None => return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr))
                };
                
                if let Some(val) = obj.get_field(&symbol) {
                    return Ok(Some(val.clone()));
                }

                if let Some(method) = obj.get_method(&symbol) {
                    let closure = Rc::new(Environment::from(env));
                    closure.insert(method.0.symbol, Value::Object(obj.clone()));
                    let func = Function::new(format!("{}.{}", obj.0.name, member), method.1.clone(), method.2.clone());
                    return Ok(Some(Value::Function(closure, Rc::new(func))));
                }

                Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr))
            },
            ExpressionKind::Identifier(name) => Ok(env.get(&name.symbol)),
            ExpressionKind::This(sid) => match env.get(&sid.symbol) {
                Some(value) => Ok(Some(value.clone())),
                _ => Err(RuntimeException::new("this does not refer to an object", self))
            },
            ExpressionKind::Number(n) => Ok(Some(Value::Number(*n))),
            ExpressionKind::String(s) => Ok(Some(Value::String(s.clone()))),
            ExpressionKind::Boolean(b) => Ok(Some(Value::Boolean(*b)))
        };
    }
}

fn eval_call(env: &Rc<Environment>, expr: &Expression, args: &Vec<Expression>) -> EvalResult {
    let lval = match expr.evaluate(env) {
        Ok(Some(val)) => val,
        Ok(None) => return Err(RuntimeException::new("Cannot call <>", expr)),
        Err(err) => return Err(err)
    };

    let mut arg_values = Vec::new();
    for arg in args {
        arg_values.push(arg.evaluate(env)?.unwrap());
    }

    match lval {
        Value::Function(env_ref, func) => {
            let closure = Rc::new(Environment::from(&env_ref));
            return func.call(&closure, arg_values, expr);
        },
        Value::BuiltinFunction(env_ref, func) => {
            let closure = Rc::new(Environment::from(&env_ref));
            return func.call(&closure, arg_values, expr);
        },
        Value::Class(env_ref, class) => {
            let closure = Rc::new(Environment::from(&env_ref));
            return class.call(&closure, arg_values, expr);
        },
        _ => {}
    }

    return Err(RuntimeException::new(format!("{} is not callable", lval), expr));
}
