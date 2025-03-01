use std::rc::Rc;

use crate::{lexer::SourcePosition, parser::{BinaryOperator, UnaryOperator}};

use super::{callable::Callable, environment::Environment, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

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
    This,
    Super,
    SuperCall(Vec<Expression>),
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
            ExpressionKind::Binary(BinaryOperator::Assign(_), left, right) => {
                let rval = right.evaluate(env)?.unwrap();
                match &left.kind {
                    ExpressionKind::Identifier(sid) => match env.assign(sid, rval.clone()) {
                        Ok(_) => return Ok(Some(rval)),
                        Err(err) => return Err(RuntimeException::new(err, self))
                    },
                    ExpressionKind::MemberAccess(obj_expr, member, sid) => match obj_expr.evaluate(env)?.unwrap() {
                        Value::Object(obj_ref) => {
                            let Some(sid) = obj_ref.resolve_symbol(member, sid) else {
                                return Err(RuntimeException::new(format!("{} has no member {}", obj_ref.0.name, member), obj_expr))
                            };

                            obj_ref.assign(&sid.symbol, rval.clone());
                            return Ok(Some(rval));
                        },
                        value => return Err(RuntimeException::new(format!("{} is not an object", value), obj_expr))
                    },
                    _ => return Err(RuntimeException::new("Invalid assignment", left))
                }
            },
            ExpressionKind::Binary(op, left, right) => {
                let rval = right.evaluate(env)?.unwrap();    
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
                let sid = match obj.resolve_symbol(member, sid) {
                    Some(symbol) => symbol,
                    None => return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr))
                };
                
                if let Some(val) = obj.get_field(&sid.symbol) {
                    return Ok(Some(val.clone()));
                }

                if let Some(method) = obj.get_method(&sid.symbol) {
                    let closure = Rc::new(Environment::from(env));
                    closure.insert(0, Value::Object(obj.clone()));
                    return Ok(Some(Value::Function(closure, Rc::new(method.clone()))));
                }

                Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr))
            },
            ExpressionKind::Identifier(sid) => match env.get(&sid.symbol) {
                Some(value) => Ok(Some(value.clone())),
                None => match env.get(&0) {
                    Some(Value::Object(obj)) => {
                        let sid = match obj.resolve_symbol(&sid.name, &Some(sid.clone())) {
                            Some(sid) => sid,
                            None => return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, sid.name), self))
                        };

                        if let Some(val) = obj.get_field(&sid.symbol) {
                            return Ok(Some(val.clone()));
                        }

                        if let Some(method) = obj.get_method(&sid.symbol) {
                            let closure = Rc::new(Environment::from(env));
                            closure.insert(0, Value::Object(obj.clone()));
                            return Ok(Some(Value::Function(closure, Rc::new(method.clone()))));
                        }

                        return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, sid.name), self));
                    },
                    _ => Err(RuntimeException::new(format!("{} is not defined", sid.name), self))
                }
            },
            ExpressionKind::This => match env.get(&0) {
                Some(value) => Ok(Some(value.clone())),
                _ => Err(RuntimeException::new("this does not refer to an object", self))
            },
            ExpressionKind::Super => match env.get(&0) {
                Some(value) => Ok(Some(value.clone())),
                _ => Err(RuntimeException::new("this does not refer to an object", self))
            },
            ExpressionKind::SuperCall(args) => {
                let Some(Value::Object(obj)) = env.get(&0) else {
                    unreachable!("super() called outside of class init method");
                };

                let Some(superclass_sid) = obj.resolve_symbol("super", &None) else {
                    return Err(RuntimeException::new("Cannot call super: no parent class", self));
                };

                let Some(Value::Class(superclass_env, superclass)) = env.get(&superclass_sid.symbol) else {
                    unreachable!("super does not refer to a class");
                };

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(arg.evaluate(env)?.unwrap());
                }

                let closure = Rc::new(Environment::from(&superclass_env));
                closure.insert(0, Value::Object(obj.clone()));
                superclass.init.call(&closure, arg_values, self)
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
