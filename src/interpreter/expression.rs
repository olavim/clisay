use std::rc::Rc;

use crate::{lexer::SourcePosition, parser::Operator};

use super::{callable::Callable, environment::Environment, object::Object, resolver::SymbolId, value::Value, EvalResult, Evaluatable, RuntimeException};

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
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    MemberAccess(Box<Expression>, String),
    Identifier(SymbolId),
    This(SymbolId),
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
                    val => Err(RuntimeException::new(format!("Expected boolean, got {}", val), self))
                }
            },
            ExpressionKind::Binary(Operator::Assign(_), left, right) => {
                let rval = right.evaluate(env)?.unwrap();
                match &left.kind {
                    ExpressionKind::Identifier(sid) => match env.assign(sid, rval.clone()) {
                        Ok(_) => return Ok(Some(rval)),
                        Err(err) => return Err(RuntimeException::new(err, self))
                    },
                    ExpressionKind::MemberAccess(obj_expr, member) => {
                        let value = obj_expr.evaluate(env)?.unwrap();
                        let Value::Object(obj, class) = value else {
                            return Err(RuntimeException::new(format!("{} is not an object", value), self));
                        };

                        let member_symbol = class.get_symbol(member).unwrap();

                        if let Err(err) = obj.assign(member_symbol, member, rval.clone()) {
                            return Err(RuntimeException::new(err, self));
                        }

                        return Ok(Some(rval));
                    },
                    _ => return Err(RuntimeException::new("Invalid assignment", left))
                }
            },
            ExpressionKind::Binary(op, left, right) => {
                let rval = right.evaluate(env)?.unwrap();    
                let lval = left.evaluate(env)?.unwrap();
                let result = match (op, lval.clone(), rval.clone()) {
                    (Operator::Plus, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                    (Operator::Minus, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
                    (Operator::Multiply, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
                    (Operator::Divide, Value::Number(l), Value::Number(r)) => Value::Number(l / r),
                    (Operator::LessThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l < r),
                    (Operator::LessThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l <= r),
                    (Operator::GreaterThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l > r),
                    (Operator::GreaterThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l >= r),
                    (Operator::BitAnd, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) & (r as i64)) as f64),
                    (Operator::BitOr, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) | (r as i64)) as f64),
                    (Operator::BitXor, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) ^ (r as i64)) as f64),
                    (Operator::LeftShift, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) << (r as i64)) as f64),
                    (Operator::RightShift, Value::Number(l), Value::Number(r)) => Value::Number(((l as i64) >> (r as i64)) as f64),
                    (Operator::LogicalAnd, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l && r),
                    (Operator::LogicalOr, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l || r),
                    (Operator::LogicalEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l == r),
                    (Operator::LogicalEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l == r),
                    (Operator::LogicalEqual, Value::String(l), Value::String(r)) => Value::Boolean(l == r),
                    (Operator::LogicalNotEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l != r),
                    (Operator::LogicalNotEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l != r),
                    (Operator::LogicalNotEqual, Value::String(l), Value::String(r)) => Value::Boolean(l != r),
                    _ => return Err(RuntimeException::new(format!("Operator '{}' cannot be applied to operands '{}' and '{}'", op, lval, rval), self))
                };
                return Ok(Some(result));
            },
            ExpressionKind::Unary(op, expr) => {
                let val = expr.evaluate(env)?.unwrap();
                let result = match (op, val.clone()) {
                    (Operator::Negative, Value::Number(n)) => Value::Number(-n),
                    (Operator::LogicalNot, Value::Boolean(b)) => Value::Boolean(!b),
                    (Operator::BitNot, Value::Number(n)) => Value::Number(!(n as i64) as f64),
                    _ => return Err(RuntimeException::new(format!("Invalid unary operation: {}{}", op, val), self))
                };
                return Ok(Some(result));
            },
            ExpressionKind::Call(expr, args) => eval_call(env, expr, args),
            ExpressionKind::MemberAccess(expr, member) => {
                let value = expr.evaluate(env)?.unwrap();
                let Value::Object(obj, class) = &value else {
                    return Err(RuntimeException::new(format!("{} is not an object", value), expr))
                };

                let symbol = match class.get_symbol(member) {
                    Some(symbol) => symbol,
                    None => return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), self))
                };
                
                if let Some(val) = obj.get_field(&symbol) {
                    return Ok(Some(val.clone()));
                }

                if let Some(method) = Object::get_method(obj, &symbol) {
                    return Ok(Some(method));
                }

                Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr))
            },
            ExpressionKind::Identifier(sid) => Ok(Some(env.get(&sid))),
            ExpressionKind::This(class_sid) => match (env.get_this(), env.get(class_sid)) {
                (Value::Object(obj, _), Value::Class(_, class)) => Ok(Some(Value::Object(obj, class))),
                _ => Err(RuntimeException::new("this does not refer to an object", self))
            },
            ExpressionKind::Super => match env.get_this() {
                Value::Object(obj, class) => Ok(Some(Value::Object(obj, class.superclass.clone().unwrap()))),
                _ => unreachable!("super used outside of class")
            },
            ExpressionKind::SuperCall(args) => {
                let Value::Object(obj, class) = env.get_this() else {
                    unreachable!("super() called outside of class init method");
                };

                let Some(superclass) = &class.superclass else {
                    unreachable!("super does not refer to a class");
                };

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(arg.evaluate(env)?.unwrap());
                }

                let closure = Rc::new(Environment::from(&env));
                closure.insert(0, Value::Object(obj.clone(), superclass.clone()));
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
        Value::Function(env_ref, func) => func.call(&env_ref, arg_values, expr),
        Value::BuiltinFunction(env_ref, func) => {
            let closure = Rc::new(Environment::from(&env_ref));
            return func.call(&closure, arg_values, expr);
        },
        Value::Class(env_ref, class) => class.call(&env_ref, arg_values, expr),
        _ => return Err(RuntimeException::new(format!("{} is not callable", lval), expr))
    }
}
