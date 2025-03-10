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
    This,
    Super,
    SuperCall(Vec<Expression>),
    Number(f64),
    String(String),
    Boolean(bool),
    Null
}

impl Evaluatable for Expression {
    fn evaluate(&self, env: &Rc<Environment>) -> EvalResult {
        return match &self.kind {
            ExpressionKind::Ternary(cond, left, right) => eval_ternary(env, cond, left, right),
            ExpressionKind::Binary(Operator::Assign(_), left, right) => eval_assignment(env, left, right),
            ExpressionKind::Binary(op, left, right) => eval_binary(env, op, left, right),
            ExpressionKind::Unary(op, expr) => eval_unary(env, op, expr),
            ExpressionKind::Call(expr, args) => eval_call(env, expr, args),
            ExpressionKind::MemberAccess(expr, member) => eval_member_access(env, expr, member),
            ExpressionKind::Identifier(sid) => Ok(Some(env.get(&sid))),
            ExpressionKind::This => eval_this(env),
            ExpressionKind::Super => eval_super(env, self),
            ExpressionKind::SuperCall(args) => eval_supercall(env, args, self),
            ExpressionKind::Number(n) => Ok(Some(Value::Number(*n))),
            ExpressionKind::String(s) => Ok(Some(Value::String(s.clone()))),
            ExpressionKind::Boolean(b) => Ok(Some(Value::Boolean(*b))),
            ExpressionKind::Null => Ok(Some(Value::Void))
        };
    }
}

fn eval_ternary(env: &Rc<Environment>, cond: &Expression, left: &Expression, right: &Expression) -> EvalResult {
    match cond.evaluate(env)?.unwrap() {
        Value::Boolean(true) => left.evaluate(env),
        Value::Boolean(false) => right.evaluate(env),
        val => Err(RuntimeException::new(format!("Expected boolean, got {}", val), cond))
    }
}

fn eval_binary(env: &Rc<Environment>, op: &Operator, left: &Expression, right: &Expression) -> EvalResult {
    let rval = &right.evaluate(env)?.unwrap();    
    let lval = &left.evaluate(env)?.unwrap();
    let result = match (op, lval, rval) {
        (Operator::Plus, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
        (Operator::Minus, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        (Operator::Multiply, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
        (Operator::Divide, Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        (Operator::LessThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l < r),
        (Operator::LessThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l <= r),
        (Operator::GreaterThan, Value::Number(l), Value::Number(r)) => Value::Boolean(l > r),
        (Operator::GreaterThanEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l >= r),
        (Operator::BitAnd, Value::Number(l), Value::Number(r)) => Value::Number(((*l as i64) & (*r as i64)) as f64),
        (Operator::BitOr, Value::Number(l), Value::Number(r)) => Value::Number(((*l as i64) | (*r as i64)) as f64),
        (Operator::BitXor, Value::Number(l), Value::Number(r)) => Value::Number(((*l as i64) ^ (*r as i64)) as f64),
        (Operator::LeftShift, Value::Number(l), Value::Number(r)) => Value::Number(((*l as i64) << (*r as i64)) as f64),
        (Operator::RightShift, Value::Number(l), Value::Number(r)) => Value::Number(((*l as i64) >> (*r as i64)) as f64),
        (Operator::LogicalAnd, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(*l && *r),
        (Operator::LogicalOr, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(*l || *r),
        (Operator::LogicalEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l == r),
        (Operator::LogicalEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l == r),
        (Operator::LogicalEqual, Value::String(l), Value::String(r)) => Value::Boolean(l == r),
        (Operator::LogicalEqual, Value::Void, Value::Void) => Value::Boolean(true),
        (Operator::LogicalEqual, Value::Void, _) => Value::Boolean(false),
        (Operator::LogicalEqual, _, Value::Void) => Value::Boolean(false),
        (Operator::LogicalNotEqual, Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l != r),
        (Operator::LogicalNotEqual, Value::Number(l), Value::Number(r)) => Value::Boolean(l != r),
        (Operator::LogicalNotEqual, Value::String(l), Value::String(r)) => Value::Boolean(l != r),
        (Operator::LogicalNotEqual, Value::Void, Value::Void) => Value::Boolean(false),
        (Operator::LogicalNotEqual, Value::Void, _) => Value::Boolean(true),
        (Operator::LogicalNotEqual, _, Value::Void) => Value::Boolean(true),
        _ => return Err(RuntimeException::new(format!("Operator '{}' cannot be applied to operands '{}' and '{}'", op, lval, rval), left))
    };
    return Ok(Some(result));
}

fn eval_assignment(env: &Rc<Environment>, left: &Expression, right: &Expression) -> EvalResult {
    let rval = right.evaluate(env)?.unwrap();
    match &left.kind {
        ExpressionKind::Identifier(sid) => match env.assign(sid, rval.clone()) {
            Ok(_) => return Ok(Some(rval)),
            Err(err) => return Err(RuntimeException::new(err, left))
        },
        ExpressionKind::MemberAccess(obj_expr, member) => {
            let value = &obj_expr.evaluate(env)?.unwrap();
            let Value::Object(obj, class_depth) = value else {
                return Err(RuntimeException::new(format!("{} is not an object", value), left));
            };

            let class = obj.get_class(*class_depth);
            let member_symbol = class.get_symbol(member).unwrap();

            if let Err(err) = obj.assign(member_symbol, member, rval.clone()) {
                return Err(RuntimeException::new(err, left));
            }

            return Ok(Some(rval));
        },
        _ => return Err(RuntimeException::new("Invalid assignment", left))
    }
}

fn eval_unary(env: &Rc<Environment>, op: &Operator, expr: &Expression) -> EvalResult {
    let val = &expr.evaluate(env)?.unwrap();
    let result = match (op, val) {
        (Operator::Negative, Value::Number(n)) => Value::Number(-(*n)),
        (Operator::LogicalNot, Value::Boolean(b)) => Value::Boolean(!(*b)),
        (Operator::BitNot, Value::Number(n)) => Value::Number(!(*n as i64) as f64),
        _ => return Err(RuntimeException::new(format!("Invalid unary operation: {}{}", op, val), expr))
    };
    return Ok(Some(result));
}

fn eval_member_access(env: &Rc<Environment>, expr: &Expression, member: &str) -> EvalResult {
    let value = &expr.evaluate(env)?.unwrap();
    let Value::Object(obj, class_depth) = value else {
        return Err(RuntimeException::new(format!("{} is not an object", value), expr));
    };

    let class = obj.get_class(*class_depth);
    let symbol = class.get_symbol(member).unwrap();
    if let Some(val) = obj.get_field(&symbol) {
        return Ok(Some(val));
    }

    if let Some(method) = Object::get_method(obj, &symbol) {
        return Ok(Some(method));
    }

    return Err(RuntimeException::new(format!("{} has no member {}", obj.0.name, member), expr));
}

fn eval_this(env: &Rc<Environment>) -> EvalResult {
    return Ok(Some(env.get_this()));
}

fn eval_super(env: &Rc<Environment>, expr: &Expression) -> EvalResult {
    return match env.get_this() {
        Value::Object(obj, class_depth) => Ok(Some(Value::Object(obj, class_depth + 1))),
        _ => Err(RuntimeException::new("super used outside of class", expr))
    };
}

fn eval_supercall(env: &Rc<Environment>, args: &Vec<Expression>, expr: &Expression) -> EvalResult {
    let Value::Object(obj, class_depth) = env.get_this() else {
        return Err(RuntimeException::new("super() called outside of class init method", expr));
    };

    let superclass = obj.get_class(class_depth + 1);
    let closure = Rc::new(Environment::from(&env));
    closure.insert(0, Value::Object(obj.clone(), class_depth + 1));
    return superclass.decl.init.call(env, &closure, args, expr);
}

fn eval_call(env: &Rc<Environment>, expr: &Expression, args: &Vec<Expression>) -> EvalResult {
    let lval = match expr.evaluate(env)? {
        Some(val) => val,
        None => return Err(RuntimeException::new("Cannot call <>", expr))
    };

    match &lval {
        Value::Function(env_ref, func) => func.call(env, env_ref, args, expr),
        Value::BuiltinFunction(env_ref, func) => func.call(env, env_ref, args, expr),
        Value::Class(env_ref, class) => class.call(env, env_ref, args, expr),
        _ => return Err(RuntimeException::new(format!("{} is not callable", lval), expr))
    }
}
