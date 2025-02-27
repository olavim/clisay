use std::{fmt, rc::Rc};

use super::{class::Class, environment::Environment, function::{BuiltinFunction, Function}, object::Object};

#[derive(Clone)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    Boolean(bool),
    Class(Rc<Environment>, Rc<Class>),
    Function(Rc<Environment>, Rc<Function>),
    BuiltinFunction(Rc<Environment>, Rc<BuiltinFunction>),
    Object(Rc<Object>)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            Value::Void => String::from("<void>"),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("{}", s),
            Value::Boolean(b) => b.to_string(),
            Value::Class(_, _) => String::from("<class>"),
            Value::Function(_, _) => String::from("<function>"),
            Value::BuiltinFunction(_, _) => String::from("<builtin function>"),
            Value::Object(_) => String::from("<object>")
        });
    }
}