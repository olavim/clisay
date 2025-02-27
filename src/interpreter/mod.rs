use std::{cell::RefCell, fmt, rc::Rc};

use environment::Environment;
use expression::Expression;
use function::BuiltinFunction;
use statement::Statement;
use value::Value;

use super::parser::AST;

mod resolver;
mod environment;
mod callable;
mod class;
mod object;
mod function;
mod value;
mod statement;
mod expression;

pub struct RuntimeException(String, Vec<String>);

impl RuntimeException {
    fn new(message: impl Into<String>, expr: &Expression) -> RuntimeException {
        let pos = expr.pos.clone();
        return RuntimeException(message.into(), vec![format!("at {}:{}", pos.file, pos.line)]);
    }

    fn from(origin: impl Into<String>, expr: &Expression, exception: RuntimeException) -> RuntimeException {
        let pos = expr.pos.clone();
        let mut stack: Vec<String> = exception.1;
        stack.extend(vec![format!("at {} in {}:{}", origin.into(), pos.file, pos.line)]);
        return RuntimeException(exception.0, stack);
    }
}

impl fmt::Display for RuntimeException {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        for frame in &self.1 {
            write!(f, "\n\t{}", frame)?;
        }
        return Ok(());
    }
}

pub type EvalResult = Result<Option<Value>, RuntimeException>;

pub trait Evaluatable {
    fn evaluate(&self, env: &Rc<Environment>) -> EvalResult;
}

pub struct Interpreter {
    current_env: Rc<Environment>,
    out: Rc<RefCell<Vec<String>>>
}

impl Interpreter {
    fn new() -> Self {
        let globals = Environment::new();
        let globals_ref = Rc::new(globals);
        let out = Rc::new(RefCell::new(Vec::new()));
        
        globals_ref.insert(0, Value::BuiltinFunction(globals_ref.clone(), Rc::new(BuiltinFunction::Print(out.clone()))));
        globals_ref.insert(1, Value::BuiltinFunction(globals_ref.clone(), Rc::new(BuiltinFunction::Time)));

        Interpreter {
            current_env: globals_ref.clone(),
            out
        }
    }

    fn evaluate(&mut self, stmt: &Statement) -> Result<Vec<String>, RuntimeException> {
        stmt.evaluate(&self.current_env)?;
        let out = self.out.borrow().clone();
        return Ok(out);
    }
}

pub fn evaluate(ast: &AST) -> Result<Vec<String>, String> {
    let resolved_ast = match resolver::analyze(&ast) {
        Ok(ast) => ast,
        Err(err) => return Err(format!("{}", err))
    };

    let mut evaluator = Interpreter::new();
    return match evaluator.evaluate(&resolved_ast.stmt) {
        Ok(out) => Ok(out),
        Err(err) => Err(format!("{}", err))
    };
}
