use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::bail;

use crate::parser::ASTExpression;
use crate::parser::ASTExpressionKind;
use crate::parser::ASTStatement;
use crate::parser::StatementKind;
use crate::parser::AST;

use super::class::ClassDeclaration;
use super::expression::Expression;
use super::expression::ExpressionKind;
use super::function::Function;
use super::statement::Statement;

#[derive(Clone)]
pub struct SymbolId {
    pub symbol: usize,
    pub name: String
}

type Scope = HashMap<String, SymbolId>;
type AnalyzerResult<T> = Result<T, anyhow::Error>;

pub struct ResolvedAST {
    pub stmt: Statement
}

struct Resolver {
    symbols: usize,
    scope_stack: Vec<Scope>,
    class_scope: usize
}

impl Resolver {
    fn new() -> Resolver {
        let mut analyzer = Resolver { 
            symbols: 0,
            class_scope: 0,
            scope_stack: vec![Scope::new()]
        };
        analyzer.declare_symbol("print");
        analyzer.declare_symbol("time");
        return analyzer;
    }

    fn enter_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn get_symbol(&self, name: &str) -> Option<SymbolId> {
        return self.scope_stack.iter().rev().find_map(|s| s.get(name).cloned());
    }

    fn get_class_symbol(&self, name: &str) -> Option<SymbolId> {
        return self.scope_stack[..self.class_scope + 1].iter().rev().find_map(|s| s.get(name).cloned());
    }

    fn ensure_symbol(&self, name: &str) -> AnalyzerResult<SymbolId> {
        return match self.get_symbol(name) {
            Some(value) => Ok(value),
            None => bail!("{} is not declared", name)
        };
    }

    fn ensure_class_symbol(&self, name: &str) -> AnalyzerResult<SymbolId> {
        return match self.get_class_symbol(name) {
            Some(value) => Ok(value),
            None => bail!("{} is not declared", name)
        };
    }

    fn declare_symbol(&mut self, name: &str) -> SymbolId {
        let symbol_id = SymbolId { symbol: self.symbols, name: name.to_string() };
        self.scope_stack.last_mut().unwrap().insert(symbol_id.name.clone(), symbol_id.clone());
        self.symbols += 1;
        return symbol_id;
    }

    fn declare_this(&mut self) -> SymbolId {
        let symbol_id = SymbolId { symbol: 0, name: String::from("this") };
        self.scope_stack.last_mut().unwrap().insert(symbol_id.name.clone(), symbol_id.clone());
        self.symbols += 1;
        return symbol_id;
    }

    fn resolve(&mut self, program: &AST) -> AnalyzerResult<ResolvedAST> {
        return Ok(ResolvedAST { stmt: self.resolve_stmt(&program.stmt)? });
    }

    fn resolve_stmt(&mut self, stmt: &ASTStatement) -> AnalyzerResult<Statement> {
        return match &stmt.kind {
            StatementKind::Compound(statements) => self.resolve_compound_stmt(statements),
            StatementKind::Expression(expression) => Ok(Statement::Expression(self.resolve_expr(expression)?)),
            StatementKind::Return(expression) => match expression {
                Some(expr) => Ok(Statement::Return(Some(self.resolve_expr(expr)?))),
                None => Ok(Statement::Return(None))
            },
            StatementKind::Say(declaration) => {
                let value = match &declaration.value {
                    Some(value) => Some(self.resolve_expr(value)?),
                    None => None
                };

                let sid = self.declare_symbol(&declaration.name);
                Ok(Statement::Say(sid, value))
            },
            StatementKind::Fn(declaration) => {
                let sid = self.declare_symbol(&declaration.name);
                self.enter_scope();

                let mut param_set: HashSet<String> = HashSet::new();
                let mut resolved_params: Vec<SymbolId> = Vec::new();

                for param in &declaration.params {
                    if param_set.contains(param) {
                        bail!("Duplicate parameter name {}", param);
                    }
                    param_set.insert(param.clone());
                    resolved_params.push(self.declare_symbol(&param));
                }

                let body = Box::from(self.resolve_stmt(&declaration.body)?);
                self.exit_scope();
                Ok(Statement::Fn(sid, resolved_params, body))
            },
            StatementKind::If(condition, then, otherwise) => {
                let cond = self.resolve_expr(&condition)?;
                let then = Box::from(self.resolve_stmt(&then)?);
                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::from(self.resolve_stmt(&otherwise)?))
                } else {
                    None
                };
                Ok(Statement::If(cond, then, otherwise))
            },
            StatementKind::While(condition, body) => {
                let cond = self.resolve_expr(&condition)?;
                let body = self.resolve_stmt(&body)?;
                Ok(Statement::While(cond, Box::from(body)))
            },
            StatementKind::Class(decl) => {
                let class_sid = self.declare_symbol(&decl.name);
                let super_sid = self.declare_symbol("super");

                let superclass_sid = match &decl.superclass {
                    Some(name) => Some(self.ensure_symbol(name)?),
                    None => None
                };

                self.enter_scope();
                self.class_scope = self.scope_stack.len() - 1;

                let mut methods: Vec<Function> = Vec::new();

                let fields: Vec<SymbolId> = decl.fields.iter()
                    .map(|field| self.declare_symbol(field))
                    .collect();

                for method in &decl.methods {
                    let method_sid = self.declare_symbol(&method.name);
                    self.enter_scope();
                    self.declare_this();
                    let resolved_params = method.params.iter().map(|param| self.declare_symbol(param)).collect();
                    let stmt = Box::new(self.resolve_stmt(&method.body)?);
                    self.exit_scope();

                    methods.push(Function::new(method_sid, resolved_params, stmt));
                }

                self.enter_scope();
                let init_params = decl.init.params.iter().map(|p| self.declare_symbol(p)).collect();
                self.declare_this();
                let init_body = Box::new(self.resolve_stmt(&decl.init.body)?);
                let init_fn = Function::new(self.declare_symbol("init"), init_params, init_body);
                self.exit_scope();

                self.exit_scope();

                let class = ClassDeclaration {
                    super_sid,
                    superclass_sid,
                    init: init_fn,
                    methods,
                    fields
                };

                Ok(Statement::Class(class_sid, class))
            }
        };
    }

    fn resolve_compound_stmt(&mut self, statements: &Vec<ASTStatement>) -> AnalyzerResult<Statement> {
        self.enter_scope();
        let mut sem_statements = Vec::new();
        for statement in statements {
            sem_statements.push(self.resolve_stmt(statement)?);
        }
        self.exit_scope();
        return Ok(Statement::Compound(sem_statements));
    }

    fn resolve_expr(&self, expr: &ASTExpression) -> AnalyzerResult<Expression> {
        let pos = expr.pos.clone();

        let kind = match &expr.kind {
            ASTExpressionKind::Ternary(cond, left, right) => {
                let cval = Box::new(self.resolve_expr(&cond)?);
                let lval = Box::new(self.resolve_expr(&left)?);
                let rval = Box::new(self.resolve_expr(&right)?);
                ExpressionKind::Ternary(cval, lval, rval)
            },
            ASTExpressionKind::Binary(op, left, right) => {
                let lval = Box::new(self.resolve_expr(&left)?);
                let rval = Box::new(self.resolve_expr(&right)?);
                ExpressionKind::Binary(op.clone(), lval, rval)
            },
            ASTExpressionKind::Unary(op, expr) => {
                let val = Box::new(self.resolve_expr(&expr)?);
                ExpressionKind::Unary(op.clone(), val)
            },
            ASTExpressionKind::Call(expr, args) => {
                let lval = Box::new(self.resolve_expr(&expr)?);
                let args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Result<Vec<Expression>, anyhow::Error>>()?;
                ExpressionKind::Call(lval, args)
            },
            ASTExpressionKind::This => match self.ensure_symbol("this") {
                Ok(_) => ExpressionKind::This,
                _ => bail!("this can only be used in a class context")
            },
            ASTExpressionKind::Super(_) => match self.ensure_symbol("super") {
                Ok(_) => ExpressionKind::Super,
                _ => bail!("super can only be used in a class context")
            },
            ASTExpressionKind::SuperCall(args) => match self.ensure_symbol("super") {
                Ok(_) => {
                    let args = args.iter()
                        .map(|arg| self.resolve_expr(arg))
                        .collect::<Result<Vec<Expression>, anyhow::Error>>()?;
                    ExpressionKind::SuperCall(args)
                },
                _ => bail!("super can only be used in a class context")
            },
            ASTExpressionKind::Identifier(name) => ExpressionKind::Identifier(self.ensure_symbol(&name)?),
            ASTExpressionKind::Number(val) => ExpressionKind::Number(*val),
            ASTExpressionKind::String(val) => ExpressionKind::String(val.clone()),
            ASTExpressionKind::Boolean(val) => ExpressionKind::Boolean(*val),
            ASTExpressionKind::MemberAccess(expr, member) => {
                let value = Box::new(self.resolve_expr(&expr)?);
                let member_sid = match expr.kind {
                    ASTExpressionKind::This => Some(self.ensure_class_symbol(&member)?),
                    _ => None
                };
                ExpressionKind::MemberAccess(value, member.clone(), member_sid)
            }
        };

        return Ok(Expression::new(kind, pos));
    }
}

pub fn resolve(program: &AST) -> Result<ResolvedAST, anyhow::Error> {
    let mut analyzer = Resolver::new();
    return analyzer.resolve(program);
}