use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use anyhow::bail;

use crate::parser::ASTExpression;
use crate::parser::ASTExpressionKind;
use crate::parser::ASTStatement;
use crate::parser::StatementKind;
use crate::parser::AST;

use super::class::Class;
use super::class::ClassInit;
use super::class::ClassMethod;
use super::expression::Expression;
use super::expression::ExpressionKind;
use super::function::Function;
use super::statement::Statement;

#[derive(Clone)]
pub struct SymbolId {
    pub symbol: usize,
    pub name: String
}

impl SymbolId {
    pub fn new(symbol: usize, name: String) -> SymbolId {
        return SymbolId { symbol, name };
    }
}

type Scope = HashMap<String, SymbolId>;
type AnalyzerResult<T> = Result<T, anyhow::Error>;

pub struct ResolvedAST {
    pub stmt: Statement
}

struct Resolver {
    symbols: Vec<SymbolId>,
    classes: HashMap<usize, Rc<Class>>,
    scope_stack: Vec<Scope>,
    current_class_symbols: Option<HashMap<String, usize>>
}

impl Resolver {
    fn new() -> Resolver {
        let mut analyzer = Resolver { 
            symbols: Vec::new(), 
            classes: HashMap::new(),
            scope_stack: vec![Scope::new()],
            current_class_symbols: None
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

    fn ensure_symbol(&self, name: &str) -> AnalyzerResult<SymbolId> {
        return match self.get_symbol(name) {
            Some(value) => Ok(value),
            None => bail!("{} is not declared", name)
        };
    }

    fn declare_symbol(&mut self, name: &str) -> SymbolId {
        let symbol = self.symbols.len();
        let symbol_id = SymbolId { symbol, name: name.to_string() };
        self.scope_stack.last_mut().unwrap().insert(String::from(name), symbol_id.clone());
        self.symbols.push(symbol_id.clone());
        return symbol_id;
    }

    fn analyze(&mut self, program: &AST) -> AnalyzerResult<ResolvedAST> {
        return Ok(ResolvedAST { stmt: self.analyze_stmt(&program.stmt)? });
    }

    fn analyze_stmt(&mut self, stmt: &ASTStatement) -> AnalyzerResult<Statement> {
        return match &stmt.kind {
            StatementKind::Compound(statements) => self.analyze_compound_stmt(statements),
            StatementKind::Expression(expression) => Ok(Statement::Expression(self.analyze_expr(expression)?)),
            StatementKind::Return(expression) => match expression {
                Some(expr) => Ok(Statement::Return(Some(self.analyze_expr(expr)?))),
                None => Ok(Statement::Return(None))
            },
            StatementKind::Say(declaration) => {
                let value = match &declaration.value {
                    Some(value) => Some(self.analyze_expr(value)?),
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

                for param in &declaration.parameters {
                    if param_set.contains(param) {
                        bail!("Duplicate parameter name {}", param);
                    }
                    param_set.insert(param.clone());
                    resolved_params.push(self.declare_symbol(&param));
                }

                let body = Box::from(self.analyze_stmt(&declaration.body)?);
                self.exit_scope();
                Ok(Statement::Fn(sid, resolved_params, body))
            },
            StatementKind::If(condition, then, otherwise) => {
                let cond = self.analyze_expr(&condition)?;
                let then = Box::from(self.analyze_stmt(&then)?);
                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::from(self.analyze_stmt(&otherwise)?))
                } else {
                    None
                };
                Ok(Statement::If(cond, then, otherwise))
            },
            StatementKind::While(condition, body) => {
                let cond = self.analyze_expr(&condition)?;
                let body = self.analyze_stmt(&body)?;
                Ok(Statement::While(cond, Box::from(body)))
            },
            StatementKind::Class(decl) => {
                let class_sid = self.declare_symbol(&decl.name);

                let superclass = match &decl.superclass {
                    Some(name) => match self.classes.get(&self.ensure_symbol(name)?.symbol) {
                        Some(class) => Some(class.clone()),
                        None => bail!("Cannot inherit undeclared class {}", name)
                    },
                    None => None
                };

                self.enter_scope();

                self.enter_scope();
                let symbols: HashMap<String, usize> = decl.fields.iter()
                    .chain(decl.methods.iter().map(|method| &method.name))
                    .map(|name| (name.clone(), self.declare_symbol(name).symbol))
                    .collect();
                self.exit_scope();

                let previous_class_symbols = self.current_class_symbols.clone();
                self.current_class_symbols = Some(symbols.clone());

                self.enter_scope();
                let resolved_super_args = decl.init.super_args.iter()
                    .map(|arg| self.analyze_expr(arg))
                    .collect::<AnalyzerResult<Vec<Expression>>>()?;

                let resolved_init_params = decl.init.parameters.iter()
                    .map(|param| self.declare_symbol(param))
                    .collect();

                let init = ClassInit {
                    this_sid: self.declare_symbol("this"),
                    superclass: superclass.clone(),
                    super_args: resolved_super_args,
                    func: Function::new(
                        format!("{}.init()", decl.name),
                        resolved_init_params,
                        Box::new(self.analyze_stmt(&decl.init.body)?)
                    )
                };
                self.exit_scope();

                let mut methods: HashMap<usize, ClassMethod> = HashMap::new();
                let fields: Vec<usize> = decl.fields.iter().map(|field| symbols.get(field).unwrap().clone()).collect();

                for method in &decl.methods {
                    self.enter_scope();
                    let this_sid = self.declare_symbol("this");
                    let resolved_params = method.parameters.iter().map(|param| self.declare_symbol(param)).collect();
                    let stmt = Box::new(self.analyze_stmt(&method.body)?);
                    self.exit_scope();

                    let symbol = symbols.get(&method.name).unwrap();
                    methods.insert(*symbol, ClassMethod(this_sid, resolved_params, stmt));
                }

                self.current_class_symbols = previous_class_symbols;
                self.exit_scope();

                let class = Rc::new(Class {
                    name: decl.name.clone(),
                    superclass,
                    init,
                    symbols,
                    methods,
                    fields
                });

                self.classes.insert(class_sid.symbol, class.clone());
                Ok(Statement::Class(class_sid, class))
            }
        };
    }

    fn analyze_compound_stmt(&mut self, statements: &Vec<ASTStatement>) -> AnalyzerResult<Statement> {
        self.enter_scope();
        let mut sem_statements = Vec::new();
        for statement in statements {
            sem_statements.push(self.analyze_stmt(statement)?);
        }
        self.exit_scope();
        return Ok(Statement::Compound(sem_statements));
    }

    fn analyze_expr(&self, expr: &ASTExpression) -> AnalyzerResult<Expression> {
        let pos = expr.pos.clone();

        let kind = match &expr.kind {
            ASTExpressionKind::Ternary(cond, left, right) => {
                let cval = Box::new(self.analyze_expr(&cond)?);
                let lval = Box::new(self.analyze_expr(&left)?);
                let rval = Box::new(self.analyze_expr(&right)?);
                ExpressionKind::Ternary(cval, lval, rval)
            },
            ASTExpressionKind::Binary(op, left, right) => {
                let lval = Box::new(self.analyze_expr(&left)?);
                let rval = Box::new(self.analyze_expr(&right)?);
                ExpressionKind::Binary(op.clone(), lval, rval)
            },
            ASTExpressionKind::Unary(op, expr) => {
                let val = Box::new(self.analyze_expr(&expr)?);
                ExpressionKind::Unary(op.clone(), val)
            },
            ASTExpressionKind::Call(expr, args) => {
                let lval = Box::new(self.analyze_expr(&expr)?);
                let mut rval = Vec::new();
                for arg in args {
                    rval.push(self.analyze_expr(&arg)?);
                }
                ExpressionKind::Call(lval, rval)
            },
            ASTExpressionKind::This => match self.current_class_symbols {
                Some(_) => ExpressionKind::This(self.ensure_symbol("this")?),
                None => bail!("this can only be used in a class context")
            },
            ASTExpressionKind::Identifier(name) => match self.ensure_symbol(&name) {
                Ok(sid) => ExpressionKind::Identifier(sid),
                Err(_) => match self.ensure_symbol("this") {
                    Ok(this_sid) => {
                        let symbol = match self.current_class_symbols.as_ref().unwrap().get(name) {
                            Some(symbol) => *symbol,
                            None => bail!("{} is not declared", name)
                        };
                        let id_kind = ExpressionKind::This(this_sid);
                        ExpressionKind::MemberAccess(
                            Box::new(Expression::new(id_kind, pos.clone())),
                            name.clone(),
                            Some(SymbolId::new(symbol, name.clone()))
                        )
                    },
                    Err(_) => bail!("{} is not declared", name)
                }
            },
            ASTExpressionKind::Number(val) => ExpressionKind::Number(*val),
            ASTExpressionKind::String(val) => ExpressionKind::String(val.clone()),
            ASTExpressionKind::Boolean(val) => ExpressionKind::Boolean(*val),
            ASTExpressionKind::MemberAccess(expr, member) => match expr.kind {
                ASTExpressionKind::This => {
                    let symbol = match self.current_class_symbols.as_ref().unwrap().get(member) {
                        Some(symbol) => *symbol,
                        None => bail!("{} is not declared", member)
                    };
                    let id_kind = ExpressionKind::This(self.ensure_symbol("this")?);
                    ExpressionKind::MemberAccess(
                        Box::new(Expression::new(id_kind, pos.clone())),
                        member.clone(),
                        Some(SymbolId::new(symbol, member.clone()))
                    )
                },
                _ => {
                    let value = Box::new(self.analyze_expr(&expr)?);
                    ExpressionKind::MemberAccess(value, member.clone(), None)
                }
            }
        };

        return Ok(Expression::new(kind, pos));
    }
}

pub fn analyze(program: &AST) -> Result<ResolvedAST, anyhow::Error> {
    let mut analyzer = Resolver::new();
    return analyzer.analyze(program);
}