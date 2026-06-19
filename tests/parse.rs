use std::collections::HashSet;

use clisay::internals::{parse, Ast, AstId, Expr, FnDecl, Literal, Operator, ReturnShape, Stmt, Symbol};

/// The top-level statements of a parsed program (unwraps the root block).
fn top_stmts(ast: &Ast) -> Vec<AstId<Stmt>> {
    let root = ast.get_root();
    let Stmt::Expression(block) = ast.get(&root) else { panic!("root is not an expression statement") };
    let Expr::Block(stmts) = ast.get(block) else { panic!("root expression is not a block") };
    stmts.clone()
}

fn nth_fn<'a>(ast: &'a Ast, stmts: &[AstId<Stmt>], i: usize) -> &'a FnDecl {
    let Stmt::Fn(decl) = ast.get(&stmts[i]) else { panic!("statement {i} is not a function") };
    decl
}

/// The value expression of a `say x = <expr>;` program.
fn say_value(ast: &Ast) -> AstId<Expr> {
    let stmts = top_stmts(ast);
    let Stmt::Say(field) = ast.get(&stmts[0]) else { panic!("not a say") };
    field.value.expect("say has no value")
}

#[test]
fn say_nullability_and_mutability() {
    let ast = parse("say a = 1; say b? = 2; say mut c = 3; say mut d? = 4;");
    let flags: Vec<(bool, bool)> = top_stmts(&ast).iter().map(|s| {
        let Stmt::Say(f) = ast.get(s) else { panic!("not a say") };
        (f.nullable, f.mutable)
    }).collect();
    assert_eq!(flags, vec![(false, false), (true, false), (false, true), (true, true)]);
}

#[test]
fn fn_return_shapes() {
    let ast = parse("fn a() {} fn b()! { return 1; } fn c()? {}");
    let stmts = top_stmts(&ast);
    assert_eq!(nth_fn(&ast, &stmts, 0).ret, ReturnShape::Void);
    assert_eq!(nth_fn(&ast, &stmts, 1).ret, ReturnShape::NonNull);
    assert_eq!(nth_fn(&ast, &stmts, 2).ret, ReturnShape::Nullable);
}

#[test]
fn param_markers() {
    let ast = parse("fn f(x, mut y, z?, mut w?) {}");
    let stmts = top_stmts(&ast);
    let params = &nth_fn(&ast, &stmts, 0).params;
    let flags: Vec<(bool, bool)> = params.iter().map(|p| (p.mutable, p.nullable)).collect();
    assert_eq!(flags, vec![(false, false), (true, false), (false, true), (true, true)]);
}

#[test]
fn lambda_return_is_inferred() {
    let ast = parse("say f = (x) => x;");
    let Expr::Literal(Literal::Lambda(decl)) = ast.get(&say_value(&ast)) else { panic!("not a lambda") };
    assert_eq!(decl.ret, ReturnShape::Inferred);
}

#[test]
fn type_field_markers() {
    let ast = parse("type T { a; b?; mut c; mut d?; init(a, c) { this.a = a; this.c = c; } }");
    let stmts = top_stmts(&ast);
    let Stmt::Type(decl) = ast.get(&stmts[0]) else { panic!("not a type") };
    let names = |set: &HashSet<Symbol>| -> HashSet<String> {
        set.iter().map(|s| ast.text(*s).to_string()).collect()
    };
    assert_eq!(names(&decl.nullable_fields), HashSet::from(["b".to_string(), "d".to_string()]));
    assert_eq!(names(&decl.mut_fields), HashSet::from(["c".to_string(), "d".to_string()]));
}

#[test]
fn req_fn_return_shape() {
    let ast = parse("trait T { req fn find()?; req fn count()!; req fn onClick(); }");
    let stmts = top_stmts(&ast);
    let Stmt::Type(decl) = ast.get(&stmts[0]) else { panic!("not a trait") };
    let shapes: Vec<ReturnShape> = decl.req_fns.iter().map(|(_, _, ret)| *ret).collect();
    assert_eq!(shapes, vec![ReturnShape::Nullable, ReturnShape::NonNull, ReturnShape::Void]);
}

#[test]
fn coalesce_operator() {
    let ast = parse("say x = a ?? b;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Binary(Operator::Coalesce, _, _)));
}

#[test]
fn safe_navigation_operators() {
    let dot = parse("say x = a?.b;");
    assert!(matches!(dot.get(&say_value(&dot)), Expr::SafeAccess(_, _, true)));

    let index = parse("say x = a?[b];");
    assert!(matches!(index.get(&say_value(&index)), Expr::SafeAccess(_, _, false)));
}

#[test]
fn assert_operator() {
    let ast = parse("say x = a!;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Assert(_)));
}

#[test]
fn not_equal_still_parses() {
    // The non-null assertion must not steal `!=`.
    let ast = parse("say x = a != b;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Binary(Operator::LogicalNotEqual, _, _)));
}
