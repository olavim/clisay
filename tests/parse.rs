use std::collections::HashSet;

use clisay::internals::{parse, parse_matcher, try_parse, Ast, AstId, Expr, FnDecl, Literal, MatchElem, MatchScalar, Matcher, Operator, ReturnShape, Stmt, Symbol};

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
fn has_operator() {
    let ast = parse("say x = a has \"b\";");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Binary(Operator::Has, _, _)));
}

#[test]
fn keyword_as_dict_key() {
    // A reserved word in dict-key position is a plain string key, not syntax.
    let dict = parse("say x = { if: 1 };");
    assert!(matches!(dict.get(&say_value(&dict)), Expr::Literal(Literal::Dict(_))));
}

#[test]
fn not_equal_still_parses() {
    // The non-null assertion must not steal `!=`.
    let ast = parse("say x = a != b;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Binary(Operator::LogicalNotEqual, _, _)));
}

fn matcher(src: &str) -> (Ast, AstId<Matcher>) {
    parse_matcher(src).expect("matcher parse error")
}

#[test]
fn matcher_atoms() {
    let (ast, m) = matcher("_");
    assert!(matches!(ast.get(&m), Matcher::Wildcard));
    let (ast, m) = matcher("42");
    assert!(matches!(ast.get(&m), Matcher::Literal(MatchScalar::Number(_))));
    let (ast, m) = matcher("null");
    assert!(matches!(ast.get(&m), Matcher::Literal(MatchScalar::Null)));
    let (ast, m) = matcher("x");
    assert!(matches!(ast.get(&m), Matcher::Binder(_)));
}

#[test]
fn matcher_type_tests() {
    let (ast, m) = matcher("is Point");
    let Matcher::Type { nominal, shape, .. } = ast.get(&m) else { panic!("not a type matcher") };
    assert!(*nominal && shape.is_none());

    let (ast, m) = matcher("is Point { x }");
    let Matcher::Type { nominal, shape, .. } = ast.get(&m) else { panic!("not a type matcher") };
    assert!(*nominal && shape.is_some());

    let (ast, m) = matcher("has Drawable");
    let Matcher::Type { nominal, .. } = ast.get(&m) else { panic!("not a type matcher") };
    assert!(!*nominal);

    // `has { … }` is a redundant spelling of a bare structural shape.
    let (ast, m) = matcher("has { x: 1 }");
    assert!(matches!(ast.get(&m), Matcher::Shape(_)));
}

#[test]
fn matcher_shape_shorthand_binds() {
    let (ast, m) = matcher("{ kind: \"line\", from, to }");
    let Matcher::Shape(fields) = ast.get(&m) else { panic!("not a shape") };
    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].key, MatchScalar::String("kind".into()));
    // `from` shorthand desugars to a binder value.
    assert!(matches!(ast.get(&fields[1].value), Matcher::Binder(_)));
}

#[test]
fn matcher_empty_shape_and_array() {
    let (ast, m) = matcher("{}");
    let Matcher::Shape(fields) = ast.get(&m) else { panic!("not a shape") };
    assert!(fields.is_empty());

    let (ast, m) = matcher("[]");
    let Matcher::Array(elems) = ast.get(&m) else { panic!("not an array") };
    assert!(elems.is_empty());
}

#[test]
fn matcher_allows_null_key() {
    let (ast, m) = matcher("{ null: _ }");
    let Matcher::Shape(fields) = ast.get(&m) else { panic!("not a shape") };
    assert_eq!(fields[0].key, MatchScalar::Null);
}

#[test]
fn matcher_array_rest() {
    let (ast, m) = matcher("[start, .., end]");
    let Matcher::Array(elems) = ast.get(&m) else { panic!("not an array") };
    assert_eq!(elems.len(), 3);
    assert!(matches!(elems[1], MatchElem::Rest(None)));

    let (ast, m) = matcher("[..rest]");
    let Matcher::Array(elems) = ast.get(&m) else { panic!("not an array") };
    assert!(matches!(elems[0], MatchElem::Rest(Some(_))));
}

#[test]
fn matcher_precedence() {
    // `@` looser than `|`: the binder spans the whole or-group.
    let (ast, m) = matcher("num @ 1 | 2 | 3");
    let Matcher::As(_, inner) = ast.get(&m) else { panic!("not an as-matcher") };
    assert!(matches!(ast.get(inner), Matcher::Or(_)));

    // `&` tighter than `|`: `a & b | c` is `(a & b) | c`.
    let (ast, m) = matcher("has A & has B | has C");
    let Matcher::Or(alts) = ast.get(&m) else { panic!("not an or-matcher") };
    assert_eq!(alts.len(), 2);
    assert!(matches!(ast.get(&alts[0]), Matcher::And(_)));
}

#[test]
fn matcher_grouping_overrides_precedence() {
    let (ast, m) = matcher("(num @ 1) | 2");
    let Matcher::Or(alts) = ast.get(&m) else { panic!("not an or-matcher") };
    assert_eq!(alts.len(), 2);
    assert!(matches!(ast.get(&alts[0]), Matcher::As(_, _)));
}

#[test]
fn matcher_rejected_forms() {
    assert!(parse_matcher("is { x }").is_err());
    assert!(parse_matcher("{ a: 1, a: 2 }").is_err());
    assert!(parse_matcher("[.., ..]").is_err());
}

#[test]
fn match_statement_arms() {
    let ast = parse("match x { is Point { a } => f(), _ => g() }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match") };
    assert_eq!(arms.len(), 2);
    assert!(arms[0].guard.is_none());
}

#[test]
fn match_guard_uses_low_precedence_operator() {
    // `=>` delimits the guard, so a bare `??` guard is not swallowed as a lambda.
    let ast = parse("match x { _ if a ?? b => g() }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match") };
    let guard = arms[0].guard.expect("missing guard");
    assert!(matches!(ast.get(&guard), Expr::Binary(Operator::Coalesce, _, _)));
}

#[test]
fn match_trailing_comma() {
    let ast = parse("match x { _ => g(), }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match") };
    assert_eq!(arms.len(), 1);
}

#[test]
fn match_empty_is_rejected() {
    assert!(try_parse("match x { }").is_err());
}

/// The condition of a single leading `if`/`while` statement.
fn condition(ast: &Ast) -> AstId<Expr> {
    let stmts = top_stmts(ast);
    match ast.get(&stmts[0]) {
        Stmt::If(cond, _, _) => *cond,
        Stmt::While(cond, _) => *cond,
        _ => panic!("not an if/while"),
    }
}

#[test]
fn match_bind_in_if_and_while() {
    let ast = parse("if { x } <- v { }");
    assert!(matches!(ast.get(&condition(&ast)), Expr::MatchBind(_, _)));

    let ast = parse("while [a, ..] <- v { }");
    assert!(matches!(ast.get(&condition(&ast)), Expr::MatchBind(_, _)));
}

#[test]
fn match_bind_in_and_operands() {
    // A match-bind is recognized on either side of `&&`.
    let ast = parse("if { x } <- v && ok { }");
    let Expr::Binary(Operator::LogicalAnd, left, _) = ast.get(&condition(&ast)) else { panic!("not &&") };
    assert!(matches!(ast.get(left), Expr::MatchBind(_, _)));

    let ast = parse("if ok && { x } <- v { }");
    let Expr::Binary(Operator::LogicalAnd, _, right) = ast.get(&condition(&ast)) else { panic!("not &&") };
    assert!(matches!(ast.get(right), Expr::MatchBind(_, _)));
}

#[test]
fn match_bind_precedence_cutoff() {
    // `??` binds looser than `<-`, so `{ a } <- x ?? y` is `({ a } <- x) ?? y`.
    let ast = parse("if { a } <- x ?? y { }");
    let Expr::Binary(Operator::Coalesce, left, _) = ast.get(&condition(&ast)) else { panic!("not ??") };
    assert!(matches!(ast.get(left), Expr::MatchBind(_, _)));
}

#[test]
fn match_bind_in_guard() {
    let ast = parse("match x { _ if { a } <- y => g() }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match") };
    let guard = arms[0].guard.expect("missing guard");
    assert!(matches!(ast.get(&guard), Expr::MatchBind(_, _)));
}

#[test]
fn match_bind_is_non_associative() {
    // A bare binder operand and a chained `<-` are both parse errors.
    assert!(try_parse("if a <- b <- c { }").is_err());
    assert!(try_parse("if { x } <- y <- z { }").is_err());
}

#[test]
fn match_bind_rejected_outside_condition() {
    assert!(try_parse("say ok = { x } <- v;").is_err());
}

#[test]
fn match_bind_rejects_pure_value_operand() {
    assert!(try_parse("if 5 <- x { }").is_err());
    assert!(try_parse("if 1 | 2 <- x { }").is_err());
}

#[test]
fn match_bind_bare_binder_hint() {
    let err = try_parse("if a <- b { }").err().expect("expected a parse error");
    assert!(err.contains("is a"), "should point to a type test: {err}");
    assert!(err.contains("space between `<` and `-`"), "should give the spacing hint: {err}");
}
