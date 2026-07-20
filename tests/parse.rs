use std::collections::HashSet;

use clisay::internals::{parse, parse_matcher, try_parse, Ast, AstId, Capability, Expr, FieldInit, FnDecl, Literal, MatchElem, MatchScalar, Matcher, ObligationRule, Operator, ReturnShape, Stmt, Symbol};

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
    let ast = parse("fn f(x, y?) {}");
    let stmts = top_stmts(&ast);
    let params = &nth_fn(&ast, &stmts, 0).params;
    let flags: Vec<bool> = params.iter().map(|p| p.nullable).collect();
    assert_eq!(flags, vec![false, true]);
}

#[test]
fn param_capability_marker() {
    // `mut` / `*mut` lead the clause, ahead of the obligation atoms.
    let ast = parse("fn f(a: mut, b: *mut, c: mut opt) {}");
    let stmts = top_stmts(&ast);
    let params = &nth_fn(&ast, &stmts, 0).params;
    assert_eq!(params[0].clause.capability, Capability::Mut);
    assert!(params[0].clause.names.is_empty());
    assert_eq!(params[1].clause.capability, Capability::MoveMut);
    assert_eq!(params[2].clause.capability, Capability::Mut);
    assert_eq!(ast.text(params[2].clause.names[0]), "opt");
}

#[test]
fn fn_return_capability_marker() {
    let ast = parse("fn f(): mut opt {}");
    let decl = nth_fn(&ast, &top_stmts(&ast), 0);
    assert_eq!(decl.clause.capability, Capability::Mut);
    assert_eq!(ast.text(decl.clause.names[0]), "opt");
}

#[test]
fn capability_marker_position_is_free() {
    // Like `void`, the capability atom composes with obligations in any order.
    for src in ["fn f(x: mut opt) {}", "fn f(x: opt mut) {}"] {
        let ast = parse(src);
        let param = &nth_fn(&ast, &top_stmts(&ast), 0).params[0];
        assert_eq!(param.clause.capability, Capability::Mut, "{src}");
        assert_eq!(ast.text(param.clause.names[0]), "opt", "{src}");
    }

    let ast = parse("fn f(x: opt *mut fails) {}");
    let param = &nth_fn(&ast, &top_stmts(&ast), 0).params[0];
    assert_eq!(param.clause.capability, Capability::MoveMut);
    let names: Vec<&str> = param.clause.names.iter().map(|n| ast.text(*n)).collect();
    assert_eq!(names, vec!["opt", "fails"]);
}

#[test]
fn value_mut_construction() {
    // `mut` before a literal or constructor wraps the construction in a value-mut marker.
    let dict = parse("say d = mut {x: 1};");
    let Expr::Mut(inner) = dict.get(&say_value(&dict)) else { panic!("dict not value-mut") };
    assert!(matches!(dict.get(inner), Expr::Literal(Literal::Dict(_))));

    let array = parse("say a = mut [1, 2];");
    let Expr::Mut(inner) = array.get(&say_value(&array)) else { panic!("array not value-mut") };
    assert!(matches!(array.get(inner), Expr::Literal(Literal::Array(_))));

    let ctor = parse("say u = mut User();");
    let Expr::Mut(inner) = ctor.get(&say_value(&ctor)) else { panic!("ctor not value-mut") };
    assert!(matches!(ctor.get(inner), Expr::Call(_, _)));
}

#[test]
fn value_mut_wraps_any_operand_optimistically() {
    // A non-construction operand still parses into the marker, so lowering can name the mistake.
    let ast = parse("say x = mut (1 + 2);");
    let Expr::Mut(inner) = ast.get(&say_value(&ast)) else { panic!("not value-mut") };
    assert!(matches!(ast.get(inner), Expr::Binary(Operator::Add, _, _)));
}

#[test]
fn capability_marker_rejections() {
    // `*mut` is one token, so a space between `*` and `mut` is not the move marker.
    assert!(try_parse("fn f(x: * mut) {}").is_err());
    assert!(try_parse("say x: mut;").is_err());
    assert!(try_parse("type T { a: mut; }").is_err());
    assert!(try_parse("fn f(x: mut mut) {}").is_err());
    assert!(try_parse("fn f(x: mut *mut) {}").is_err());
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
    let shapes: Vec<ReturnShape> = decl.req_fns.iter().map(|rf| rf.ret).collect();
    assert_eq!(shapes, vec![ReturnShape::Nullable, ReturnShape::NonNull, ReturnShape::Void]);
}

#[test]
fn say_slot_clause() {
    let ast = parse("say v: opt; say w: opt fails;");
    let stmts = top_stmts(&ast);
    let names = |init: &FieldInit| -> Vec<String> {
        init.clause.names.iter().map(|n| ast.text(*n).to_string()).collect()
    };
    let Stmt::Say(v) = ast.get(&stmts[0]) else { panic!("not a say") };
    assert_eq!(names(v), vec!["opt"]);
    assert!(!v.clause.container && !v.clause.void);
    let Stmt::Say(w) = ast.get(&stmts[1]) else { panic!("not a say") };
    assert_eq!(names(w), vec!["opt", "fails"]);
}

#[test]
fn field_slot_clause_container() {
    let ast = parse("type T { x: [taint]; }");
    let stmts = top_stmts(&ast);
    let Stmt::Type(decl) = ast.get(&stmts[0]) else { panic!("not a type") };
    let (_, clause) = &decl.field_clauses[0];
    assert!(clause.container);
    assert_eq!(ast.text(clause.names[0]), "taint");
}

#[test]
fn fn_return_slot_clause_void() {
    let ast = parse("fn f(): void {}");
    let stmts = top_stmts(&ast);
    let decl = nth_fn(&ast, &stmts, 0);
    assert!(decl.clause.void);
    assert!(decl.clause.names.is_empty());
}

#[test]
fn slot_clause_void_position_is_free() {
    // `void` is an atom, so it composes with obligations in any order.
    for src in ["fn f(): opt void {}", "fn f(): void opt {}", "fn f(): fails opt void {}"] {
        let ast = parse(src);
        let stmts = top_stmts(&ast);
        let decl = nth_fn(&ast, &stmts, 0);
        assert!(decl.clause.void, "{src}");
        assert!(!decl.clause.names.is_empty(), "{src}");
    }
}

#[test]
fn slot_clause_rejections() {
    assert!(try_parse("say a: opt opt;").is_err());
    assert!(try_parse("say b: [[taint]];").is_err());
    assert!(try_parse("say c: [void];").is_err());
    assert!(try_parse("fn f(): void void {}").is_err());
    assert!(try_parse("say d: void;").is_err());
    assert!(try_parse("fn g(x: void) {}").is_err());
    assert!(try_parse("type T { a: void; }").is_err());
    assert!(try_parse("say e: ;").is_err());
    assert!(try_parse("say f: = 1;").is_err());
}

#[test]
fn container_malformed_insides_get_targeted_errors() {
    let void = try_parse("say a: [taint void];").err().expect("expected a parse error");
    assert!(void.contains("'void' is not a valid container obligation"), "{void}");

    let nested = try_parse("say b: [opt [fails]];").err().expect("expected a parse error");
    assert!(nested.contains("cannot nest"), "{nested}");

    let comma = try_parse("say c: [opt, [fails]];").err().expect("expected a parse error");
    assert!(comma.contains("separated by spaces"), "{comma}");
}

#[test]
fn obligation_declaration() {
    let ast = parse("obligation tainted; obligation parsed: discharge to use Unparsed; obligation borrowed: discharge to escape; obligation held: discharge before drop;");
    let stmts = top_stmts(&ast);

    let Stmt::Obligation { name, witness, rule } = ast.get(&stmts[0]) else { panic!("not an obligation") };
    assert_eq!(ast.text(*name), "tainted");
    assert!(witness.is_none());
    assert_eq!(*rule, ObligationRule::ToUse);

    let Stmt::Obligation { witness, rule, .. } = ast.get(&stmts[1]) else { panic!("not an obligation") };
    let Some(w) = *witness else { panic!("witness form has no witness") };
    assert_eq!(ast.text(w), "Unparsed");
    assert_eq!(*rule, ObligationRule::ToUse);

    let Stmt::Obligation { witness, rule, .. } = ast.get(&stmts[2]) else { panic!("not an obligation") };
    assert!(witness.is_none());
    assert_eq!(*rule, ObligationRule::ToEscape);

    let Stmt::Obligation { rule, .. } = ast.get(&stmts[3]) else { panic!("not an obligation") };
    assert_eq!(*rule, ObligationRule::BeforeDrop);
}

#[test]
fn obligation_declaration_rejections() {
    assert!(try_parse("obligation bad: discharge to escape Row;").is_err());
    assert!(try_parse("obligation bad: discharge before drop Row;").is_err());
    assert!(try_parse("obligation bad: discharge to use 0;").is_err());
    assert!(try_parse("obligation bad: discharge to sink;").is_err());
    assert!(try_parse("obligation bad: no use;").is_err());
}

#[test]
fn coalesce_operator() {
    let ast = parse("say x = a ?? b;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Binary(Operator::Coalesce, _, _)));
}

#[test]
fn access_guard_operator() {
    let member = parse("say x = user?.name;");
    assert!(matches!(member.get(&say_value(&member)), Expr::SafeAccess(_, _, true)));

    let index = parse("say x = arr?[i];");
    assert!(matches!(index.get(&say_value(&index)), Expr::SafeAccess(_, _, false)));

    let call = parse("say x = cb?();");
    assert!(matches!(call.get(&say_value(&call)), Expr::SafeCall(_, _)));
}

#[test]
fn access_guard_ignores_space_after_question() {
    // `?` binds left, so whitespace before the accessor is irrelevant.
    let tight = parse("say x = x?.y;");
    let spaced = parse("say x = x? .y;");
    assert!(matches!(tight.get(&say_value(&tight)), Expr::SafeAccess(_, _, true)));
    assert!(matches!(spaced.get(&say_value(&spaced)), Expr::SafeAccess(_, _, true)));
}

#[test]
fn propagate_operator() {
    let ast = parse("say x = readFile(p)?!;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Propagate(_)));
}

#[test]
fn coalesce_handler_form() {
    let ast = parse("say x = parse(s) ?? e => log(e);");
    let Expr::Handle(_, binder, _) = ast.get(&say_value(&ast)) else { panic!("not a handler") };
    assert_eq!(ast.text(*binder), "e");
    let lambda = parse("say x = a ?? (y => y);");
    assert!(matches!(lambda.get(&say_value(&lambda)), Expr::Binary(Operator::Coalesce, _, _)));
}

#[test]
fn access_guard_rejections() {
    assert!(try_parse("say x = foo?;").is_err());
    assert!(try_parse("say x = foo ?.bar;").is_err());
    assert!(try_parse("say x = foo ?!;").is_err());
}

#[test]
fn assert_operator() {
    let ast = parse("say x = a!;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Assert(_)));
}

#[test]
fn has_operator() {
    let ast = parse("say x = a has { b: _ };");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Has(_, _)));
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
fn parse_error_renders_a_caret() {
    let err = try_parse("if x ~ y { }").err().expect("expected a parse error");
    // A numbered gutter row then a caret aligned under the matcher `y`.
    assert!(err.contains("1 | if x ~ y { }\n  |        ^"), "{err}");
}

#[test]
fn unexpected_token_error_renders_a_caret() {
    let err = try_parse("1 o;").err().expect("expected a parse error");
    // The caret sits under the unexpected token `o`, labelled with what was expected.
    assert!(err.contains("1 | 1 o;\n  |   ^ expected ';'"), "{err}");
}

#[test]
fn color_is_off_by_default_and_toggles_on() {
    // Off by default so captured output stays plain.
    let plain = try_parse("1 o;").err().expect("expected a parse error");
    assert!(!plain.contains('\x1b'), "{plain}");

    clisay::enable_color(true);
    let colored = try_parse("1 o;").err().expect("expected a parse error");
    clisay::enable_color(false);
    assert!(colored.contains("\x1b["), "{colored}");
}

#[test]
fn unclosed_block_points_at_opener() {
    // Running off the end of a block reports the missing `}` and its opening brace.
    let err = try_parse("fn f()! {\n    return 1;").err().expect("expected a parse error");
    assert!(err.contains("expected '}'"), "{err}");
    assert!(err.contains("unclosed '{'"), "{err}");
}

#[test]
fn unclosed_delimiter_points_at_opener() {
    let err = try_parse("say x = (1 + 2;").err().expect("expected a parse error");
    // The failure point carries the caret; the unclosed `(` gets a marker beneath it.
    assert!(err.contains("^ expected ')'"), "{err}");
    assert!(err.contains("unclosed '('"), "{err}");
}

#[test]
fn error_frame_shows_leading_context() {
    let err = try_parse("say a = 1;\n1 o;\nsay b = 2;").err().expect("expected a parse error");
    // The line before the offending line is shown; the line after is not.
    assert!(err.contains("1 | say a = 1;\n2 | 1 o;\n"), "{err}");
    assert!(!err.contains("3 | say b = 2;"), "{err}");
}

#[test]
fn error_header_has_severity_and_locator() {
    let err = try_parse("1 o;").err().expect("expected a parse error");
    assert!(err.starts_with("error: "), "{err}");
    // The locator carries file, line, and column. The empty test filename leaves the file blank.
    assert!(err.contains("\n --> :1:3\n"), "{err}");
}

#[test]
fn mismatch_names_the_found_token_by_source_text() {
    let err = try_parse("1 o;").err().expect("expected a parse error");
    assert!(err.contains("found 'o'"), "{err}");
    assert!(!err.contains("id("), "{err}");
}

#[test]
fn node_span_covers_whole_source() {
    let (ast, m) = matcher("n @ 1 | _");
    assert_eq!(ast.pos(&m).snippet(), "n @ 1 | _");

    let (ast, m) = matcher("{ x: 1 }");
    assert_eq!(ast.pos(&m).snippet(), "{ x: 1 }");

    let ast = parse("a + b;");
    let stmts = top_stmts(&ast);
    let Stmt::Expression(expr) = ast.get(&stmts[0]) else { panic!("not an expression statement") };
    assert_eq!(ast.pos(expr).snippet(), "a + b");
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
    // A bare name as an `&`/`|` operand binds the whole value and is rejected.
    assert!(parse_matcher("has A & b").is_err());
    assert!(parse_matcher("has A | b").is_err());
}

#[test]
fn matcher_negative_literal() {
    let (ast, m) = matcher("-42");
    let Matcher::Literal(MatchScalar::Number(n)) = ast.get(&m) else { panic!("not a number literal") };
    assert_eq!(*n, -42.0);
}

#[test]
fn negated_literal_folds_to_a_constant() {
    let ast = parse("say a = -5;");
    let Expr::Literal(Literal::Number(n)) = ast.get(&say_value(&ast)) else { panic!("negation of a literal did not fold") };
    assert_eq!(*n, -5.0);

    // A negated non-literal stays a runtime unary.
    let ast = parse("say b = -x;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Unary(Operator::Negate, _)));
}

#[test]
fn matcher_typed_shape_lookahead() {
    // A `{ key: ... }` or shorthand shape binds to the type; a statement-like `{` does not.
    let (ast, m) = matcher("is P { x, y }");
    let Matcher::Type { shape, .. } = ast.get(&m) else { panic!("not a type matcher") };
    assert!(shape.is_some());

    let (ast, m) = matcher("is P");
    let Matcher::Type { shape, .. } = ast.get(&m) else { panic!("not a type matcher") };
    assert!(shape.is_none());
}

#[test]
fn match_statement_arms() {
    let ast = parse("match x { is Point { a } => f(), _ => g() }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match dispatch") };
    assert_eq!(arms.len(), 2);
    assert!(arms[0].guard.is_none());
}

#[test]
fn match_guard_uses_low_precedence_operator() {
    // `=>` delimits the guard, so a bare `??` guard is not swallowed as a lambda.
    let ast = parse("match x { _ if a ?? b => g() }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match dispatch") };
    let guard = arms[0].guard.expect("missing guard");
    assert!(matches!(ast.get(&guard), Expr::Binary(Operator::Coalesce, _, _)));
}

#[test]
fn match_trailing_comma() {
    let ast = parse("match x { _ => g(), }");
    let stmts = top_stmts(&ast);
    let Stmt::Match(_, arms) = ast.get(&stmts[0]) else { panic!("not a match dispatch") };
    assert_eq!(arms.len(), 1);
}

#[test]
fn match_is_arms_only() {
    // `match` hosts only `matcher => body` arms; a shape, a bare matcher, or a mix is a parse error.
    assert!(try_parse("match d { kind: \"move\", dx }").is_err());
    assert!(try_parse("match d { is A | is B }").is_err());
    assert!(try_parse("match d { is A => f(), x: 1 }").is_err());
    assert!(try_parse("match d { is A, is B }").is_err());
}

#[test]
fn match_empty_is_rejected() {
    assert!(try_parse("match x { }").is_err());
}

#[test]
fn match_is_not_an_expression() {
    // The one-liner lives in `~`, so `match` never appears in value position.
    assert!(try_parse("say b = match d { _ => 1 };").is_err());
}

#[test]
fn tilde_one_liner_in_if_head() {
    let ast = parse("if d ~ { kind, dx, dy } { f(); }");
    let stmts = top_stmts(&ast);
    let Stmt::If(cond, _, _) = ast.get(&stmts[0]) else { panic!("not an if") };
    let Expr::Match(_, matcher) = ast.get(cond) else { panic!("condition is not a `~` one-liner") };
    assert!(matches!(ast.get(matcher), Matcher::Shape(_)));
}

#[test]
fn tilde_one_liner_in_while_and_and_heads() {
    let ast = parse("while q ~ [head, ..rest] { g(); }");
    let Stmt::While(cond, _) = ast.get(&top_stmts(&ast)[0]) else { panic!("not a while") };
    assert!(matches!(ast.get(cond), Expr::Match(_, _)));

    // `~` binds tighter than `&&`, so the right operand is the whole `~` test.
    let ast = parse("if a && d ~ { kind } { g(); }");
    let Stmt::If(cond, _, _) = ast.get(&top_stmts(&ast)[0]) else { panic!("not an if") };
    let Expr::Binary(Operator::LogicalAnd, _, right) = ast.get(cond) else { panic!("not an &&") };
    assert!(matches!(ast.get(right), Expr::Match(_, _)));
}

#[test]
fn tilde_binderless_one_liner_in_say_value() {
    let ast = parse("say b = d ~ is A | is B;");
    let Expr::Match(_, matcher) = ast.get(&say_value(&ast)) else { panic!("say value is not a `~` one-liner") };
    assert!(matches!(ast.get(matcher), Matcher::Or(_)));
}

#[test]
fn tilde_does_not_chain() {
    let err = try_parse("say b = a ~ { x } ~ { y };").err().expect("expected a parse error");
    assert!(err.contains("does not chain"), "{err}");
}

#[test]
fn match_arm_guard_requires_arrow() {
    assert!(try_parse("match x { _ if a }").is_err());
}

#[test]
fn tilde_prefix_and_infix_are_distinct() {
    // Prefix `~` is bitwise-not; infix `~` is test-and-bind.
    let ast = parse("say a = ~b;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Unary(Operator::BitNot, _)));
    let ast = parse("say c = d ~ is T;");
    assert!(matches!(ast.get(&say_value(&ast)), Expr::Match(_, _)));
}
