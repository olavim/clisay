use clisay::internals::{lower, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchBody, HirMatchElem, HirMatcher, HirStmt, ReturnShape};

/// The top-level statements of a lowered program (unwraps the root block).
fn top_stmts(hir: &Hir) -> Vec<HirId<HirStmt>> {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let HirExpr::Block(stmts) = hir.get(block) else { panic!("root expression is not a block") };
    stmts.clone()
}

fn first_arm_matcher(hir: &Hir) -> &HirMatcher {
    let stmts = top_stmts(hir);
    let HirStmt::Match(_, HirMatchBody::Arms(arms)) = hir.get(&stmts[0]) else { panic!("first statement is not a match dispatch") };
    &arms[0].matcher
}

fn nth_fn<'a>(hir: &'a Hir, stmts: &[HirId<HirStmt>], i: usize) -> &'a HirFnDecl {
    let HirStmt::Fn(decl) = hir.get(&stmts[i]) else { panic!("statement {i} is not a function") };
    decl
}

#[test]
fn say_flags_survive_lowering() {
    let hir = lower("say mut x? = 1;");
    let stmts = top_stmts(&hir);
    let HirStmt::Say(field) = hir.get(&stmts[0]) else { panic!("not a say") };
    assert!(field.nullable);
    assert!(field.mutable);
}

#[test]
fn fn_param_and_return_flags_survive_lowering() {
    let hir = lower("fn f(mut a?)! { return a; }");
    let stmts = top_stmts(&hir);
    let decl = nth_fn(&hir, &stmts, 0);
    assert_eq!(decl.ret, ReturnShape::NonNull);
    assert_eq!(decl.params.len(), 1);
    assert!(decl.params[0].nullable);
    assert!(decl.params[0].mutable);
}

#[test]
fn coalesce_lowers_to_dedicated_node() {
    let hir = lower("say a = 1; say b = 2; say c = a ?? b;");
    let stmts = top_stmts(&hir);
    let HirStmt::Say(field) = hir.get(&stmts[2]) else { panic!("not a say") };
    let value = field.value.expect("say has no value");
    assert!(matches!(hir.get(&value), HirExpr::Coalesce(_, _)));
}

#[test]
fn safe_access_lowers_to_dedicated_node() {
    let hir = lower("say a = 1; say b = a?.x;");
    let stmts = top_stmts(&hir);
    let HirStmt::Say(field) = hir.get(&stmts[1]) else { panic!("not a say") };
    let value = field.value.expect("say has no value");
    assert!(matches!(hir.get(&value), HirExpr::SafeAccess(_, _, true)));
}

#[test]
fn assert_lowers_to_dedicated_node() {
    let hir = lower("say a = 1; say b = a!;");
    let value = {
        let stmts = top_stmts(&hir);
        let HirStmt::Say(field) = hir.get(&stmts[1]) else { panic!("not a say") };
        field.value.expect("say has no value")
    };
    assert!(matches!(hir.get(&value), HirExpr::Assert(_)));
}

#[test]
fn type_field_flags_survive_lowering() {
    let hir = lower("type T { next?; mut count; }");
    let stmts = top_stmts(&hir);
    let HirStmt::Type(decl) = hir.get(&stmts[0]) else { panic!("not a type") };
    let next = hir.symbol_of("next").expect("next not interned");
    let count = hir.symbol_of("count").expect("count not interned");
    assert!(decl.nullable_fields.contains(&next));
    assert!(decl.mut_fields.contains(&count));
}

#[test]
fn shorthand_field_lowers_to_binder() {
    let hir = lower("match v { { x } => 0 }");
    let HirMatcher::Shape(fields) = first_arm_matcher(&hir) else { panic!("not a shape matcher") };
    assert_eq!(fields.len(), 1);
    assert!(matches!(fields[0].key, HirLiteral::String(ref s) if s == "x"));
    let x = hir.symbol_of("x").expect("x not interned");
    assert!(matches!(fields[0].value, HirMatcher::Binder(b) if b == x));
}

#[test]
fn array_rest_lowers() {
    let hir = lower("match v { [start, ..rest] => 0 }");
    let HirMatcher::Array(elements) = first_arm_matcher(&hir) else { panic!("not an array matcher") };
    assert_eq!(elements.len(), 2);
    assert!(matches!(elements[0], HirMatchElem::Elem(HirMatcher::Binder(_))));
    let rest = hir.symbol_of("rest").expect("rest not interned");
    assert!(matches!(elements[1], HirMatchElem::Rest(Some(r)) if r == rest));
}

#[test]
fn combinators_lower() {
    let hir = lower("match v { has A & is B => 0 }\ntype A { }\ntype B { }");
    let HirMatcher::And(parts) = first_arm_matcher(&hir) else { panic!("not an and matcher") };
    assert_eq!(parts.len(), 2);
    assert!(matches!(parts[0], HirMatcher::Type { nominal: false, .. }));
    assert!(matches!(parts[1], HirMatcher::Type { nominal: true, .. }));
}

#[test]
fn match_statement_lowers_to_arms() {
    let hir = lower("match v { is A => 1, _ => 0 }\ntype A { }");
    let stmts = top_stmts(&hir);
    let HirStmt::Match(_, HirMatchBody::Arms(arms)) = hir.get(&stmts[0]) else { panic!("first statement is not a match dispatch") };
    assert_eq!(arms.len(), 2);
    assert!(matches!(arms[0].matcher, HirMatcher::Type { nominal: true, .. }));
    assert!(matches!(arms[1].matcher, HirMatcher::Wildcard));
}
