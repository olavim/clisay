use clisay::internals::{lower, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchElem, HirMatcher, HirStmt, ReturnShape};

/// The top-level statements of a lowered program (unwraps the root block).
/// The statements the program wrote. The compiler declares its own built-ins in the same block.
fn top_stmts(hir: &Hir) -> Vec<HirId<HirStmt>> {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let HirExpr::Block(stmts) = hir.get(block) else { panic!("root expression is not a block") };
    stmts.iter().filter(|s| !matches!(hir.get(*s), HirStmt::Type(decl) if decl.builtin.is_some())).copied().collect()
}

fn first_arm_matcher(hir: &Hir) -> &HirMatcher {
    let stmts = top_stmts(hir);
    let HirStmt::Match(_, arms) = hir.get(&stmts[0]) else { panic!("first statement is not a match dispatch") };
    hir.get(&arms[0].matcher)
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
    let hir = lower("fn f(a?)! { return a; }");
    let stmts = top_stmts(&hir);
    let decl = nth_fn(&hir, &stmts, 0);
    assert_eq!(decl.ret, ReturnShape::NonNull);
    assert_eq!(decl.params.len(), 1);
    assert!(decl.params[0].nullable);
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
    assert!(matches!(hir.get(&fields[0].value), HirMatcher::Binder(b) if *b == x));
}

#[test]
fn array_rest_lowers() {
    let hir = lower("match v { [start, ..rest] => 0 }");
    let HirMatcher::Array(elements) = first_arm_matcher(&hir) else { panic!("not an array matcher") };
    assert_eq!(elements.len(), 2);
    let HirMatchElem::Elem(first) = elements[0] else { panic!("not an element") };
    assert!(matches!(hir.get(&first), HirMatcher::Binder(_)));
    let rest = hir.symbol_of("rest").expect("rest not interned");
    assert!(matches!(elements[1], HirMatchElem::Rest(Some(r)) if r == rest));
}

#[test]
fn combinators_lower() {
    let hir = lower("match v { has A & B => 0 }\ntype A { }\ntype B { }");
    let HirMatcher::And(parts) = first_arm_matcher(&hir) else { panic!("not an and matcher") };
    assert_eq!(parts.len(), 2);
    assert!(matches!(hir.get(&parts[0]), HirMatcher::Type { nominal: false, .. }));
    assert!(matches!(hir.get(&parts[1]), HirMatcher::Type { nominal: true, .. }));
}

#[test]
fn match_statement_lowers_to_arms() {
    let hir = lower("match v { A => 1, _ => 0 }\ntype A { }");
    let stmts = top_stmts(&hir);
    let HirStmt::Match(_, arms) = hir.get(&stmts[0]) else { panic!("first statement is not a match dispatch") };
    assert_eq!(arms.len(), 2);
    assert!(matches!(hir.get(&arms[0].matcher), HirMatcher::Type { nominal: true, .. }));
    assert!(matches!(hir.get(&arms[1].matcher), HirMatcher::Wildcard));
}

/// Every matcher is its own arena node, so each carries the span of the text it was written as.
#[test]
fn each_matcher_carries_its_own_span() {
    let hir = lower("match v { A | B => 0 }\ntype A { }\ntype B { }");
    let stmts = top_stmts(&hir);
    let HirStmt::Match(_, arms) = hir.get(&stmts[0]) else { panic!("first statement is not a match dispatch") };
    let HirMatcher::Or(alternatives) = hir.get(&arms[0].matcher) else { panic!("not an or matcher") };
    assert_eq!(hir.pos(&arms[0].matcher).snippet(), "A | B");
    assert_eq!(hir.pos(&alternatives[0]).snippet(), "A");
    assert_eq!(hir.pos(&alternatives[1]).snippet(), "B");
}

/// A type names what it mixes by declaration id, minted at whichever site mentions it first.
#[test]
fn a_mixed_trait_keeps_one_id_in_either_declaration_order() {
    for src in ["type T with K { }\ntrait K { }", "trait K { }\ntype T with K { }"] {
        let hir = lower(src);
        let stmts = top_stmts(&hir);
        let ty = stmts.iter().find_map(|s| match hir.get(s) {
            HirStmt::Type(decl) => Some(decl), _ => None,
        }).expect("no type declaration");
        let tr = stmts.iter().find_map(|s| match hir.get(s) {
            HirStmt::Trait(decl) => Some(decl), _ => None,
        }).expect("no trait declaration");

        assert_ne!(ty.id, tr.id, "a type and the trait it mixes are two declarations: {src}");
        assert!(ty.provides.iter().any(|(_, id)| *id == ty.id), "type does not provide itself: {src}");
        assert!(ty.provides.iter().any(|(_, id)| *id == tr.id), "type does not provide the trait: {src}");
    }
}

/// Two declarations sharing a name are two ids, which is what lets a type test tell them apart.
/// Sibling scopes are where a name can reach two declarations, since neither shadows the other.
#[test]
fn same_named_declarations_get_distinct_ids() {
    let hir = lower("fn mk()! { type T { pub x; } return T { x: 1 }; }\nfn probe() { type T { pub y; } }");
    let stmts = top_stmts(&hir);
    let nested_type = |index: usize| {
        let HirStmt::Fn(decl) = hir.get(&stmts[index]) else { panic!("statement is not a function") };
        let HirExpr::Block(body) = hir.get(&decl.body) else { panic!("function body is not a block") };
        let HirStmt::Type(ty) = hir.get(&body[0]) else { panic!("nested statement is not a type") };
        (ty.name, ty.id)
    };
    let (first_name, first_id) = nested_type(0);
    let (second_name, second_id) = nested_type(1);

    assert!(first_name == second_name, "the two declarations should share a name");
    assert_ne!(first_id, second_id);
}

/// An obligation's witness names a declaration, so it carries that declaration's id.
#[test]
fn an_obligation_witness_takes_its_declarations_id() {
    for src in [
        "obligation warned { witness Wt; discharge to use; }\ntrait Wt { }",
        "trait Wt { }\nobligation warned { witness Wt; discharge to use; }",
    ] {
        let hir = lower(src);
        let stmts = top_stmts(&hir);
        let tr = stmts.iter().find_map(|s| match hir.get(s) {
            HirStmt::Trait(decl) => Some(decl), _ => None,
        }).expect("no trait declaration");
        let warned = hir.symbol_of("warned").expect("warned not interned");
        let (_, obligation) = hir.obligations().find(|(name, _)| *name == warned).expect("no obligation");
        let witness = obligation.witness.as_ref().expect("obligation has no witness");
        assert_eq!(witness.id, tr.id, "{src}");
    }
}
