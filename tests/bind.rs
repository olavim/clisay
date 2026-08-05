use clisay::internals::{bind, Hir, HirStmt, TypeLayout};

/// The `TypeLayout` of the first top-level `type` declaration.
fn first_type_layout<'a>(hir: &Hir, bindings: &'a clisay::internals::Bindings) -> &'a TypeLayout {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let clisay::internals::HirExpr::Block(stmts) = hir.get(block) else { panic!("root is not a block") };
    let stmt = stmts.iter()
        .find(|s| matches!(hir.get(*s), HirStmt::Type(decl) if decl.builtin.is_none()))
        .expect("no type declaration");
    bindings.type_layout(stmt)
}

#[test]
fn field_nullability_and_mutability_on_layout() {
    let (hir, bindings) = bind("type T { pub next?; pub mut count; pub fixed; }");
    let layout = first_type_layout(&hir, &bindings);

    let next = hir.symbol_of("next").expect("next not interned");
    let count = hir.symbol_of("count").expect("count not interned");
    let fixed = hir.symbol_of("fixed").expect("fixed not interned");

    assert!(layout.is_nullable(next));
    assert!(!layout.is_mutable(next));

    assert!(layout.is_mutable(count));
    assert!(!layout.is_nullable(count));

    assert!(!layout.is_nullable(fixed));
    assert!(!layout.is_mutable(fixed));
}

#[test]
fn method_return_nullability_on_layout() {
    let (hir, bindings) = bind("type T { pub fn maybe(this)? { return null; } pub fn always(this)! { return 1; } }");
    let layout = first_type_layout(&hir, &bindings);

    let maybe = hir.symbol_of("maybe").expect("maybe not interned");
    let always = hir.symbol_of("always").expect("always not interned");

    assert!(layout.is_nullable(maybe));
    assert!(!layout.is_nullable(always));
}

/// The declaration the compiler supplies for a built-in, which `top_stmts` leaves out.
fn builtin_type_stmt(hir: &Hir) -> clisay::internals::HirId<HirStmt> {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let clisay::internals::HirExpr::Block(stmts) = hir.get(block) else { panic!("root is not a block") };
    *stmts.iter().find(|s| matches!(hir.get(*s), HirStmt::Type(decl) if decl.builtin.is_some()))
        .expect("no built-in type declaration")
}

/// The statements the program wrote. The compiler declares its own built-ins in the same block.
fn top_stmts(hir: &Hir) -> Vec<clisay::internals::HirId<HirStmt>> {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let clisay::internals::HirExpr::Block(stmts) = hir.get(block) else { panic!("root is not a block") };
    stmts.iter().filter(|s| !matches!(hir.get(*s), HirStmt::Type(decl) if decl.builtin.is_some())).copied().collect()
}

/// The value expression of the `say` statement at `index`.
fn say_value(hir: &Hir, stmts: &[clisay::internals::HirId<HirStmt>], index: usize) -> clisay::internals::HirId<clisay::internals::HirExpr> {
    let HirStmt::Say(field) = hir.get(&stmts[index]) else { panic!("statement is not a say") };
    field.value.expect("say has no value")
}

const SAME_NAME: &str = "\
fn mk()! { type T { pub x; } return T { x: 1 }; }
fn probe() { type T { pub y; } say v = mk(); say b = v is T; say c = v ~ T; }
";

/// The statements of the function declared at top-level `index`.
fn fn_body(hir: &Hir, stmts: &[clisay::internals::HirId<HirStmt>], index: usize) -> Vec<clisay::internals::HirId<HirStmt>> {
    let HirStmt::Fn(decl) = hir.get(&stmts[index]) else { panic!("statement is not a function") };
    let clisay::internals::HirExpr::Block(body) = hir.get(&decl.body) else { panic!("function body is not a block") };
    body.clone()
}

/// A type test names the declaration in scope where it is written. A sibling function's `T` is
/// unspellable there, so the local one is the only thing `T` can mean.
#[test]
fn a_type_test_resolves_to_the_declaration_in_scope() {
    use clisay::internals::HirExpr;
    let (hir, bindings) = bind(SAME_NAME);
    let stmts = top_stmts(&hir);
    let sibling = fn_body(&hir, &stmts, 0)[0];
    let body = fn_body(&hir, &stmts, 1);
    let local = body[0];
    assert!(matches!(hir.get(&sibling), HirStmt::Type(_)), "sibling statement is not a type");
    assert!(matches!(hir.get(&local), HirStmt::Type(_)), "nested statement is not a type");

    let is_expr = say_value(&hir, &body, 2);
    let HirExpr::Match(_, matcher) = hir.get(&is_expr) else { panic!("`is` is not a match") };
    assert_eq!(bindings.type_ref(matcher).map(|d| d.index()), Some(local.index()));
    assert_ne!(bindings.type_ref(matcher).map(|d| d.index()), Some(sibling.index()));
}

/// A matcher's type node resolves the same way, since each node carries its own answer.
#[test]
fn a_matcher_type_resolves_to_the_declaration_in_scope() {
    use clisay::internals::HirExpr;
    let (hir, bindings) = bind(SAME_NAME);
    let stmts = top_stmts(&hir);
    let body = fn_body(&hir, &stmts, 1);
    let local = body[0];

    let match_expr = say_value(&hir, &body, 3);
    let HirExpr::Match(_, matcher) = hir.get(&match_expr) else { panic!("not a `~` expression") };
    assert_eq!(bindings.type_ref(matcher).map(|d| d.index()), Some(local.index()));
}

/// A trait test resolves to its trait declaration. A trait takes no runtime slot, so the scope is
/// the only thing that knows which declaration its name reaches.
#[test]
fn a_trait_test_resolves_to_its_trait_declaration() {
    use clisay::internals::HirExpr;
    let (hir, bindings) = bind("trait Tr { }\ntype T with Tr { }\nsay v = T { };\nsay b = v is Tr;");
    let stmts = top_stmts(&hir);
    assert!(matches!(hir.get(&stmts[0]), HirStmt::Trait(_)), "first statement is not a trait");
    let is_expr = say_value(&hir, &stmts, 3);
    let HirExpr::Match(_, matcher) = hir.get(&is_expr) else { panic!("`is` is not a match") };
    assert_eq!(bindings.type_ref(matcher).map(|d| d.index()), Some(stmts[0].index()));
}

#[test]
fn a_test_on_a_builtin_resolves_to_its_declaration() {
    use clisay::internals::HirExpr;
    let (hir, bindings) = bind("fn src(): fails { return Err(\"e\"); }\nsay v: fails = src();\nsay b = v is Err;");
    let stmts = top_stmts(&hir);
    let err = builtin_type_stmt(&hir);

    let is_expr = say_value(&hir, &stmts, 2);
    let HirExpr::Match(_, matcher) = hir.get(&is_expr) else { panic!("`is` is not a match") };
    assert_eq!(bindings.type_ref(matcher).map(|d| d.index()), Some(err.index()));
}

/// Members are numbered in declaration order, not in the order their names happen to be interned.
/// A name mentioned earlier in the program interns earlier, which must not reorder the fields.
#[test]
fn field_ids_follow_declaration_order() {
    use clisay::internals::TypeMember;
    let (hir, bindings) = bind("say b = 0;\nsay a = 0;\ntype T { pub a; pub b; pub c; }");
    let layout = first_type_layout(&hir, &bindings);

    for (name, expected) in [("a", 0), ("b", 1), ("c", 2)] {
        let sym = hir.symbol_of(name).expect("field name not interned");
        let member = layout.members.get(&sym).copied();
        assert!(matches!(member, Some(TypeMember::Field(id)) if id == expected), "field '{name}' is not id {expected}");
    }
}
