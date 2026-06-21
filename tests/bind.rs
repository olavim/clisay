use clisay::internals::{bind, Hir, HirStmt, TypeLayout};

/// The `TypeLayout` of the first top-level `type` declaration.
fn first_type_layout<'a>(hir: &Hir, bindings: &'a clisay::internals::Bindings) -> &'a TypeLayout {
    let root = hir.get_root();
    let HirStmt::Expression(block) = hir.get(&root) else { panic!("root is not an expression statement") };
    let clisay::internals::HirExpr::Block(stmts) = hir.get(block) else { panic!("root is not a block") };
    let stmt = stmts.iter().find(|s| matches!(hir.get(*s), HirStmt::Type(_))).expect("no type declaration");
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
    let (hir, bindings) = bind("type T { pub fn maybe()? { return null; } pub fn always()! { return 1; } }");
    let layout = first_type_layout(&hir, &bindings);

    let maybe = hir.symbol_of("maybe").expect("maybe not interned");
    let always = hir.symbol_of("always").expect("always not interned");

    assert!(layout.is_nullable(maybe));
    assert!(!layout.is_nullable(always));
}
