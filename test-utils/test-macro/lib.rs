use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, ItemFn, LitStr};

#[proc_macro_attribute]
pub fn test_resources(args: TokenStream, input: TokenStream) -> TokenStream {
    let folder = parse_macro_input!(args as LitStr);
    let ItemFn { attrs, vis, sig, block } = parse_macro_input!(input as ItemFn);
    let attrs = attrs.iter()
        .filter(|attr| !attr.path().is_ident("test_resources"))
        .filter(|attr| !attr.path().is_ident("test"))
        .collect::<Vec<_>>();

    let ident = &sig.ident;
    let test_name = ident.to_string();
    let ident_init = format_ident!("{}_init", ident);
    let ident_test_dummy = format_ident!("{}_test_dummy", ident);

    (quote! {
        #(#attrs)* #vis #sig #block

        #[test]
        fn #ident_test_dummy() {
            // This is a dummy test to ensure the test function is not empty.
            // The actual tests are collected in the constructor.
            unreachable!();
        }
        
        ::test_collector::ctor::declarative::ctor! {
            #[ctor]
            fn #ident_init() {
                ::test_collector::TestCollection::add_tests(#ident, #test_name, #folder);
            }
        }
    }).into()
}