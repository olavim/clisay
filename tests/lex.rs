use clisay::internals::{lex, ContextualKeyword, TokenType};

/// Token kinds of a source string, excluding the trailing EOF.
fn kinds(src: &str) -> Vec<TokenType> {
    lex(src).iter().map(|t| t.kind).filter(|k| *k != TokenType::EOF).collect()
}

#[test]
fn lexes_question_mark() {
    // `?` is a single-char token; the multi-char `??`/`?.`/`?[` forms are assembled
    // in the parser from adjacent tokens, like `&&` and `==`.
    assert_eq!(kinds("?"), vec![TokenType::Question]);
    assert_eq!(kinds("??"), vec![TokenType::Question, TokenType::Question]);
    assert_eq!(kinds("?."), vec![TokenType::Question, TokenType::Dot]);
    assert_eq!(kinds("?["), vec![TokenType::Question, TokenType::LeftBracket]);
}

#[test]
fn lexes_exclamation() {
    assert_eq!(kinds("!"), vec![TokenType::Exclamation]);
}

#[test]
fn mut_is_a_contextual_keyword() {
    // `mut` is a modifier in declaration position, like `pub`/`inner`, so it lexes as
    // an identifier and is recognized contextually.
    let toks = lex("mut");
    assert_eq!(toks[0].kind, TokenType::Identifier);
    assert_eq!(toks[0].contextual(), Some(ContextualKeyword::Mut));
}
