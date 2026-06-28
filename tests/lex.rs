use clisay::internals::{lex, ContextualKeyword, TokenType};

/// Token kinds of a source string, excluding the trailing EOF.
fn kinds(src: &str) -> Vec<TokenType> {
    lex(src).iter().map(|t| t.kind).filter(|k| *k != TokenType::EOF).collect()
}

#[test]
fn lexes_question_mark() {
    // A bare `?` is the nullable marker; the `??`/`?.`/`?[` forms are their own tokens.
    assert_eq!(kinds("?"), vec![TokenType::Question]);
    assert_eq!(kinds("??"), vec![TokenType::QuestionQuestion]);
    assert_eq!(kinds("?."), vec![TokenType::QuestionDot]);
    assert_eq!(kinds("?["), vec![TokenType::QuestionBracket]);
}

#[test]
fn lexes_exclamation() {
    assert_eq!(kinds("!"), vec![TokenType::Exclamation]);
}

#[test]
fn lexes_match_bind_arrow_greedily() {
    // `<-` is one token, so `a<-b` is a match-bind, never `a < -b`.
    assert_eq!(kinds("a<-b"), vec![TokenType::Identifier, TokenType::LeftArrow, TokenType::Identifier]);
    // A spaced less-than against a negated operand stays two tokens.
    assert_eq!(kinds("a < -b"), vec![TokenType::Identifier, TokenType::LessThan, TokenType::Minus, TokenType::Identifier]);
}

#[test]
fn lexes_rest_and_as_and_match() {
    assert_eq!(kinds(".."), vec![TokenType::DotDot]);
    assert_eq!(kinds("..rest"), vec![TokenType::DotDot, TokenType::Identifier]);
    // A lone `.` stays member access, distinct from the `..` rest token.
    assert_eq!(kinds("a.b"), vec![TokenType::Identifier, TokenType::Dot, TokenType::Identifier]);
    assert_eq!(kinds("@"), vec![TokenType::At]);
    assert_eq!(kinds("match"), vec![TokenType::Match]);
}

#[test]
fn lexes_multi_char_operators() {
    assert_eq!(kinds("<="), vec![TokenType::LessEqual]);
    assert_eq!(kinds("&&"), vec![TokenType::AmpAmp]);
    assert_eq!(kinds("=>"), vec![TokenType::FatArrow]);
    assert_eq!(kinds(">>"), vec![TokenType::GreaterGreater]);
    // Longest match wins: `<<=` is one token, not `<<` then `=`.
    assert_eq!(kinds("<<="), vec![TokenType::LessLessEqual]);
    // Spacing splits them: `< =` is two tokens, not `<=`.
    assert_eq!(kinds("< ="), vec![TokenType::LessThan, TokenType::Equal]);
}

#[test]
fn mut_is_a_contextual_keyword() {
    // `mut` is a modifier in declaration position, like `pub`/`inner`, so it lexes as
    // an identifier and is recognized contextually.
    let toks = lex("mut");
    assert_eq!(toks[0].kind, TokenType::Identifier);
    assert_eq!(toks[0].contextual(), Some(ContextualKeyword::Mut));
}
