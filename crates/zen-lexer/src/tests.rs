//! This module contains unit tests for the lexer implementation. The tests cover various
//! aspects of the lexer, including token recognition, handling of whitespace and comments,
//! and error recovery.
//! Any bug fixes or improvements to the lexer should be accompanied by corresponding tests
//! in this module to ensure that the lexer behaves as expected.
use crate::*;
use chumsky::Parser;
use lang::{Literal, LiteralBase};

#[test]
pub fn lex_keywords() {
    let input = "return true false let var mut if else while struct";
    let expected = vec![
        Token::Keyword("return"),
        Token::Keyword("true"),
        Token::Keyword("false"),
        Token::Keyword("let"),
        Token::Keyword("var"),
        Token::Keyword("mut"),
        Token::Keyword("if"),
        Token::Keyword("else"),
        Token::Keyword("while"),
        Token::Keyword("struct"),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
pub fn lex_numbers() {
    let input = "42 0x2A 0b101010 0o52";
    let expected = vec![
        Token::Number(Literal::new("42", LiteralBase::Decimal)),
        Token::Number(Literal::new("2A", LiteralBase::Hexadecimal)),
        Token::Number(Literal::new("101010", LiteralBase::Binary)),
        Token::Number(Literal::new("52", LiteralBase::Octal)),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
pub fn lex_strings() {
    let input = r#""hello" "world" "zen lexer""#;
    let expected = vec![
        Token::String("hello".to_string()),
        Token::String("world".to_string()),
        Token::String("zen lexer".to_string()),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
pub fn lex_operators() {
    let input = "+ - * / = == != < <= > >= && || ! @";
    let expected = vec![
        Token::Operator("+"),
        Token::Operator("-"),
        Token::Operator("*"),
        Token::Operator("/"),
        Token::Operator("="),
        Token::Operator("=="),
        Token::Operator("!="),
        Token::Operator("<"),
        Token::Operator("<="),
        Token::Operator(">"),
        Token::Operator(">="),
        Token::Operator("&&"),
        Token::Operator("||"),
        Token::Operator("!"),
        Token::Operator("@"),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
pub fn lex_delimiters() {
    let input = "( ) [ ] { } . , : ; :: ..";
    let expected = vec![
        Token::Delimiter("("),
        Token::Delimiter(")"),
        Token::Delimiter("["),
        Token::Delimiter("]"),
        Token::Delimiter("{"),
        Token::Delimiter("}"),
        Token::Delimiter("."),
        Token::Delimiter(","),
        Token::Delimiter(":"),
        Token::Delimiter(";"),
        Token::Delimiter("::"),
        Token::Delimiter(".."),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
pub fn lex_mixed_input() {
    let input = r#"
        // A simple function that returns "big" or "small"
        let x := 42;
        if (x >= 0x10) {
            return /* Inline comment*/ "big";
        } else {
            return "small";
        }"#;

    let expected = vec![
        Token::Keyword("let"),
        Token::Identifier("x"),
        Token::Delimiter(":"),
        Token::Operator("="),
        Token::Number(Literal::new("42", LiteralBase::Decimal)),
        Token::Delimiter(";"),
        Token::Keyword("if"),
        Token::Delimiter("("),
        Token::Identifier("x"),
        Token::Operator(">="),
        Token::Number(Literal::new("10", LiteralBase::Hexadecimal)),
        Token::Delimiter(")"),
        Token::Delimiter("{"),
        Token::Keyword("return"),
        Token::String("big".to_string()),
        Token::Delimiter(";"),
        Token::Delimiter("}"),
        Token::Keyword("else"),
        Token::Delimiter("{"),
        Token::Keyword("return"),
        Token::String("small".to_string()),
        Token::Delimiter(";"),
        Token::Delimiter("}"),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

/// Ensure that an identifier can contain a keyword as a substring without being misclassified.
/// For example, "whileloop" should be classified as an identifier, not as the keyword "while"
/// followed by "loop".
///
/// This test was written to catch a previous bug in the lexer implementation.
#[test]
pub fn lex_identifier_with_keyword() {
    let input = "whileloop orelse anifcondition";
    let expected = vec![
        Token::Identifier("whileloop"),
        Token::Identifier("orelse"),
        Token::Identifier("anifcondition"),
    ];
    let result = lexer()
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|spanned| spanned.into_inner())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}
