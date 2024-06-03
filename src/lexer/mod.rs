//! The lexer module contains the implementation of the lexer for the
//! programming language. The lexer is responsible for recognizing the
//! different tokens in the input string and returning them to the parser.
//! The lexer is implemented using the amazing `chumsky` parser combinator
//! library and is defined as a sequence of parser combinators that recognize
//! the different tokens in the input string.
//!
//! The code here is short and quite simple because the `chumsky` library
//! abstracts away most of the complexity of writing a lexer, which is nice
//! as it allows us to focus on the language itself rather than some "boring"
//! implementation details.
use crate::lang::Span;
use chumsky::prelude::*;

/// A token is a single unit of the input string that represents
/// a single entity in the language. For example, an identifier,
/// a keyword, an operator...
///
/// The lexer's job is to take the input string and split it into
/// a sequence of tokens. The parser will then take this sequence
/// of tokens and build an abstract syntax tree (AST) that represents
/// the structure of the program. The lexer is responsible for
/// recognizing the different tokens in the input string and returning
/// them to the parser.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    /// An operator is a special combination of characters that
    /// represents an operation in the language. For example, `+`
    /// is an operator that represents an addition operation, and
    /// `==` is an operator that represents an equality comparison.
    /// An operator can have different meanings depending on the
    /// context in which it is used. For example, `&` can either
    /// represent a bitwise AND operation or an address-of operator,
    /// depending on the context.
    Operator(&'a str),

    /// A delimiter is a special character that separates tokens
    /// in the input string. For example, a semicolon `;` is a delimiter
    /// as well as a comma `,`, brackets `[]`, parentheses `()`, etc.
    Delimiter(&'a str),

    /// A number literal
    Number(u64),
}

impl core::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Token::Delimiter(delim) => write!(f, "{}", delim),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Number(num) => write!(f, "{}", num),
        }
    }
}

/// Lexes the input string and returns a sequence of tokens that represent
/// the different entities in the input string. The lexer is responsible
/// for recognizing the different tokens in the input string and returning
/// them to the parser, which will then build an abstract syntax tree (AST)
/// with those tokens.
pub fn lexer<'a>(
) -> impl Parser<'a, &'a str, Vec<(Token<'a>, Span)>, extra::Err<Rich<'a, char, Span>>>
{
    // A simpler parser for integers that supports different
    // common bases for integers (e.g. 0x for hexadecimal, 0b
    // for binary...)
    let number = choice((
        just("0x")
            .ignore_then(text::int(16))
            .map(|x| u64::from_str_radix(x, 16).unwrap()),
        just("0b")
            .ignore_then(text::int(2))
            .map(|x| u64::from_str_radix(x, 2).unwrap()),
        just("0o")
            .ignore_then(text::int(8))
            .map(|x| u64::from_str_radix(x, 8).unwrap()),
        text::int(10).from_str().map(|x| x.unwrap()),
    ))
    .map(Token::Number);

    // A lexer for delimiters.
    let delimiter = choice((just("("), just(")")))
        .to_slice()
        .map(Token::Delimiter);

    // A parser for operators. Operators are special combinations of
    // one or more characters that represent an operation in the language.
    // For example, `+` is an operator that represents an addition operation,
    // and `==` is an operator that represents an equality comparison.
    //
    // However, operators can be ambiguous. For example, `&` can either
    // represent a bitwise AND operation or an address-of operator, depending
    // on the context. To resolve this ambiguity, we need to consider the
    // surrounding tokens when parsing operators. This will not be covered
    // in the lexer, but will be handled in the parser.
    let operator = choice((
        just("+"), // Addition
        just("-"), // Subtraction
        just("*"), // Multiplication
        just("/"), // Division
        just("%"), // Modulo
    ))
    .to_slice()
    .map(Token::Operator);

    // A single token can be one of the above
    let token = choice((number, delimiter, operator));

    // A parser for comments. Comments start with `//` and end
    // with a newline character. Comments are ignored by the
    // parser and are not returned as tokens
    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    // Create the final lexer by combining the token parser with
    // the comment parser. The lexer will first attempt to parse
    // a token, and if that fails, it will skip characters until
    // the end of the input string or until it finds a new
    // valid token. This allows the lexer to recover from errors
    // and continue parsing the input string.
    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next
        // character as a token instead to recover from the error
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    /// Test the lexer with a simple number operation.
    fn lexer_test_expr(operator: &str) {
        let input = format!("1 {} 2", operator);
        let tokens = lexer().parse(&input).unwrap();
        assert_eq!(
            tokens,
            vec![
                (Token::Number(1), Span::new(0, 1)),
                (Token::Operator(operator), Span::new(2, 3)),
                (Token::Number(2), Span::new(4, 5)),
            ]
        );
    }

    /// Test the lexer with a simple addition expression
    #[test]
    fn lexer_add_expr() {
        lexer_test_expr("+");
    }

    /// Test the lexer with a simple subtraction expression
    #[test]
    fn lexer_sub_expr() {
        lexer_test_expr("-");
    }

    /// Test the lexer with a simple multiplication expression
    #[test]
    fn lexer_mul_expr() {
        lexer_test_expr("*");
    }

    /// Test the lexer with a simple division expression
    #[test]
    fn lexer_div_expr() {
        lexer_test_expr("/");
    }

    /// Test the lexer with a simple modulo expression
    #[test]
    fn lexer_mod_expr() {
        lexer_test_expr("%");
    }

    /// Test the lexer with a binary literal
    #[test]
    fn lexer_binary_literal() {
        let input = "0b1010";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(tokens, vec![(Token::Number(0b1010), Span::new(0, 6))]);
    }

    /// Test the lexer with an octal literal
    #[test]
    fn lexer_octal_literal() {
        let input = "0o777";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(tokens, vec![(Token::Number(0o777), Span::new(0, 5))]);
    }

    /// Test the lexer with a hexadecimal literal
    #[test]
    fn lexer_hexadecimal_literal() {
        let input = "0xdeadbeef";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(tokens, vec![(Token::Number(0xdeadbeef), Span::new(0, 10))]);
    }

    /// Test the lexer with all delimiters supported by the language
    #[test]
    fn lexer_delimiters() {
        let input = "( )";
        let tokens = lexer().parse(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                (Token::Delimiter("("), Span::new(0, 1)),
                (Token::Delimiter(")"), Span::new(2, 3)),
            ]
        );
    }

    /// Test the lexer with an invalid input string that contains
    /// unsupported characters
    #[test]
    #[should_panic]
    fn lexer_invalid_input() {
        lexer().parse("ù£").unwrap();
    }
}
