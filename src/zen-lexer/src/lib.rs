//! The lexer module contains the implementation of the lexer for the programming language. The
//! lexer is responsible for recognizing the different tokens in the input string and returning
//! them to the parser.
//!
//! The lexer is implemented using the amazing `chumsky` parser combinator library and is defined
//! as a sequence of parser combinators that recognize the different tokens in the input string.
//!
//! The code here is short and quite simple because the `chumsky` library abstracts away most of
//! the complexity of writing a lexer, which is nice as it allows us to focus on the language
//! itself rather than some "boring" implementation details.
use chumsky::prelude::*;
use lang::{Literal, LiteralBase, Span, Spanned};

/// A token is a single unit of the input string that represents a single entity in the language.
/// For example, an identifier, a keyword, an operator...
///
/// The lexer's job is to take the input string and split it into a sequence of tokens. The parser
/// will then take this sequence of tokens and build an abstract syntax tree (AST) that represents
/// the structure of the program. The lexer is responsible for recognizing the different tokens in
/// the input string and returning them to the parser.
///
/// For simplicity, the lexer here use slices of the input string to represent the value of tokens
/// such as identifiers, keywords, operators, and delimiters. However, this is less efficient than
/// using an enum to represent these tokens as it requires comparing strings instead of comparing
/// enum variants, which is faster. But this is simpler to implement and understand.
/// TODO: Consider changing this in the future when performance becomes a concern.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    /// A keyword is a special identifier that has a predefined meaning in the language.
    /// Keywords are reserved and cannot be used as identifiers. For example, `if`, `else`,
    /// `while`, `for`, `return` are all keywords in most programming languages.
    Keyword(&'a str),

    /// An identifier is a sequence of characters that represents a name in the language.
    /// Identifiers are used to name variables, functions, classes, etc. For example, `x`, `y`,
    /// `foo`, `bar` are all valid identifiers. Identifiers can contain letters, digits, and
    /// underscores, but they cannot start with a digit to avoid ambiguity with number literals.
    Identifier(&'a str),

    /// An operator is a special combination of characters that represents an operation in the
    /// language. For example, `+` is an operator that represents an addition operation, and
    /// `==` is an operator that represents an equality comparison. An operator can have different
    /// meanings depending on the context in which it is used. For example, `&` can either represent
    /// a bitwise AND operation or an address-of operator, depending on the context.
    Operator(&'a str),

    /// A delimiter is a special character that separates tokens in the input string. For example,
    /// a semicolon `;` is a delimiter  as well as a comma `,`, brackets `[]`, parentheses `()`...
    Delimiter(&'a str),

    /// A number literal
    Number(Literal<'a>),
}

impl core::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Token::Identifier(ident) => write!(f, "{ident}"),
            Token::Delimiter(delim) => write!(f, "{delim}"),
            Token::Operator(op) => write!(f, "{op}"),
            Token::Keyword(kw) => write!(f, "{kw}"),
            Token::Number(lit) => write!(f, "{lit}"),
        }
    }
}

/// Lexes the input string and returns a sequence of tokens that represent the different
/// entities in the input string. The lexer is responsible for recognizing the different
/// tokens in the input string and returning them to the parser, which will then build an
/// abstract syntax tree (AST) with those tokens.
///
/// # Panics
/// This function should never panic in theory. However, panic actually can happen if there is
/// the lexer cannot parse an number literal due to an invalid format. For example, `0xGHI`
/// is not a valid hexadecimal number, so the lexer will panic when it tries to parse it.
/// FIXME: Change this function to not panic on invalid number literals.
#[must_use]
pub fn lexer<'a>()
-> impl Parser<'a, &'a str, Vec<Spanned<Token<'a>>>, extra::Err<Rich<'a, char, Span>>> {
    // A simple lexer for parsing keywords.
    let keyword = choice((
        just("return"),
        just("true"),
        just("false"),
        just("let"),
        just("var"),
        just("mut"),
    ))
    .map(Token::Keyword);

    // A lexer for parsing C-style identifiers
    let ident = text::ascii::ident().map(Token::Identifier);

    // A simpler parser for integers that supports different
    // common bases for integers (e.g. 0x for hexadecimal, 0b
    // for binary...)
    let number = choice((
        just("0x")
            .ignore_then(text::int(16))
            .map(|s| (s, LiteralBase::Hexadecimal)),
        just("0b")
            .ignore_then(text::int(2))
            .map(|s| (s, LiteralBase::Binary)),
        just("0o")
            .ignore_then(text::int(8))
            .map(|s| (s, LiteralBase::Octal)),
        text::int(10).map(|s| (s, LiteralBase::Decimal)),
    ))
    .map(|(num_str, base)| Token::Number(Literal::new(num_str, base)));

    // A parser for operators. Operators are special combinations of
    // one or more characters that represent an operation in the language.
    // For example, `+` is an operator that represents an addition operation,
    // and `==` is an operator that represents an equality comparison.
    //
    // However, operators can be ambiguous. For example, `&` can either
    // represent a bitwise AND operation or an address-of operator, depending
    // on the context. To resolve this ambiguity, we must consider longer
    // operators first before shorter operators. To ensure that longer operators
    // will not be consumed as shorter operators.
    let operator = choice((just("+"), just("-"), just("*"), just("/"), just("=")))
        .to_slice()
        .map(Token::Operator);

    // A lexer for delimiters.
    let delimiter = choice((
        choice((just("::"), just(".."))),
        choice((
            just("("),
            just(")"),
            just("["),
            just("]"),
            just("{"),
            just("}"),
            just(":"),
            just(";"),
            just(","),
        )),
    ))
    .to_slice()
    .map(Token::Delimiter);

    // A single token can be one of the above
    let token = choice((keyword, ident, operator, delimiter, number));

    // A parser for comments. Comments start with `//` and end with a newline
    // character. Comments are ignored by the parser and are not returned as tokens
    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    // A parser for multiline comments. Multiline comments start with `/*`
    // and end with `*/`. Multiline comments can span multiple lines and are
    // ignored by the parser and are not returned as tokens. Nested comments
    // are not supported.
    let multiline_comment = just("/*")
        .then(any().and_is(just("*/").not()).repeated())
        .then_ignore(just("*/"))
        .padded();

    // Create the final lexer by combining the token parser with the comment parser.
    // The lexer will first attempt to parse a token, and if that fails, it will skip
    // characters until the end of the input string or until it finds a new valid token.
    // This allows the lexer to recover from errors and continue parsing the input string
    // to allow for better error reporting and partial AST generation.
    token
        .map_with(|tok, e| Spanned::new(tok, e.span()))
        .padded_by(multiline_comment.repeated())
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
