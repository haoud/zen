use crate::lexer::Token;
use crate::{
    ast,
    lang::{self, Span},
};
use chumsky::prelude::*;

/// Parses an identifier token and returns an `ast::Identifier` which is
/// composed of the name of the identifier and its span.
#[must_use]
pub fn identifier<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    super::ParserInput<'tokens, 'src>,
    ast::Identifier<'src>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    select! {
        Token::Identifier(name) = e => ast::Identifier {
            name: name,
            span: e.span(),
        }
    }
}

/// Parses product operators tokens:
/// - `*` for multiplication
/// - `/` for division
/// - `%` for remainder
#[must_use]
pub fn product_ops<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    super::ParserInput<'tokens, 'src>,
    lang::operator::BinaryOp,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    choice((
        just(Token::Operator("*")).to(lang::operator::BinaryOp::Mul),
        just(Token::Operator("/")).to(lang::operator::BinaryOp::Div),
        just(Token::Operator("%")).to(lang::operator::BinaryOp::Mod),
    ))
}

/// Parses sum operators tokens:
/// - `+` for addition
/// - `-` for subtraction
#[must_use]
pub fn sum_ops<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    super::ParserInput<'tokens, 'src>,
    lang::operator::BinaryOp,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    choice((
        just(Token::Operator("+")).to(lang::operator::BinaryOp::Add),
        just(Token::Operator("-")).to(lang::operator::BinaryOp::Sub),
    ))
}

/// Parses builtins types. Thoses types are the basic types that are
/// built-in in the language and that are not user-defined. However,
/// unlike some languages, Zen built-in types are not keywords, they
/// are just special identifiers that are recognized by the parser in
/// certain contexts.
#[must_use]
pub fn builtin_types<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    super::ParserInput<'tokens, 'src>,
    lang::Type,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    choice((
        just(Token::Identifier("i8")).map(|_| lang::types::Type::I8),
        just(Token::Identifier("u8")).map(|_| lang::types::Type::U8),
        just(Token::Identifier("i16")).map(|_| lang::types::Type::I16),
        just(Token::Identifier("u16")).map(|_| lang::types::Type::U16),
        just(Token::Identifier("i32")).map(|_| lang::types::Type::I32),
        just(Token::Identifier("u32")).map(|_| lang::types::Type::U32),
        just(Token::Identifier("i64")).map(|_| lang::types::Type::I64),
        just(Token::Identifier("u64")).map(|_| lang::types::Type::U64),
        just(Token::Identifier("int")).map(|_| lang::types::Type::Int),
        just(Token::Identifier("uint")).map(|_| lang::types::Type::Uint),
        just(Token::Identifier("bool")).map(|_| lang::types::Type::Bool),
    ))
}
