//! A parser for atomic expressions in the Zen programming language. Atomic expressions
//! are the simplest forms of expressions that cannot be broken down further, and therefore
//! serve as the building blocks for more complex expressions.
use chumsky::{input::ValueInput, prelude::*};
use lang::ty::TypeSpecifier;
use span::Span;

use super::ParserError;

/// A parser for numeric literals in the Zen programming language. This parser recognizes
/// numeric tokens and constructs `ast::Literal` instances representing integer literals.
#[must_use]
pub fn number_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ast::Literal<'src>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Number(val) = e =>
            ast::Literal {
                ty: TypeSpecifier::INT,
                value: val,
                span: e.span(),
            },
    }
}

/// A parser for identifiers in the language. An identifier is a sequence of characters that
/// represents a name in the language. Identifiers are used to name variables, functions, types...
#[must_use]
pub fn identifier_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ast::Identifier<'src>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier(name) = e => ast::Identifier { name, span: e.span() }
    }
}

/// A parser for boolean literals in the language. Boolean literals are the keywords `true` and
/// `false`, which represent the two possible values of the boolean type.
#[must_use]
pub fn boolean_value_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ast::Expr<'src>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Keyword("true") = e =>
            ast::Expr {
                kind: ast::ExprKind::Bool(true),
                ty: TypeSpecifier::BOOL,
                span: e.span(),
            },
        lexer::Token::Keyword("false") = e =>
            ast::Expr {
                kind: ast::ExprKind::Bool(false),
                ty: TypeSpecifier::BOOL,
                span: e.span(),
            },
    }
}

/// A parser for built-in types in the language. Built-in types are the primitive types provided
/// by the language, such as `bool`, `int`, `str`, and `void`.
#[must_use]
pub fn builtin_type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ast::Type, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier("bool") = e => ast::Type {
            specifier: TypeSpecifier::BOOL,
            span: e.span()
        },
        lexer::Token::Identifier("int") = e => ast::Type {
            specifier: TypeSpecifier::INT,
            span: e.span()
        },
        lexer::Token::Identifier("str") = e => ast::Type {
            specifier: TypeSpecifier::STR,
            span: e.span()
        },
        lexer::Token::Identifier("void") = e => ast::Type {
            specifier: TypeSpecifier::VOID,
            span: e.span()
        },
    }
}

/// A parser for string literals in the language. String literals are sequences of characters
/// enclosed in double quotes. This parser returns only the string value, without span information.
#[must_use]
pub fn string_value_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, String, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::String(s) => s
    }
}

/// A parser for unary operators in the language (!, -).
#[must_use]
pub fn unary_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::UnaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    choice((
        just(lexer::Token::Operator("-")).to(lang::UnaryOp::Neg),
        just(lexer::Token::Operator("!")).to(lang::UnaryOp::Not),
    ))
}

/// A parser for addition (+) and subtraction (-) operators in the language.
#[must_use]
pub fn sum_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    choice((
        just(lexer::Token::Operator("+")).to(lang::BinaryOp::Add),
        just(lexer::Token::Operator("-")).to(lang::BinaryOp::Sub),
    ))
}

/// A parser for multiplication (*) and division (/) operators in the language.
#[must_use]
pub fn product_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    choice((
        just(lexer::Token::Operator("*")).to(lang::BinaryOp::Mul),
        just(lexer::Token::Operator("/")).to(lang::BinaryOp::Div),
    ))
}

/// A parser for relational operators in the language (==, !=, <, <=, >, >=).
#[must_use]
pub fn relational_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    choice((
        just(lexer::Token::Operator("==")).to(lang::BinaryOp::Eq),
        just(lexer::Token::Operator("!=")).to(lang::BinaryOp::Neq),
        just(lexer::Token::Operator("<")).to(lang::BinaryOp::Lt),
        just(lexer::Token::Operator("<=")).to(lang::BinaryOp::Lte),
        just(lexer::Token::Operator(">")).to(lang::BinaryOp::Gt),
        just(lexer::Token::Operator(">=")).to(lang::BinaryOp::Gte),
    ))
}

/// A parser for logical AND operators in the language (&&).
#[must_use]
pub fn logical_and_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    just(lexer::Token::Operator("&&")).to(lang::BinaryOp::And)
}

/// A parser for logical OR operators in the language (||).
#[must_use]
pub fn logical_or_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    just(lexer::Token::Operator("||")).to(lang::BinaryOp::Or)
}
