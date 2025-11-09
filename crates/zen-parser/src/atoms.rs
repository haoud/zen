//! A parser for atomic expressions in the Zen programming language. Atomic expressions
//! are the simplest forms of expressions that cannot be broken down further, and therefore
//! serve as the building blocks for more complex expressions.
use chumsky::{input::ValueInput, prelude::*};
use lang::ty::{BuiltinType, Type};
use span::{Span, Spanned};

use super::ParserError;

/// A parser for numeric literals in the Zen programming language. This parser recognizes
/// numeric tokens and constructs `ast::Literal` instances representing integer literals.
#[must_use]
pub fn number_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Literal<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Number(val) = e => Spanned::new(
            ast::Literal {
                ty: Type::Builtin(BuiltinType::Int),
                value: val,
            },
            e.span()
        )
    }
}

/// A parser for identifiers in the language. An identifier is a sequence of characters that
/// represents a name in the language. Identifiers are used to name variables, functions, types...
#[must_use]
pub fn identifier_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Identifier<'src>>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier(name) = e => Spanned::new(ast::Identifier { name }, e.span())
    }
}

/// A parser for boolean literals in the language. Boolean literals are the keywords `true` and
/// `false`, which represent the two possible values of the boolean type.
#[must_use]
pub fn boolean_value_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Keyword("true") = e => Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Bool(Spanned::new(true, e.span())),
                ty: Type::Builtin(BuiltinType::Bool),
            },
            e.span()
        ),
        lexer::Token::Keyword("false") = e => Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Bool(Spanned::new(false, e.span())),
                ty: Type::Builtin(BuiltinType::Bool),
            },
            e.span()
        ),
    }
}

/// A parser for built-in types in the language. Built-in types are the primitive types provided
/// by the language, such as `bool`, `int`, `str`, and `void`.
#[must_use]
pub fn builtin_type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<lang::ty::Type>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier("bool") = e => Spanned::new(Type::Builtin(BuiltinType::Bool), e.span()),
        lexer::Token::Identifier("int") = e => Spanned::new(Type::Builtin(BuiltinType::Int), e.span()),
        lexer::Token::Identifier("str") = e => Spanned::new(Type::Builtin(BuiltinType::Str), e.span()),
        lexer::Token::Identifier("void") = e => Spanned::new(Type::Builtin(BuiltinType::Void), e.span()),
    }
}

/// A parser for string literals in the language. String literals are sequences of characters
/// enclosed in double quotes.
#[must_use]
pub fn string_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<String>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::String(s) = e => Spanned::new(s, e.span())
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
