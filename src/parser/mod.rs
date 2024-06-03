use crate::lexer::Token;
use crate::{
    ast,
    lang::{self, Span},
};
use chumsky::prelude::*;

/// The type of the input that our parser operates on. The input is the
/// `&[(Token, Span)]` token buffer generated by the lexer, wrapped in a
/// `SpannedInput` which 'splits' it apart into its constituent parts,
/// tokens and spans, for chumsky to understand.
type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<
    Token<'src>,
    Span,
    &'tokens [(Token<'src>, Span)],
>;

/// Parses an expression.
pub fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    ast::Expr,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        let value = select! {
            Token::Number(n) => n,
        };

        // `Atoms` are expressions that can't be broken down further
        // and are the base case of the recursive parser. They can be
        // either literals or expressions enclosed in parentheses.
        let atom = value
            .map_with(|value, e| ast::Expr {
                kind: ast::ExprKind::Literal(value),
                span: e.span(),
            })
            .or(expr.clone().delimited_by(
                just(Token::Delimiter("(")),
                just(Token::Delimiter(")")),
            ))
            .boxed();

        // Product operators: multiplication (`*`), division (`/`)
        // and remainder (`%`). They have higher precedence than sum
        // operators, thus they are parsed first.
        let product = atom.clone().foldl_with(
            product_ops().then(atom).repeated(),
            |lhs, (op, rhs), e| ast::Expr {
                kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                span: e.span(),
            },
        );

        // Sum operators (e.g. `+` and `-`)
        let sum = product.clone().foldl_with(
            sum_ops().then(product).repeated(),
            |lhs, (op, rhs), e| ast::Expr {
                kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                span: e.span(),
            },
        );

        sum
    })
}

/// Parses product operators tokens:
/// - `*` for multiplication
/// - `/` for division
/// - `%` for remainder
pub fn product_ops<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
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
pub fn sum_ops<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    lang::operator::BinaryOp,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    choice((
        just(Token::Operator("+")).to(lang::operator::BinaryOp::Add),
        just(Token::Operator("-")).to(lang::operator::BinaryOp::Sub),
    ))
}
