use crate::{
    ast,
    lang::{self, Span, Spanned},
    lexer::Token,
};
use chumsky::{input::ValueInput, prelude::*};

/// A type alias for parser errors, which are represented as `extra::Err<Rich<Token, Span>>`. This
/// is very useful to make the function signatures of the parsers less verbose...
type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

/// A parser for an entire source file in the language. Currently, a source file only
/// consists of a list of function definitions, but more top-level constructs can be
/// added in the future, like global variable declarations, struct definitions, enum
/// definitions...
#[must_use]
pub fn file_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<ast::Function<'src>>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    func_parser().repeated().collect()
}

/// A parser for function definitions in the language. A function definition consists of a return
/// type, a name, a list of parameters (currently empty since parameters are not yet supported) and
/// a body (a list of statements).
#[must_use]
pub fn func_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Function<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let ret_type = builtin_type_parser();
    let name = ident_parser();
    let body = stmt_parser()
        .repeated()
        .collect()
        .delimited_by(just(Token::Delimiter("{")), just(Token::Delimiter("}")));

    ret_type
        .then(name)
        .then_ignore(just(Token::Delimiter("(")).ignore_then(just(Token::Delimiter(")"))))
        .then(body)
        .map_with(|((ty, name), body), e| {
            Spanned::new(
                ast::Function {
                    prototype: Spanned::new(
                        ast::FunctionPrototype {
                            ident: name,
                            ret: ty,
                        },
                        e.span(),
                    ),
                    body,
                },
                e.span(),
            )
        })
}

/// A parser for blocks of statements in the language. A block is a sequence of statements
/// enclosed in curly braces `{}`. Blocks are used in function bodies, conditional statements,
/// and loops, but can also be used anywhere a single statement is expected.
#[must_use]
pub fn block_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ast::Block<'src>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    stmt_parser()
        .repeated()
        .collect()
        .delimited_by(just(Token::Delimiter("{")), just(Token::Delimiter("}")))
        .map(|statements| ast::Block {
            stmts: statements,
            ty: lang::Type::Infer,
        })
}

/// A parser for statements in the language. Currently, the only statement that is supported
/// is a `return` statement, but more statements can be added in the future, like variable
/// declarations, variable assignments, if statements, while loops...
#[must_use]
pub fn stmt_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Stmt<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    // Parse a `return` statement, which is an expression of the form `return <expr>;`
    let return_expr = just(Token::Keyword("return"))
        .ignore_then(expr_parser())
        .then_ignore(just(Token::Delimiter(";")))
        .map_with(|expr, e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Return(Box::new(expr)),
                },
                e.span(),
            )
        });

    let let_expr = just(Token::Keyword("let"))
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Delimiter(":")))
        .then(builtin_type_parser().or_not())
        .then_ignore(just(Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(Token::Delimiter(";")))
        .map_with(|((ident, ty), expr), e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Let(
                        ident,
                        ty.unwrap_or(Spanned::none(lang::Type::Infer)),
                        Box::new(expr),
                    ),
                },
                e.span(),
            )
        });

    let var_expr = just(Token::Keyword("var"))
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Delimiter(":")))
        .then(builtin_type_parser().or_not())
        .then_ignore(just(Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(Token::Delimiter(";")))
        .map_with(|((ident, ty), expr), e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Var(
                        ident,
                        ty.unwrap_or(Spanned::none(lang::Type::Infer)),
                        Box::new(expr),
                    ),
                },
                e.span(),
            )
        });

    let assign_expr = ident_parser()
        .then_ignore(just(Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(Token::Delimiter(";")))
        .map_with(|(ident, expr), e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Assign(Box::new(ident), Box::new(expr)),
                },
                e.span(),
            )
        });

    choice((return_expr, let_expr, var_expr, assign_expr)).boxed()
}

/// A parser for expressions in the language. Currently, the only expression that is supported
/// is a number literal, but more expressions can be added in the future, like binary expressions,
/// unary expressions, function calls...
#[must_use]
pub fn expr_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|expr| {
        // Parse a number literal.
        let value = select! {
            Token::Number(val) = e => Spanned::new(
                ast::Literal {
                    ty: lang::Type::Int,
                    value: val,
                },
                e.span()
            )
        };

        // An atom is either a literal or a parenthesized expression. They have the maximum
        // precedence in the expression hierarchy, since they cannot be broken down any further,
        // called "atoms" for that reason.
        let atom = value
            .map_with(|lit, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Literal(lit),
                        ty: lang::Type::Int,
                    },
                    e.span(),
                )
            })
            .or(ident_parser().map_with(|ident, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Identifier(ident),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            }))
            .or(bool_value_parser())
            .or(expr.delimited_by(just(Token::Delimiter("(")), just(Token::Delimiter(")"))))
            .boxed();

        // Parse unary operators (e.g. `-`). These have higher precedence than
        // binary operators, so we parse them first.
        let unary = unary_ops()
            .repeated()
            .foldr_with(atom.clone(), |op, rhs, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Unary(op, Box::new(rhs)),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // Parse product operators (e.g. `*` and `/`). These have higher precedence
        // than sum operators, so we parse them first.
        let product = unary
            .clone()
            .foldl_with(product_ops().then(unary).repeated(), |lhs, (op, rhs), e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // Parse sum operators (e.g. `+` and `-`). These have lower precedence than
        // product operators, so we parse them after.
        let sum = product
            .clone()
            .foldl_with(sum_ops().then(product).repeated(), |lhs, (op, rhs), e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        sum
    })
}

#[must_use]
pub fn unary_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::UnaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    choice((just(Token::Operator("-")).to(lang::UnaryOp::Neg),))
}

/// A parser for addition and subtraction operators in the language.
#[must_use]
pub fn sum_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    choice((
        just(Token::Operator("+")).to(lang::BinaryOp::Add),
        just(Token::Operator("-")).to(lang::BinaryOp::Sub),
    ))
}

/// A parser for multiplication and division operators in the language.
#[must_use]
pub fn product_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    choice((
        just(Token::Operator("*")).to(lang::BinaryOp::Mul),
        just(Token::Operator("/")).to(lang::BinaryOp::Div),
    ))
}

/// A parser for boolean literals in the language. Boolean literals are the
/// keywords `true` and `false`, which represent the two possible values of
/// the boolean type.
#[must_use]
pub fn bool_value_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Keyword("true") = e => Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Bool(Spanned::new(true, e.span())),
                ty: lang::Type::Bool,
            },
            e.span()
        ),
        Token::Keyword("false") = e => Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Bool(Spanned::new(false, e.span())),
                ty: lang::Type::Bool,
            },
            e.span()
        ),
    }
}

/// A parser for identifiers in the language. An identifier is a sequence of
/// characters that represents a name in the language. Identifiers are used to
/// name variables, functions, classes, etc.
#[must_use]
pub fn ident_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Identifier<'src>>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Identifier(name) = e => Spanned::new(ast::Identifier { name }, e.span())
    }
}

/// A parser for built-in types in the language. Currently, the only built-in
/// type is `int`, but more types can be added in the future, like `float`, 'bool',
/// `char`... However, user-defined types (e.g. structs, enums, unions...) will not
/// be handled by this parser.
#[must_use]
pub fn builtin_type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<lang::Type>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Identifier("bool") = e => Spanned::new(lang::Type::Bool, e.span()),
        Token::Identifier("int") = e => Spanned::new(lang::Type::Int, e.span()),
    }
}
