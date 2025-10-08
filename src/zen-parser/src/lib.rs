use chumsky::{input::ValueInput, prelude::*};
use lang::{Span, Spanned};

/// A type alias for parser errors, which are represented as `extra::Err<Rich<Token, Span>>`. This
/// is very useful to make the function signatures of the parsers less verbose...
type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, lexer::Token<'src>, Span>>;

/// A parser for an entire source file in the language. Currently, a source file only
/// consists of a list of function definitions, but more top-level constructs can be
/// added in the future, like global variable declarations, struct definitions, enum
/// definitions...
#[must_use]
pub fn file_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<ast::Function<'src>>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
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
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    // A parser for function parameters, which are of the form `(mut) <ident> : <type>`. Multiple
    // parameters are separated by commas. For example: `a: int, b: int`.
    let parameters = just(lexer::Token::Keyword("mut"))
        .or_not()
        .then(ident_parser())
        .then_ignore(just(lexer::Token::Delimiter(":")))
        .then(builtin_type_parser())
        .map_with(|((mutable, name), ty), e| {
            Spanned::new(
                ast::FunctionParameter {
                    mutable: mutable.is_some(),
                    ident: name,
                    ty,
                },
                e.span(),
            )
        })
        .separated_by(just(lexer::Token::Delimiter(",")))
        .collect();

    builtin_type_parser()
        .then(ident_parser())
        .then(parameters.delimited_by(
            just(lexer::Token::Delimiter("(")),
            just(lexer::Token::Delimiter(")")),
        ))
        .then(stmt_parser().repeated().collect().delimited_by(
            just(lexer::Token::Delimiter("{")),
            just(lexer::Token::Delimiter("}")),
        ))
        .map_with(|(((ty, name), params), body), e| {
            Spanned::new(
                ast::Function {
                    prototype: Spanned::new(
                        ast::FunctionPrototype {
                            ident: name,
                            ret: ty,
                            params,
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
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    stmt_parser()
        .repeated()
        .collect()
        .delimited_by(
            just(lexer::Token::Delimiter("{")),
            just(lexer::Token::Delimiter("}")),
        )
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
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    // Parse a `return` statement, which is an expression of the form `return <expr>;`
    let return_expr = just(lexer::Token::Keyword("return"))
        .ignore_then(expr_parser())
        .then_ignore(just(lexer::Token::Delimiter(";")))
        .map_with(|expr, e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Return(Box::new(expr)),
                },
                e.span(),
            )
        });

    // Parse a `let` statement, which is of the form `let <ident> [: <type>] = <expr>;`. The
    // type annotation is optional, and if it is not provided, the type will be inferred
    // from the expression if possible.
    let let_expr = just(lexer::Token::Keyword("let"))
        .ignore_then(ident_parser())
        .then_ignore(just(lexer::Token::Delimiter(":")))
        .then(builtin_type_parser().or_not())
        .then_ignore(just(lexer::Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(lexer::Token::Delimiter(";")))
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

    // Parse a `var` statement, which is of the form `var <ident> [: <type>] = <expr>;`
    // This is very similar to a let statement, but the variable can be mutated after it
    // is declared, unlike a let statement which declares an immutable variable.
    let var_expr = just(lexer::Token::Keyword("var"))
        .ignore_then(ident_parser())
        .then_ignore(just(lexer::Token::Delimiter(":")))
        .then(builtin_type_parser().or_not())
        .then_ignore(just(lexer::Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(lexer::Token::Delimiter(";")))
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

    // Parse an assignment statement, which is of the form `<ident> [op] = <expr>;`, where
    // `op` is an optional binary operator for compound assignments like `+=`, `-=`...
    let assign_op_expr = ident_parser()
        .then(product_ops().or(sum_ops()).or_not())
        .then_ignore(just(lexer::Token::Operator("=")))
        .then(expr_parser())
        .then_ignore(just(lexer::Token::Delimiter(";")))
        .map_with(|((ident, op), expr), e| {
            Spanned::new(
                ast::Stmt {
                    kind: ast::StmtKind::Assign(op, Box::new(ident), Box::new(expr)),
                },
                e.span(),
            )
        });

    choice((return_expr, let_expr, var_expr, assign_op_expr)).boxed()
}

/// A parser for expressions in the language. Currently, the only expression that is supported
/// is a number literal, but more expressions can be added in the future, like binary expressions,
/// unary expressions, function calls...
#[must_use]
pub fn expr_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    recursive(|expr| {
        // Parse a number literal.
        let value = select! {
            lexer::Token::Number(val) = e => Spanned::new(
                ast::Literal {
                    ty: lang::Type::Int,
                    value: val,
                },
                e.span()
            )
        };

        // An identifier is a sequence of characters that represents a name in the language.
        // Identifiers are used to name variables, functions, classes, etc.
        let identifier = ident_parser().map_with(|ident, e| {
            Spanned::new(
                ast::Expr {
                    kind: ast::ExprKind::Identifier(ident),
                    ty: lang::Type::Infer,
                },
                e.span(),
            )
        });

        // A list of expressions separated by commas. This is mostly useful for function
        // call arguments, but can also be used in other places where a list of expressions
        // is expected.
        let items = expr
            .clone()
            .separated_by(just(lexer::Token::Delimiter(",")))
            .collect();

        // A function call is an identifier followed by a list of arguments enclosed
        // in parentheses. For example: `foo(a, b + 2)`. The arguments are optional,
        // so a function call can also be just an identifier followed by empty
        // parentheses, like `foo()`.
        let call = ident_parser()
            .then(items.delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .map_with(|(function, args), e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::FunctionCall(Box::new(function), args),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

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
            .or(call)
            .or(identifier)
            .or(bool_value_parser())
            .or(expr.delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .boxed();

        // Now we will parse operators based on their precedence, from highest to lowest. If two
        // operators have the same precedence, they will be parsed based on their associativity,
        // which is left-to-right for all binary operators in this language.

        // Parse unary operators.
        let unary = unary_ops()
            .repeated()
            .foldr_with(atom, |op, rhs, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Unary(op, Box::new(rhs)),
                        ty: lang::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // Parse product operators.
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

        // Parse sum operators.
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

        // Parse relational operators.
        let relational_ops = sum
            .clone()
            .foldl_with(
                relational_ops().then(sum).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        // Parse logical AND operator.
        let logical_and = relational_ops
            .clone()
            .foldl_with(
                logical_and_ops().then(relational_ops).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        // Parse logical OR operator.
        let logical_or = logical_and
            .clone()
            .foldl_with(
                logical_or_ops().then(logical_and).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        logical_or
    })
}

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

/// A parser for addition and subtraction operators in the language.
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

/// A parser for multiplication and division operators in the language.
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

/// A parser for relational operators in the language.
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

/// A parser for logical AND operators in the language.
#[must_use]
pub fn logical_and_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    just(lexer::Token::Operator("&&")).to(lang::BinaryOp::And)
}

/// A parser for logical OR operators in the language.
#[must_use]
pub fn logical_or_ops<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, lang::BinaryOp, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    just(lexer::Token::Operator("||")).to(lang::BinaryOp::Or)
}

/// A parser for boolean literals in the language. Boolean literals are the
/// keywords `true` and `false`, which represent the two possible values of
/// the boolean type.
#[must_use]
pub fn bool_value_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Keyword("true") = e => Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Bool(Spanned::new(true, e.span())),
                ty: lang::Type::Bool,
            },
            e.span()
        ),
        lexer::Token::Keyword("false") = e => Spanned::new(
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
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier(name) = e => Spanned::new(ast::Identifier { name }, e.span())
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
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    select! {
        lexer::Token::Identifier("bool") = e => Spanned::new(lang::Type::Bool, e.span()),
        lexer::Token::Identifier("int") = e => Spanned::new(lang::Type::Int, e.span()),
    }
}
