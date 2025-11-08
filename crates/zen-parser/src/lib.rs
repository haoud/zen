use ast::TopLevelItemKind;
use chumsky::{input::ValueInput, prelude::*};
use lang::{
    Span, Spanned,
    ty::{BuiltinType, Type},
};

pub mod atoms;

/// A type alias for parser errors, which are represented as `extra::Err<Rich<Token, Span>>`. This
/// is very useful to make the function signatures of the parsers less verbose...
type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, lexer::Token<'src>, Span>>;

/// A parser for an entire source file in the language. A source file consists of a list of
/// top-level items that can be interleaved in any order.
#[must_use]
pub fn file_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<ast::TopLevelItem<'src>>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    choice((
        func_parser().map(|sp| TopLevelItemKind::Function(sp)),
        struct_parser().map(|sp| TopLevelItemKind::Struct(sp)),
    ))
    .map_with(|item, e| Spanned::new(ast::TopLevelItem { kind: item }, e.span()))
    .repeated()
    .collect()
}

/// A parser for function definitions in the language. A function definition consists of a return
/// type, a name, a list of parameters (currently empty since parameters are not yet supported) and
/// a body (a list of statements).
#[must_use]
pub fn struct_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Struct<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    // A parser for struct fields, which are of the form `<ident> : <type>`.
    let field = atoms::identifier_parser()
        .then_ignore(just(lexer::Token::Delimiter(":")))
        .then(type_parser())
        .map_with(|(name, ty), e| Spanned::new(ast::StructField { ident: name, ty }, e.span()));

    // A parser for a list of fields, which are field definitions separated by commas and
    // enclosed in curly braces. We allow a trailing comma after the last field for convenience.
    let fields = field
        .separated_by(just(lexer::Token::Delimiter(",")))
        .allow_trailing()
        .collect()
        .delimited_by(
            just(lexer::Token::Delimiter("{")),
            just(lexer::Token::Delimiter("}")),
        );

    // The main struct parser, which combines the struct declaration which are of the form
    // `struct <ident> { <fields> }`.
    just(lexer::Token::Keyword("struct"))
        .ignore_then(atoms::identifier_parser())
        .then(fields)
        .map_with(|(name, fields), e| {
            Spanned::new(
                ast::Struct {
                    ident: name,
                    fields,
                },
                e.span(),
            )
        })
        .boxed()
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
        .then(atoms::identifier_parser())
        .then_ignore(just(lexer::Token::Delimiter(":")))
        .then(type_parser())
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

    type_parser()
        .then(atoms::identifier_parser())
        .then(parameters.delimited_by(
            just(lexer::Token::Delimiter("(")),
            just(lexer::Token::Delimiter(")")),
        ))
        .map_with(|((ty, name), params), e| {
            Spanned::new(
                ast::FunctionPrototype {
                    ident: name,
                    ret: ty,
                    params,
                },
                e.span(),
            )
        })
        .then(stmt_parser().repeated().collect().delimited_by(
            just(lexer::Token::Delimiter("{")),
            just(lexer::Token::Delimiter("}")),
        ))
        .map_with(|(prototype, body), e| Spanned::new(ast::Function { prototype, body }, e.span()))
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
    recursive(|stmt| {
        // A parser for a block of statements, which is a sequence of statements enclosed
        // in curly braces `{}`. Since blocks can be nested, we use recursion to parse
        // them. Blocks can also appear in other places, like function bodies, if statements,
        // while loops...
        let block = stmt
            .repeated()
            .collect()
            .delimited_by(
                just(lexer::Token::Delimiter("{")),
                just(lexer::Token::Delimiter("}")),
            )
            .map_with(|stmts, e| {
                Spanned::new(
                    ast::Block {
                        stmts,
                        ty: lang::ty::Type::Infer,
                    },
                    e.span(),
                )
            });

        // Parse a `return` statement, which is an expression of the form `return <expr>;`
        let return_expr = just(lexer::Token::Keyword("return"))
            .ignore_then(expr_parser().or_not())
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
            .ignore_then(atoms::identifier_parser())
            .then_ignore(just(lexer::Token::Delimiter(":")))
            .then(type_parser().or_not())
            .then_ignore(just(lexer::Token::Operator("=")))
            .then(expr_parser())
            .then_ignore(just(lexer::Token::Delimiter(";")))
            .map_with(|((ident, ty), expr), e| {
                Spanned::new(
                    ast::Stmt {
                        kind: ast::StmtKind::Var(
                            ident,
                            ty.unwrap_or(Spanned::none(lang::ty::Type::Infer)),
                            Box::new(expr),
                            false,
                        ),
                    },
                    e.span(),
                )
            });

        // Parse a `var` statement, which is of the form `var <ident> [: <type>] = <expr>;`
        // This is very similar to a let statement, but the variable can be mutated after it
        // is declared, unlike a let statement which declares an immutable variable.
        let var_expr = just(lexer::Token::Keyword("var"))
            .ignore_then(atoms::identifier_parser())
            .then_ignore(just(lexer::Token::Delimiter(":")))
            .then(type_parser().or_not())
            .then_ignore(just(lexer::Token::Operator("=")))
            .then(expr_parser())
            .then_ignore(just(lexer::Token::Delimiter(";")))
            .map_with(|((ident, ty), expr), e| {
                Spanned::new(
                    ast::Stmt {
                        kind: ast::StmtKind::Var(
                            ident,
                            ty.unwrap_or(Spanned::none(lang::ty::Type::Infer)),
                            Box::new(expr),
                            true,
                        ),
                    },
                    e.span(),
                )
            });

        // Parse an assignment statement, which is of the form `<expr> [op] = <expr>;`, where
        // `op` is an optional binary operator for compound assignments like `+=`, `-=`...
        let assign_op_expr = expr_parser()
            .then(atoms::product_ops().or(atoms::sum_ops()).or_not())
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

        let if_stmt = just(lexer::Token::Keyword("if"))
            .ignore_then(expr_parser().delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .then(block.clone())
            .then(
                just(lexer::Token::Keyword("else"))
                    .ignore_then(block.clone())
                    .or_not(),
            )
            .map_with(|((condition, then_block), else_block), e| {
                Spanned::new(
                    ast::Stmt {
                        kind: ast::StmtKind::If(
                            Box::new(condition),
                            Box::new(then_block),
                            else_block.map(Box::new),
                        ),
                    },
                    e.span(),
                )
            });

        let while_stmt = just(lexer::Token::Keyword("while"))
            .ignore_then(expr_parser().delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .then(block.clone())
            .map_with(|(condition, body), e| {
                Spanned::new(
                    ast::Stmt {
                        kind: ast::StmtKind::While(Box::new(condition), Box::new(body)),
                    },
                    e.span(),
                )
            });

        // An expression statement is simply an expression followed by a semicolon. The expression
        // is evaluated, and its result is discarded. This is useful for expressions that have
        // side effects, such as function calls.
        let expr_stmt = expr_parser()
            .then_ignore(just(lexer::Token::Delimiter(";")))
            .map_with(|expr, e| {
                Spanned::new(
                    ast::Stmt {
                        kind: ast::StmtKind::Expr(Box::new(expr)),
                    },
                    e.span(),
                )
            });

        choice((
            return_expr,
            let_expr,
            var_expr,
            assign_op_expr,
            if_stmt,
            while_stmt,
            expr_stmt,
        ))
        .boxed()
    })
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
        // Parse a string literal. A string literal is a sequence of characters
        // enclosed in double quotes.
        let string = atoms::string_parser().map_with(|s, e| {
            Spanned::new(
                ast::Expr {
                    kind: ast::ExprKind::String(s),
                    ty: Type::Builtin(BuiltinType::Str),
                },
                e.span(),
            )
        });

        // An identifier is a sequence of characters that represents a name in the language.
        // Identifiers are used to name variables, functions, classes, etc.
        let identifier = atoms::identifier_parser().map_with(|ident, e| {
            Spanned::new(
                ast::Expr {
                    kind: ast::ExprKind::Identifier(ident),
                    ty: lang::ty::Type::Infer,
                },
                e.span(),
            )
        });

        // A list of expressions separated by commas. This is used for parsing
        // function call arguments as well as initializer lists.
        let items = expr
            .clone()
            .separated_by(just(lexer::Token::Delimiter(",")))
            .collect();

        // Parse an initializer list, which is a list of expressions separated by commas
        // and enclosed in braces `{}`. For example: `{1, 2, 3}`.
        let list = items
            .clone()
            .delimited_by(
                just(lexer::Token::Delimiter("{")),
                just(lexer::Token::Delimiter("}")),
            )
            .map_with(|elements, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::List(elements),
                        ty: lang::ty::Type::Infer,
                    },
                    e.span(),
                )
            });

        // A function call is an identifier followed by a list of arguments enclosed
        // in parentheses. For example: `foo(a, b + 2)`. The arguments are optional,
        // so a function call can also be just an identifier followed by empty
        // parentheses, like `foo()`.
        let call = atoms::identifier_parser()
            .then(items.clone().delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .map_with(|(function, args), e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::FunctionCall(Box::new(function), args),
                        ty: lang::ty::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // An intrinsic function call is similar to a regular function call, but it is
        // prefixed with the `@` symbol. Intrinsic functions are built-in functions
        // that are provided by the language and are not defined by the user.
        let intrinsic_call = just(lexer::Token::Operator("@"))
            .ignore_then(atoms::identifier_parser())
            .then(items.delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .map_with(|(function, args), e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::IntrinsicCall(Box::new(function), args),
                        ty: lang::ty::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // An atom is either a literal or a parenthesized expression. They have the maximum
        // precedence in the expression hierarchy, since they cannot be broken down any further,
        // called "atoms" for that reason.
        let atom = atoms::number_parser()
            .map_with(|lit, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Literal(lit),
                        ty: Type::Builtin(BuiltinType::Int),
                    },
                    e.span(),
                )
            })
            .or(list)
            .or(intrinsic_call)
            .or(call)
            .or(identifier)
            .or(string)
            .or(atoms::boolean_value_parser())
            .or(expr.delimited_by(
                just(lexer::Token::Delimiter("(")),
                just(lexer::Token::Delimiter(")")),
            ))
            .boxed();

        // Now we will parse operators based on their precedence, from highest to lowest. If two
        // operators have the same precedence, they will be parsed based on their associativity,
        // which is left-to-right for all binary operators in this language.

        // The dot operator for field access has the highest precedence. It has the form
        // `<expr>.<ident>`, where `<expr>` is an expression and `<ident>` is an identifier
        // representing the field name. In the parser, we accept any kind of expression on
        // the left side of the dot, but in the semantic analysis phase, we will ensure that
        // the left side evaluates to only a subset of expressions that can have fields, like
        // struct instances.
        let dot_operator = atom
            .clone()
            .foldl_with(
                just(lexer::Token::Delimiter("."))
                    .ignore_then(atoms::identifier_parser())
                    .repeated(),
                |base, field, e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::FieldAccess(Box::new(base), field),
                            ty: lang::ty::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        // Parse unary operators.
        let unary = atoms::unary_ops()
            .repeated()
            .foldr_with(dot_operator, |op, rhs, e| {
                Spanned::new(
                    ast::Expr {
                        kind: ast::ExprKind::Unary(op, Box::new(rhs)),
                        ty: lang::ty::Type::Infer,
                    },
                    e.span(),
                )
            })
            .boxed();

        // Parse product operators.
        let product = unary
            .clone()
            .foldl_with(
                atoms::product_ops().then(unary).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::ty::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        // Parse sum operators.
        let sum = product
            .clone()
            .foldl_with(
                atoms::sum_ops().then(product).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::ty::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        // Parse relational operators.
        let relational_ops = sum
            .clone()
            .foldl_with(
                atoms::relational_ops().then(sum).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::ty::Type::Infer,
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
                atoms::logical_and_ops().then(relational_ops).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::ty::Type::Infer,
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
                atoms::logical_or_ops().then(logical_and).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned::new(
                        ast::Expr {
                            kind: ast::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                            ty: lang::ty::Type::Infer,
                        },
                        e.span(),
                    )
                },
            )
            .boxed();

        logical_or
    })
}

/// A parser for type in the language. Currently, it supports built-in types like
/// `bool`, `int`, `str`, `void`, and array types like `<type>[<size>]`.
#[must_use]
pub fn type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<lang::ty::Type>, ParserError<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = lexer::Token<'src>, Span = Span>,
{
    // A struct type is of the form `struct <ident>`, where `<ident>` is the name of the struct.
    let strct = just(lexer::Token::Keyword("struct"))
        .ignore_then(atoms::identifier_parser())
        .map_with(|name, e| Spanned::new(lang::ty::Type::Struct(name.name.to_string()), e.span()));

    // An array type is of the form `<type>[<size>]`, where `<type>` is a built-in type
    // and `<size>` is a number literal representing the size of the array.
    let array = atoms::builtin_type_parser()
        .then_ignore(just(lexer::Token::Delimiter("[")))
        .then(
            atoms::number_parser().validate(|count, e, emitter| match count.value.parse_u64() {
                Ok(num) => num,
                Err(err) => {
                    let kind = match err.kind() {
                        std::num::IntErrorKind::Empty => "is empty".to_string(),
                        std::num::IntErrorKind::InvalidDigit => {
                            "contains invalid digits".to_string()
                        }
                        std::num::IntErrorKind::PosOverflow => "is too large".to_string(),
                        std::num::IntErrorKind::NegOverflow => "is negative".to_string(),
                        std::num::IntErrorKind::Zero => "cannot be zero".to_string(),
                        _ => "is invalid".to_string(),
                    };

                    emitter.emit(Rich::custom(
                        e.span(),
                        format!("Invalid array size: {} {}", count.value.as_str(), kind),
                    ));
                    // Return a default value to continue parsing. The AST is now poisoned with
                    // invalid data, but the program will not continue past the parsing phase since
                    // an error has already been reported. That's why returning a default value here
                    // is acceptable.
                    0
                }
            }),
        )
        .then_ignore(just(lexer::Token::Delimiter("]")))
        .map_with(|(ty, count), e| {
            Spanned::new(lang::ty::Type::Array(Box::new(ty.0), count), e.span())
        });

    choice((strct, array, atoms::builtin_type_parser())).boxed()
}
