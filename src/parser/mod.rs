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
                    _phantom: std::marker::PhantomData,
                },
                e.span(),
            )
        });

    return_expr
}

/// A parser for expressions in the language. Currently, the only expression that is supported
/// is a number literal, but more expressions can be added in the future, like binary expressions,
/// unary expressions, function calls...
pub fn expr_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Expr<'src>>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
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

    // Map the parsed literal to an expression.
    value.map_with(|lit, e| {
        Spanned::new(
            ast::Expr {
                kind: ast::ExprKind::Literal(lit),
                ty: lang::Type::Int,
                _phantom: std::marker::PhantomData,
            },
            e.span(),
        )
    })
}

/// A parser for identifiers in the language. An identifier is a sequence of
/// characters that represents a name in the language. Identifiers are used to
/// name variables, functions, classes, etc.
pub fn ident_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ast::Identifier<'src>>, ParserError<'tokens, 'src>>
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
pub fn builtin_type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<lang::Type>, ParserError<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Identifier("int") = e => Spanned::new(lang::Type::Int, e.span()),
    }
}
