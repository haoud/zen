use lang::{Spanned, Type};

use crate::c::Codegen;

impl Codegen {
    /// Generates the expression to be used as an argument in a formatting function. This allows
    /// for special handling of certain types, such as booleans, which need to be converted to
    /// strings before being passed to the formatting function in order to have a nice output.
    #[must_use]
    pub fn generate_fmt_args(&mut self, expr: &ast::Expr) -> String {
        match &expr.ty {
            Type::Bool => format!(
                "({}) ? \"true\" : \"false\"",
                self.generate_expr(expr, false)
            ),
            Type::Array(_, _) => match &expr.kind {
                ast::ExprKind::List(items) => {
                    let elements = items
                        .iter()
                        .map(|item| self.generate_fmt_args(&item.0))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("{}", elements)
                }
                ast::ExprKind::Identifier(_) => {
                    // FIXME: WE NEED THE SYMBOL TABLE HERE TO GET THE TYPE OF THE IDENTIFIER
                    /*let elements = (0..*size)
                        .map(|i| ident.name.to_owned() + "[" + &i.to_string() + "]")
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("{}", elements)*/
                    todo!()
                }
                _ => unreachable!(),
            },
            _ => self.generate_expr(expr, false),
        }
    }
}

/// Generates a C format string based on the provided format and arguments. Our custom format
/// string is very simple and similar to Rust's format strings, only supporting `{}` as a
/// placeholder for arguments. Each `{}` will be replaced with the appropriate C format specifier
/// based on the type of the corresponding argument.
///
/// # Panics
/// Panics if the number of `{}` in the format string does not match the number of arguments.
#[must_use]
pub fn generate_fmt_string(fmt: &str, args: &[Spanned<ast::Expr>]) -> String {
    let mut chars = fmt.chars().peekable();
    let mut result = String::new();
    let mut idx = 0;

    while let Some(c) = chars.next() {
        if c == '{' && chars.peek() == Some(&'}') {
            // Consume the closing '}' and add the appropriate format specifier
            // to the result string
            result.push_str(&get_fmt_specifier(&args[idx].ty));
            chars.next();
            idx += 1;
        } else if c == '%' {
            // Escape percent signs for C format strings
            result.push_str("%%");
        } else {
            result.push(c);
        }
    }

    result
}

/// Returns the C format specifier for a given type.
///
/// # Panics
/// Panics if the type is `Unknown`, `Infer`, or `Void`, as `Unknown` and `Infer` types should not
/// appear in the AST at this stage and `Void` type cannot be formatted and should only be used
/// for functions that do not return a value.
pub fn get_fmt_specifier(ty: &Type) -> String {
    match ty {
        Type::Bool => "%s".to_owned(), // Booleans will be printed as "true" or "false"
        Type::Int => "%d".to_owned(),
        Type::Str => "\\\"%s\\\"".to_owned(),
        Type::Array(ty, len) => {
            format!(
                "[{}]",
                (0..*len)
                    .map(|_| get_fmt_specifier(ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        Type::Unknown | Type::Infer | Type::Void => unreachable!(),
    }
}
