use lang::ty::{BuiltinType, Type};
use span::Spanned;

use crate::c::Codegen;

impl Codegen<'_> {
    /// Generates the expression to be used as an argument in a formatting function. This allows
    /// for special handling of certain types, such as booleans, which need to be converted to
    /// strings before being passed to the formatting function in order to have a nice output.
    #[must_use]
    pub fn generate_fmt_args(&mut self, expr: &ast::Expr) -> String {
        match &expr.ty {
            // Special handling for array types to generate each element's formatting argument
            // individually. This allow us to support directly passing array literals or identifiers
            // to formatting functions.
            Type::Array(ty, size) => match &expr.kind {
                ast::ExprKind::List(items) => {
                    let elements = items
                        .iter()
                        .map(|item| self.generate_fmt_args(&item.inner()))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("{}", elements)
                }
                ast::ExprKind::Identifier(ident) => match **ty {
                    Type::Builtin(BuiltinType::Bool) => {
                        let elements = (0..*size)
                            .map(|i| {
                                Self::emit_bool_to_str(&format!("{}[{}]", ident.inner().name, i))
                            })
                            .collect::<Vec<String>>()
                            .join(", ");
                        return format!("{}", elements);
                    }
                    _ => {
                        let elements = (0..*size)
                            .map(|i| format!("{}[{}]", ident.inner().name, i))
                            .collect::<Vec<String>>()
                            .join(", ");
                        return format!("{}", elements);
                    }
                },
                _ => {
                    // Array types should only be formatted from array literals or identifiers.
                    unreachable!()
                }
            },
            // Special handling for boolean types to convert them to "true" or "false" strings
            // before passing them to the formatting function.
            Type::Builtin(BuiltinType::Bool) => {
                Self::emit_bool_to_str(&self.generate_expr(expr, false))
            }
            // For all other types, just generate the expression normally since they are natively
            // supported by C's printf function.
            _ => self.generate_expr(expr, false),
        }
    }

    /// Generates a C expression that converts a boolean expression to a string representation.
    #[must_use]
    pub fn emit_bool_to_str(expr: &str) -> String {
        format!("({}) ? \"true\" : \"false\"", expr)
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
            result.push_str(&get_fmt_specifier(&args[idx].inner().ty));
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

/// Returns the C format specifier for a given type. For some types, such as arrays, it will instead
/// return a composite format specifier that represents the entire structure, since those types are
/// not natively supported by C's printf function.
///
/// # Panics
/// Panics if the type is `Unknown`, `Infer`, or `Void`, as `Unknown` and `Infer` types should not
/// appear in the AST at this stage and `Void` type cannot be formatted and should only be used
/// for functions that do not return a value.
pub fn get_fmt_specifier(ty: &Type) -> String {
    match ty {
        Type::Array(ty, len) => {
            format!(
                "[{}]",
                (0..*len)
                    .map(|_| get_fmt_specifier(ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        Type::Struct(name) => {
            // TODO: Display struct fields properly and not just the struct name
            format!("struct {} {{ <todo> }}", name)
        }
        Type::Builtin(ty) => match ty {
            BuiltinType::Bool => "%s".to_owned(), // Booleans will be printed as "true" or "false"
            BuiltinType::Int => "%d".to_owned(),
            BuiltinType::Str => "\\\"%s\\\"".to_owned(),
            BuiltinType::Void => unreachable!(),
        },
        Type::Infer | Type::Unknown => unreachable!(),
    }
}
