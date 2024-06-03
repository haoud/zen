use crate::ast;

pub mod compiler;

/// Generate C code from the given AST. For now, this function only generates
/// code for a single function that returns the result of the given expression
/// as an integer. It the future, this function will generate code for the
/// function that corresponds to the given AST and will only include the zen
/// standard library functions that are actually used in the AST.
#[must_use]
pub fn generate<'src>(ast: &ast::Expr) -> String {
    let mut code = String::new();
    code += "#include <stdio.h>\n";
    code += "#include <stdlib.h>\n";
    code += "\n";
    code += "int test() {\n";
    code += &generate_expr(ast);
    code += "}\n";
    code += "\n";
    code += "int main() {\n";
    code += "\tprintf(\"%d\\n\", test());\n";
    code += "\treturn 0;\n";
    code += "}\n";
    code
}

/// Parse the given expression and generate the corresponding C code.
#[must_use]
fn generate_expr(expr: &ast::Expr) -> String {
    match expr.kind {
        ast::ExprKind::Literal(x) => format!("return {};\n", x),
        ast::ExprKind::Binary(..) => {
            format!("\treturn {};\n", generate_inline_expr(expr))
        }
        ast::ExprKind::Error(..) => unreachable!(),
    }
}

/// Generate an inline expression from the given AST expression and
/// return it as a string of C code.
#[must_use]
fn generate_inline_expr(expr: &ast::Expr) -> String {
    match &expr.kind {
        ast::ExprKind::Literal(x) => format!("{}", x),
        ast::ExprKind::Binary(op, lhs, rhs) => {
            let lhs = generate_inline_expr(lhs);
            let rhs = generate_inline_expr(rhs);
            format!("({} {} {})", lhs, op, rhs)
        }
        ast::ExprKind::Error(..) => unreachable!(),
    }
}
