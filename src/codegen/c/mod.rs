use crate::ast;

pub mod compiler;

/// Generate C code from the given AST. For now, this function only generates
/// code for a single function that returns the result of the given expression
/// as an integer. It the future, this function will generate code for the
/// function that corresponds to the given AST and will only include the zen
/// standard library functions that are actually used in the AST.
#[must_use]
pub fn generate<'src>(exprs: &[ast::Expr]) -> String {
    let mut code = String::new();
    code += "#include <stdio.h>\n";
    code += "#include <stdlib.h>\n";
    code += "\n";
    code += "int test() {\n";

    for expr in exprs {
        code += "\t";
        code += &generate_expr(expr);
        code += "\n";
    }

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
    match &expr.kind {
        ast::ExprKind::Literal(x) => format!("{}", x),
        ast::ExprKind::Binary(op, lhs, rhs) => {
            let lhs = generate_expr(&lhs);
            let rhs = generate_expr(&rhs);
            format!("({} {} {})", lhs, op, rhs)
        }
        ast::ExprKind::Let(ident, expr) => {
            let ident = generate_expr(&ident);
            let expr = generate_expr(&expr);
            format!("int {} = {};", ident, expr)
        }
        ast::ExprKind::Identifier(ident) => ident.to_string(),
        ast::ExprKind::Return(expr) => {
            let expr = generate_expr(&expr);
            format!("return {};\n", expr)
        }
        ast::ExprKind::Error(..) => unreachable!(),
    }
}
