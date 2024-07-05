use crate::ast;

pub mod compiler;

/// The code generator for the C backend. This struct is used to generate
/// the C code from the AST. It keeps track of the current scope depth
/// and the generated code.
#[derive(Debug)]
pub struct Codegen {
    /// The depth of the current scope. This is used to indent the
    /// generated code properly. This does not include the final
    /// binary, but is useful to have an human-readable output for
    /// debugging purposes.
    depth: usize,

    /// The generated code
    code: String,
}

impl Codegen {
    #[must_use]
    pub fn new() -> Self {
        Self {
            depth: 0,
            code: String::new(),
        }
    }

    /// Add the standard includes to the generated code. This is
    /// needed because `Zen` is at its very beginning and does not
    /// have a standard library yet, so we need to include the C
    /// standard library to communicate with the host system.
    pub fn add_hosted_includes(&mut self) {
        self.code += "#include <stdio.h>\n";
        self.code += "#include <stdlib.h>\n";
    }

    /// Add the main function to the generated code. This function
    /// will call the `compute` function and print the result to
    /// the standard output. This is needed because `Zen` is at its
    /// very beginning and does not have a standard library yet.
    pub fn add_hosted_main(&mut self) {
        self.code += "int main() {\n";
        self.code += "\tprintf(\"%d\\n\", compute());\n";
        self.code += "\treturn 0;\n";
        self.code += "}\n";
    }

    /// Add a newline to the generated code. This is useful to separate
    /// different parts of the code and make it more readable.
    pub fn add_separator(&mut self) {
        self.code += "\n";
    }

    /// Indent the futur line of code by adding tabs to the generated
    /// code depending on the current scope depth.
    pub fn indent_codegen(&mut self) {
        for _ in 0..self.depth {
            self.code += "\t";
        }
    }

    /// Generate the function header. This function will generate the
    /// function return type, name and parameters. If the `prototype`
    /// flag is set to `true`, the function will add a semicolon at
    /// the end of the function declaration.
    pub fn generate_fn_header(
        &mut self,
        func: &ast::Function,
        prototype: bool,
    ) {
        // Generate the function return type and name
        self.code += "int ";
        self.code += &func.prototype.name;
        self.code += "(";

        // Generate the function parameters
        for (i, param) in func.prototype.args.iter().enumerate() {
            self.code += "int ";
            self.code += &param.name;
            if i < func.prototype.args.len() - 1 {
                self.code += ", ";
            }
        }

        // If this is a prototype, we need to add a semicolon at the
        // end of the function declaration.
        if prototype {
            self.code += ");\n"
        } else {
            self.code += ")\n";
        }
    }

    /// Generate the function body. This function will generate the
    /// function body by iterating over the function expressions and
    /// generating the corresponding C code.
    pub fn generate_fn(&mut self, func: &ast::Function) {
        self.generate_fn_header(func, false);
        self.code += "{\n";
        self.depth += 1;

        for expr in &func.body {
            let code = &self.generate_expr(expr);
            self.indent_codegen();
            self.code += code;
            self.code += "\n";
        }

        self.code += "}\n";
        self.depth -= 1;
    }

    /// Generate the C code for the given expression. This function
    /// will recursively generate the C code for the expression and
    /// its children. It will return the generated C code as a string.
    pub fn generate_expr(&mut self, expr: &ast::Expr) -> String {
        match &expr.kind {
            ast::ExprKind::Call(func, args) => {
                let mut code = String::new();
                code += &func.kind.as_identifier().unwrap();
                code += "(";
                for (i, arg) in args.iter().enumerate() {
                    code += &self.generate_expr(arg);
                    if i < args.len() - 1 {
                        code += ", ";
                    }
                }
                code += ")";
                code
            }
            ast::ExprKind::Literal(x) => format!("{}", x),
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.generate_expr(&lhs);
                let rhs = self.generate_expr(&rhs);
                format!("({} {} {})", lhs, op, rhs)
            }
            ast::ExprKind::Let(ident, expr) => {
                let ident = self.generate_expr(&ident);
                let expr = self.generate_expr(&expr);
                format!("int {} = {};", ident, expr)
            }
            ast::ExprKind::Identifier(ident) => ident.to_string(),
            ast::ExprKind::Return(expr) => {
                let expr = self.generate_expr(&expr);
                format!("return {};", expr)
            }
            ast::ExprKind::Error(..) => unreachable!(),
        }
    }
}

/// Generate the C code from the given list of functions. This function
/// will generate the C code for the given list of functions and return
/// the generated code as a string.
#[must_use]
pub fn generate<'src>(funcs: &[ast::Function]) -> String {
    let mut codegen = Codegen::new();
    codegen.add_hosted_includes();
    codegen.add_separator();

    // Generate the function prototypes at the top of the file
    // to avoid having to worry about the order of the functions
    // in the source code.
    for func in funcs {
        codegen.generate_fn_header(func, true);
    }

    // Generate all the functions
    codegen.add_separator();
    for func in funcs {
        codegen.generate_fn(func);
        codegen.add_separator();
    }

    // Add the main function to the generated code to run the
    // program and return the result.
    codegen.add_hosted_main();
    codegen.code
}
