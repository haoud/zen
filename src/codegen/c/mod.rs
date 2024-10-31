//! The C code generator for the Zen programming language. This module
//! is responsible for generating the C code from the AST. The generated
//! code is then compiled by the C compiler to produce the final binary.
//!
//! The C backend produce human-readable C code that can be easily debugged
//! or inspected, and is a feature of the C backend. This is useful for
//! debugging purposes, or for understanding how the code generator works.
//!
//! # Pros and Cons
//!
//! ## Pros
//!  - Way easier to implement: The C backend is easier to implement than
//!    a direct code generation to the target architecture or by using an
//!    intermediate representation (IR) like LLVM IR. This is because C is
//!    syntactically similar to Zen, which makes the code generation process
//!    simpler and more straightforward.
//!
//!  - Easier to debug: The generated C code is human-readable and can be
//!    easily debugged using standard C debugging tools, or even by inspecting
//!    the generated code directly. This is useful for debugging the code
//!    generator itself, or for debugging the generated code.
//!
//! ## Cons
//!  - Slower build times: The generated C code must be compiled by the C
//!    compiler to produce the final binary. This adds an extra step to the
//!    build process and increases the build times compared to a direct
//!    code generation to the target architecture.
//!
//!  - Less control: The generated C code is not as optimized as it could
//!    be if we were to generate the code directly to the target architecture.
//!
//!  - Less flexibility: The generated C code is limited by the C language
//!    and its features. This means that some features of the Zen language
//!    may not be possible to implement in C, or may require more complex
//!    code generation. For example, we cannot easily specify a behavior when
//!    an unsigned integer overflows in C, since the behavior is undefined in C,
//!    while it is well-defined in Zen (the value wraps around, used by almost
//!    all computer architectures).
use crate::{ast, lang};

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
    /// Create a new empty code generator
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
        self.code += "#include <stdint.h>\n";
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
        self.code += &func.prototype.ident.name;
        self.code += "(";

        // Generate the function name
        for (i, param) in func.prototype.args.iter().enumerate() {
            self.code += "int ";
            self.code += &param.ident.name;
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

        for stmt in &func.body {
            let code = &self.generate_stmt(stmt);
            self.indent_codegen();
            self.code += code;
            self.code += "\n";
        }

        self.code += "}\n";
        self.depth -= 1;
    }

    /// Generate the C code for the given statement. This function
    /// will recursively generate the C code for the statement and
    /// its children. It will return the generated C code as a string.
    #[must_use]
    pub fn generate_stmt(&mut self, stmt: &ast::Stmt) -> String {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => self.generate_expr(expr),
            ast::StmtKind::Let(variable, expr) => {
                let expr = self.generate_expr(expr);
                let ty = self.generate_type(&variable.ty);
                format!("{} {} = {};", ty, variable.ident.name, expr)
            }
            ast::StmtKind::Return(expr) => {
                let expr = self.generate_expr(expr);
                format!("return {};", expr)
            }
            ast::StmtKind::Error(..) => unreachable!(),
        }
    }

    /// Generate the C code for the given expression. This function
    /// will recursively generate the C code for the expression and
    /// its children. It will return the generated C code as a string.
    #[must_use]
    pub fn generate_expr(&mut self, expr: &ast::Expr) -> String {
        match &expr.kind {
            ast::ExprKind::Identifier(ident) => ident.name.to_string(),
            ast::ExprKind::Call(func, args) => {
                let mut code = String::new();

                code += func.kind.as_identifier().unwrap().name;
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
            ast::ExprKind::Literal(literal) => format!("{}", literal.value),
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.generate_expr(&lhs);
                let rhs = self.generate_expr(&rhs);
                format!("({} {} {})", lhs, op, rhs)
            }
            ast::ExprKind::Error(..) => unreachable!(),
        }
    }

    /// Convert the built-in Zen types to C types.
    ///
    /// # Panics
    /// This function will panic if the type is `Never` or `Infer` is passed
    /// to the function:
    ///  - The type checker should have remplaced all Infer types by concrete
    ///    types before the code generation phase.
    ///  - The Never type has no equivalent in C and should not be used in the
    ///    generated code, since it represents an value that can never be
    ///    constructed.
    pub fn generate_type(&mut self, ty: &lang::Type) -> String {
        match ty {
            lang::Type::I8 => "int8_t".to_string(),
            lang::Type::I16 => "int16_t".to_string(),
            lang::Type::I32 => "int32_t".to_string(),
            lang::Type::I64 => "int64_t".to_string(),
            lang::Type::U8 => "uint8_t".to_string(),
            lang::Type::U16 => "uint16_t".to_string(),
            lang::Type::U32 => "uint32_t".to_string(),
            lang::Type::U64 => "uint64_t".to_string(),
            lang::Type::Int => "int".to_string(),
            lang::Type::Uint => "unsigned int".to_string(),
            lang::Type::Bool => "bool".to_string(),
            lang::Type::Char => "char".to_string(),
            lang::Type::Unit => "void".to_string(),
            lang::Type::Never => unreachable!(),
            lang::Type::Infer => unreachable!(),
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
