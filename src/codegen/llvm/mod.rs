use crate::{ast, lang};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    values::{FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, io::Write, path::Path};

/// Represents the LLVM code generator for the language. It contains
/// the LLVM context, builder, and module in which we are working.
#[derive(Debug)]
pub struct Compiler<'a, 'ctx> {
    /// The LLVM context in which we are working
    pub context: &'ctx Context,

    /// The LLVM builder in which we are working
    pub builder: &'a Builder<'ctx>,

    /// The LLVM module in which we are working
    pub module: &'a Module<'ctx>,

    /// The variable in the current scope
    pub variables: HashMap<&'a str, PointerValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Creates a new LLVM code generator with the given context,
    /// builder, and module.
    #[must_use]
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            variables: HashMap::new(),
            context,
            builder,
            module,
        }
    }

    /// Generates a function that computes the given expression and returns
    /// the result. The function is named `dummy` and takes no arguments.
    /// This is a temporary function to test the LLVM code generation until
    /// we have a proper language with functions and expressions.
    pub fn generate_fn(
        &mut self,
        exprs: &'a [ast::Expr<'a>],
    ) -> Result<FunctionValue, String> {
        let prototype = self.dummy_prototype()?;

        let entry = self.context.append_basic_block(prototype, "entry");
        self.builder.position_at_end(entry);

        // Build all expressions in the function.
        for expr in exprs {
            self.generate_expr(expr)?;
        }

        Ok(prototype)
    }

    /// Generates a dummy prototype function that takes no arguments and
    /// returns an integer. This is a temporary function to test the LLVM
    /// code generation until we have a proper language with functions and
    /// expressions.
    fn dummy_prototype(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let prototype = self.context.i32_type().fn_type(&[], false);
        Ok(self.module.add_function("dummy", prototype, None))
    }

    /// Generates the LLVM IR for the given expression
    fn generate_expr(
        &mut self,
        expr: &'a ast::Expr<'a>,
    ) -> Result<IntValue<'ctx>, String> {
        match &expr.kind {
            ast::ExprKind::Literal(n) => {
                Ok(self.context.i32_type().const_int(*n as u64, false))
            }
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.generate_expr(lhs)?;
                let rhs = self.generate_expr(rhs)?;
                match op {
                    // Addition
                    lang::operator::BinaryOp::Add => Ok(self
                        .builder
                        .build_int_add(lhs, rhs, "tmpadd")
                        .unwrap()),

                    // Subtraction
                    lang::operator::BinaryOp::Sub => Ok(self
                        .builder
                        .build_int_sub(lhs, rhs, "tmpsub")
                        .unwrap()),

                    // Multiplication
                    lang::operator::BinaryOp::Mul => Ok(self
                        .builder
                        .build_int_mul(lhs, rhs, "tmpmul")
                        .unwrap()),

                    // Division
                    lang::operator::BinaryOp::Div => Ok(self
                        .builder
                        .build_int_unsigned_div(lhs, rhs, "tmpdiv")
                        .unwrap()),

                    // Remainder (Modulo)
                    lang::operator::BinaryOp::Mod => Ok(self
                        .builder
                        .build_int_signed_rem(lhs, rhs, "tmpmod")
                        .unwrap()),
                }
            }
            ast::ExprKind::Identifier(ident) => {
                let var = self.variables.get(ident).unwrap();
                Ok(self
                    .builder
                    .build_load(self.context.i32_type(), *var, &ident)
                    .expect("Failed to load variable")
                    .into_int_value())
            }
            ast::ExprKind::Let(ident, expr) => {
                let ident = ident.kind.as_identifier().unwrap();
                let value = self.generate_expr(expr)?;

                // Create an alloca instruction for the variable
                let alloca = self
                    .builder
                    .build_alloca(self.context.i32_type(), &ident)
                    .expect("Failed to create alloca");

                // Store the value in the alloca instruction
                self.builder
                    .build_store(alloca, value)
                    .expect("Failed to store value");

                // Add the variable to the current scope
                self.variables.insert(ident, alloca);
                Ok(value)
            }
            ast::ExprKind::Return(expr) => {
                let value = self.generate_expr(expr)?;
                let _ = self.builder.build_return(Some(&value));
                Ok(value)
            }
            ast::ExprKind::Error(..) => unreachable!(),
        }
    }
}

/// Build the given AST expression and write the generated LLVM IR to the
/// given output file. The function returns the generated LLVM IR as a string
/// for debugging purposes.
pub fn build<'src>(exprs: &[ast::Expr], output: &'src str) -> String {
    Target::initialize_x86(&InitializationConfig::default());

    // Get the current target triple and create a target machine
    // with it to generate code for the current machine.
    let target_triple = TargetMachine::get_default_triple();
    let target_target = Target::from_triple(&target_triple).unwrap();
    let target_machine = Target::create_target_machine(
        &target_target,
        &target_triple,
        "generic",
        "",
        Default::default(),
        Default::default(),
        Default::default(),
    )
    .expect("Failed to create LLVM target machine");

    let target_data = target_machine.get_target_data();

    let context = inkwell::context::Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    // Configure the module with the target triple and data layout
    module.set_data_layout(&target_data.get_data_layout());
    module.set_triple(&target_triple);

    // Compile the expression
    let mut compiler = Compiler::new(&context, &builder, &module);
    let _ = compiler.generate_fn(exprs).unwrap();

    // Create an temporary object file name from the output file
    // name to write the object file to disk.
    let object_path = Path::new(output)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
        + ".o";

    // Write the object file to disk
    target_machine
        .write_to_file(
            &module,
            inkwell::targets::FileType::Object,
            Path::new(&object_path),
        )
        .expect("Failed to write object file");

    // Temporary glue code to compile the object file to an
    // executable because our compiler can for now only
    // generate one function called `dummy` that computes a
    // single expression and returns it.
    let main = "
        #include <stdio.h>

        extern int dummy();

        int main() {
            printf(\"%d\\n\", dummy());
            return 0;
        }
    ";

    // Compile the object file to an executable with clang
    let mut clang = std::process::Command::new("clang")
        .arg(object_path.clone())
        .arg("-x")
        .arg("c")
        .arg("-o")
        .arg(output)
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    clang
        .stdin
        .as_mut()
        .expect("Failed to open stdin")
        .write_all(main.as_bytes())
        .expect("Failed to write to clang");

    // If the compilation was successful, remove the object file, otherwise
    // panic with an error message.
    match clang.wait() {
        Ok(_) => _ = std::fs::remove_file(object_path),
        Err(e) => panic!("Failed to compile object file: {}", e),
    }

    // Return the generated LLVM IR as a string
    module.print_to_string().to_string()
}
