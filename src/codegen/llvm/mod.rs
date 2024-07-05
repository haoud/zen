use crate::{ast, lang};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, io::Write, path::Path};

/// A scope represents a stack of variable scopes in which we can
/// store and retrieve variables. Each scope contains a map of
/// variable names to their corresponding LLVM pointers as well as
/// the current function in which we are working.
#[derive(Debug)]
pub struct Scope<'ctx> {
    /// A stack of variable scopes. Each scope is a map of variable
    /// names to their corresponding LLVM pointers. The scope are
    /// implemented as a stack to allow for nested scopes.
    variables: Vec<HashMap<String, PointerValue<'ctx>>>,

    /// The current function in which we are working. Zen does not
    /// support nested functions, so we only need to keep track of
    /// the current function.
    function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            function: None,
        }
    }

    /// Get a variable accessible in the current scope. If no variable
    /// with the given name is found, return `None`.
    #[must_use]
    pub fn get_variable(&self, name: &str) -> Option<PointerValue<'ctx>> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(*var);
            }
        }
        None
    }

    /// Inserts a variable in the current scope. If the variable already
    /// exists, it will be overwritten.
    pub fn insert_variable(&mut self, name: &str, var: PointerValue<'ctx>) {
        self.variables
            .last_mut()
            .unwrap()
            .insert(name.to_string(), var);
    }

    /// Creates a new scope with an empty variable stack
    pub fn start_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    /// Ends the current scope by popping the last variable scope
    /// from the stack.
    pub fn end_scope(&mut self) {
        self.variables.pop();
    }
}

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
    pub scope: Scope<'ctx>,
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
            scope: Scope::new(),
            context,
            builder,
            module,
        }
    }

    /// Returns the function with the given name if it exists in the module.
    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    pub fn generate_prototype(
        &mut self,
        func: &'a ast::Function,
    ) -> Result<FunctionValue, String> {
        let args_types = func
            .prototype
            .args
            .iter()
            .map(|_| self.context.i32_type().into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let header = self.context.i32_type().fn_type(&args_types, false);
        Ok(self.module.add_function(&func.prototype.name, header, None))
    }

    /// Generates a function that computes the given expression and returns
    /// the result. The function is named `dummy` and takes no arguments.
    /// This is a temporary function to test the LLVM code generation until
    /// we have a proper language with functions and expressions.
    pub fn generate_fn(
        &mut self,
        func: &'a ast::Function,
    ) -> Result<FunctionValue, String> {
        self.scope.start_scope();
        let prototype = self
            .get_function(&func.prototype.name)
            .ok_or("Function not found")?;

        let entry = self.context.append_basic_block(prototype, "entry");
        self.builder.position_at_end(entry);

        // For each argument in the function, create an alloca instruction
        // at the beginning of the function and store the argument in it.
        for (arg, param) in
            func.prototype.args.iter().zip(prototype.get_param_iter())
        {
            let alloca = self
                .builder
                .build_alloca(self.context.i32_type(), &arg.name)
                .expect("Failed to create alloca");

            self.builder.build_store(alloca, param).unwrap();
            self.scope.insert_variable(&arg.name, alloca);
        }

        // Build all expressions in the function.dummy
        for expr in &func.body {
            self.generate_expr(expr)?;
        }

        self.scope.end_scope();
        Ok(prototype)
    }

    /// Generates the LLVM IR for the given expression. This function assumes
    /// that a semantic analysis has been performed and that the expression is
    /// valid. The function returns the resulting LLVM value of the expression.
    fn generate_expr(
        &mut self,
        expr: &'a ast::Expr<'a>,
    ) -> Result<IntValue<'ctx>, String> {
        match &expr.kind {
            ast::ExprKind::Call(func, args) => {
                // Get the function from the module that we want to call
                let function = self
                    .get_function(func.kind.as_identifier().unwrap())
                    .ok_or("Function not found")?;

                // Evaluate all arguments (from left to right) that we
                // want to pass to the function.
                let args = args
                    .iter()
                    .map(|arg| self.generate_expr(arg).map(|arg| arg.into()))
                    .collect::<Result<Vec<BasicMetadataValueEnum>, String>>()?;

                // Build the call instruction
                let call = self
                    .builder
                    .build_call(function, &args, "tmpcall")
                    .unwrap();

                // Get the return value of the function call. For now, we
                // assume that the function returns an integer value.
                let ret = call
                    .try_as_basic_value()
                    .left()
                    .expect("Failed to get function return value")
                    .into_int_value();

                Ok(ret)
            }
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
                // Get the variable from the current scope
                let var = self
                    .scope
                    .get_variable(ident)
                    .ok_or("Variable not found")?;

                // Load the value from the variable
                Ok(self
                    .builder
                    .build_load(self.context.i32_type(), var, &ident)
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
                self.scope.insert_variable(&ident, alloca);
                Ok(value)
            }
            ast::ExprKind::Return(expr) => {
                let value = self.generate_expr(expr)?;
                self.builder.build_return(Some(&value)).unwrap();
                Ok(value)
            }
            ast::ExprKind::Error(..) => unreachable!(),
        }
    }
}

/// Build the given AST expression and write the generated LLVM IR to the
/// given output file. The function returns the generated LLVM IR as a string
/// for debugging purposes.
pub fn build<'src>(funcs: &[ast::Function], output: &'src str) -> String {
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

    let mut compiler = Compiler::new(&context, &builder, &module);

    // Generate prototypes for all functions
    for func in funcs {
        let _ = compiler.generate_prototype(&func).unwrap();
    }

    // Compile functions
    for func in funcs {
        let _ = compiler.generate_fn(&func).unwrap();
    }

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

        extern int compute();

        int main() {
            printf(\"%d\\n\", compute());
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
