use crate::error::SemanticDiagnostic;
use lang::{self, Spanned, ty::TypeTable};
use std::collections::HashMap;

pub mod error;
pub mod expr;
pub mod flow;
pub mod scope;
pub mod stmt;

pub type SemanticNote<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticWarning<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticError<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;

/// Represents the semantic analysis context, including scopes and symbol tables.
#[derive(Debug)]
pub struct SemanticAnalysis<'src> {
    /// The scope stack for managing variable and function declarations.
    scopes: scope::Scope<'src>,

    /// A collection of semantic errors encountered during analysis.
    errors: SemanticDiagnostic<'src>,

    /// A table of types metadata used during semantic analysis.
    types: TypeTable,
}

impl<'src> SemanticAnalysis<'src> {
    /// Create a new semantic analysis context with an empty global scope.
    #[must_use]
    pub fn new(filename: &'src str) -> Self {
        Self {
            errors: SemanticDiagnostic::new(filename),
            scopes: scope::Scope::new(),
            types: TypeTable::new(),
        }
    }

    /// Perform semantic analysis on the given program
    pub fn check_program(&mut self, program: &mut [Spanned<ast::TopLevelItem<'src>>]) {
        // First, insert all function prototypes and struct definitions into the global scope
        // to allow for forward references during the analysis.
        for item in program.iter_mut() {
            match &mut item.0.kind {
                ast::TopLevelItemKind::Function(func) => {
                    self.scopes.insert_function(&mut self.errors, func);
                }
                ast::TopLevelItemKind::Struct(strct) => {
                    self.scopes.insert_struct(&mut self.errors, strct);
                }
            }
        }

        // Now, check all struct definitions to ensure their fields are semantically correct
        // and add their types to the type table for later use.
        for item in program.iter_mut() {
            if let ast::TopLevelItemKind::Struct(structure) = &mut item.0.kind {
                self.check_struct(structure);
            }
        }

        // Now, check each function for semantic correctness.
        for items in program.iter_mut() {
            if let ast::TopLevelItemKind::Function(function) = &mut items.0.kind {
                self.check_function(function);
            }
        }
    }

    /// Check a function for semantic correctness.
    pub fn check_function(&mut self, function: &mut Spanned<ast::Function<'src>>) {
        // Verify that array types are not used as function parameter types.
        if let lang::ty::Type::Array(_, _) = function.prototype.ret.0 {
            self.errors.emit_array_type_as_function_return_type_error(
                function.prototype.ret.span(),
                &function.prototype,
            );
        }

        // Insert function parameters into the current scope.
        self.scopes.enter_scope();
        for param in &function.prototype.params {
            // Verify that function parameters are not declared with the void type.
            if param.ty.0 == lang::ty::Type::Void {
                self.errors
                    .emit_void_function_parameter_error(param.span(), &function.prototype);
            }

            // Insert the parameter into the current scope.
            self.scopes.insert_variable(
                &mut self.errors,
                Spanned::new(
                    lang::sym::Variable {
                        mutable: param.mutable,
                        name: param.ident.name,
                        ty: param.ty.0.clone(),
                    },
                    param.span(),
                ),
            );
        }
        self.check_statements(function.prototype.clone(), &mut function.body);
        self.scopes.exit_scope();

        // Simple control flow analysis to ensure all code paths return a value and to
        // identify unreachable code.
        let mut cfa = flow::ControlFlowAnalysis::new(&mut self.errors);
        cfa.check_block(&function.0.body, function);
    }

    /// Check a struct definition for semantic correctness.
    pub fn check_struct(&mut self, structure: &mut Spanned<ast::Struct<'src>>) {
        let struct_ty = lang::ty::Type::Struct(structure.ident.name.to_owned());
        let struct_span = structure.ident.span();
        let struct_name = structure.ident.name;
        let mut metadata = lang::ty::TypeMetadata::build(&struct_ty);

        // Check for redeclaration of the struct type. If it already exists, we simply
        // skip adding it again, but we do not emit an error here since it was already
        // handled during the scope insertion phase.
        if self.types.get_type_metadata(&struct_ty).is_some() {
            return;
        }

        // Verify that there are no duplicate field names.
        let mut fieldmap = HashMap::new();
        for field in &structure.fields {
            if let Some(previous) = fieldmap.get(field.ident.name) {
                self.errors.emit_struct_field_redeclaration_error(
                    field.ident.name,
                    field.span(),
                    *previous,
                );
            } else {
                fieldmap.insert(field.ident.name, field.span());
            }
        }

        for field in &mut structure.fields {
            // Void type is not allowed for struct fields.
            if field.ty.0 == lang::ty::Type::Void {
                self.errors
                    .emit_void_field_declaration_error(field.ty.span(), field);
            }

            // Special checks for struct types used as field types.
            if let lang::ty::Type::Struct(name) = &field.ty.0 {
                // If we reach here, the type may still be defined later in the program, so
                // we check the symbol table to see if it's a known struct type. If not, we
                // can now emit an unknown type error and we set the field type to Unknown
                // to ensure that further analysis can continue and will not try to use this
                // invalid type.
                if self.scopes.get_struct(name).is_none() {
                    self.errors
                        .emit_unknown_type_error(field.ty.span(), &field.ty.0);
                    field.ty.0 = lang::ty::Type::Unknown;
                }

                // Recursive struct definitions are not allowed to prevent infinite size types.
                // We check if the field type is the same as the struct being defined, but we
                // also need to consider indirect recursion involving other structs.
                if self.check_indirect_recursion(&field.ty.0, &struct_name) {
                    self.errors.emit_infinite_struct_size_error(
                        &struct_name,
                        struct_span,
                        field.ty.span(),
                    );
                    field.ty.0 = lang::ty::Type::Unknown;
                }
            }

            metadata
                .fields
                .insert(field.ident.name.to_owned(), field.ty.0.clone());
        }
        self.types.add_type(struct_ty, metadata);
    }

    /// Check for indirect recursion in struct definitions. This function will recursively
    /// traverse struct field types to see if any of them eventually reference the original
    /// struct being defined.
    fn check_indirect_recursion(&self, ty: &lang::ty::Type, struct_name: &str) -> bool {
        match ty {
            lang::ty::Type::Struct(name) => {
                if name == struct_name {
                    return true;
                }
                if let Some(structure) = self.scopes.get_struct(name) {
                    for field in &structure.fields {
                        if self.check_indirect_recursion(&field.ty, struct_name) {
                            return true;
                        }
                    }
                }
                false
            }
            lang::ty::Type::Array(ty, _) => self.check_indirect_recursion(ty, struct_name),
            _ => false,
        }
    }

    /// Finalize the semantic analysis, returning any collected errors.
    pub fn finalize(self) -> Result<TypeTable, Vec<SemanticError<'src>>> {
        let errors = self.errors.collect();
        if errors.is_empty() {
            Ok(self.types)
        } else {
            Err(errors)
        }
    }
}
