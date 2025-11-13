use crate::error::SemanticDiagnostic;
use lang::ty::{StructMetadata, TypeTable};
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
    pub fn check_program(&mut self, program: &mut [ast::TopLevelItem<'src>]) {
        // First, insert all function prototypes into the global scope and insert all struct
        // definitions into the type table without checking their fields or bodies to allow for
        // forward references during the analysis.
        for item in program.iter_mut() {
            match &mut item.kind {
                ast::TopLevelItemKind::Function(func) => {
                    self.scopes.insert_function(&mut self.errors, func);
                }
                ast::TopLevelItemKind::Struct(structure) => {
                    self.insert_struct(structure);
                }
            }
        }

        // Now, check all struct definitions to ensure their fields are semantically correct
        for item in program.iter_mut() {
            if let ast::TopLevelItemKind::Struct(structure) = &mut item.kind {
                self.check_struct(structure);
            }
        }

        // Now, check each function for semantic correctness.
        for items in program.iter_mut() {
            if let ast::TopLevelItemKind::Function(function) = &mut items.kind {
                self.check_function(function);
            }
        }
    }

    /// Insert a struct definition into the type table, checking for redeclarations. This function
    /// only checks for redeclarations of the struct itself and its fields, but does not check the
    /// field types for correctness at this stage. More detailed checks are performed later in
    /// `check_struct`.
    pub fn insert_struct(&mut self, structure: &ast::Struct<'src>) {
        // Check for struct redeclaration.
        if let Some(first) = self.types.get_struct_metadata(&structure.ident.name) {
            self.errors.emit_struct_redefinition_error(
                structure.ident.name,
                structure.span,
                first.span,
            );
        } else {
            // Collect field types and check for redeclarations
            let mut fields_order = Vec::new();
            let mut fields = HashMap::new();

            for field in &structure.fields {
                if fields.contains_key(field.ident.name) {
                    let first = &structure
                        .fields
                        .iter()
                        .find(|f| f.ident.name == field.ident.name)
                        .unwrap();
                    self.errors.emit_struct_field_redeclaration_error(
                        field.ident.name,
                        field.span,
                        first.span,
                    );
                } else {
                    fields.insert(field.ident.name.to_owned(), field.ty.specifier.clone());
                    fields_order.push(field.ident.name.to_owned());
                }
            }

            // Insert the struct metadata into the type table
            self.types.insert_struct(
                structure.ident.name.to_string(),
                StructMetadata {
                    fields_order,
                    fields,
                    span: structure.span,
                },
            );
        }
    }

    /// Check a function for semantic correctness.
    pub fn check_function(&mut self, function: &mut ast::Function<'src>) {
        // Verify that array types are not used as function parameter types.
        if function.prototype.ret.specifier.is_array() {
            self.errors.emit_array_type_as_function_return_type_error(
                function.prototype.ret.span,
                &function.prototype,
            );
        }

        // Insert function parameters into the current scope.
        self.scopes.enter_scope();
        for param in &function.prototype.params {
            // Verify that function parameters are not declared with the void type.
            if param.ty.specifier.is_void() {
                self.errors
                    .emit_void_function_parameter_error(param.span, &function.prototype);
            }

            // Insert the parameter into the current scope.
            self.scopes.insert_variable(
                &mut self.errors,
                lang::sym::Variable {
                    mutable: param.mutable,
                    name: param.ident.name,
                    ty: param.ty.specifier.clone(),
                    span: param.span,
                },
            );
        }
        self.check_statements(function.prototype.clone(), &mut function.body);
        self.scopes.exit_scope();

        // Simple control flow analysis to ensure all code paths return a value and to
        // identify unreachable code.
        let mut cfa = flow::ControlFlowAnalysis::new(&mut self.errors);
        cfa.check_block(&function.body, function);
    }

    /// Check a struct definition for semantic correctness. This includes verifying that
    /// field types are valid and that there are no recursive struct definitions that would
    /// lead to infinite size types.
    pub fn check_struct(&mut self, structure: &mut ast::Struct<'src>) {
        for field in &mut structure.fields {
            // Void type is not allowed for struct fields.
            if field.ty.specifier.is_void() {
                self.errors
                    .emit_void_field_declaration_error(field.ty.span, field);
                field.ty.specifier = lang::ty::TypeSpecifier::Unknown;
            }

            // Special checks for fields that are structs.
            if let lang::ty::TypeSpecifier::Struct(name) = &field.ty.specifier {
                // Check in the type table if the struct type exists.
                if !self.types.struct_exists(name) {
                    self.errors
                        .emit_unknown_type_error(field.ty.span, &field.ty.specifier);
                    field.ty.specifier = lang::ty::TypeSpecifier::Unknown;
                }

                // Recursive struct definitions are not allowed to prevent infinite size types.
                // We check if the field type is the same as the struct being defined, but we
                // also need to consider indirect recursion involving other structs.
                if self.check_indirect_recursion(&field.ty.specifier, &structure.ident.name) {
                    self.errors.emit_infinite_struct_size_error(
                        &structure.ident.name,
                        structure.ident.span,
                        field.ty.span,
                    );
                    field.ty.specifier = lang::ty::TypeSpecifier::Unknown;
                }
            }
        }
    }

    /// Check for indirect recursion in struct definitions. This function will recursively
    /// traverse struct field types to see if any of them eventually reference the original
    /// struct being defined.
    fn check_indirect_recursion(&self, ty: &lang::ty::TypeSpecifier, struct_name: &str) -> bool {
        match ty {
            lang::ty::TypeSpecifier::Struct(name) => {
                if name == struct_name {
                    return true;
                }
                if let Some(structure) = self.types.get_struct_metadata(name) {
                    for (_, field_type) in &structure.fields {
                        if self.check_indirect_recursion(field_type, struct_name) {
                            return true;
                        }
                    }
                }
                false
            }
            lang::ty::TypeSpecifier::Array(ty, _) => self.check_indirect_recursion(ty, struct_name),
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
