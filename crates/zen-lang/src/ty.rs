use std::{collections::HashMap, hash::Hash};

use crate::{BinaryOp, Span, UnaryOp};

/// The types supported by the language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// An array type. The first element is the type of the elements in the array, and the second
    /// element is the size of the array. All elements in the array must have the same type.
    Array(Box<Type>, u64),

    /// A struct type. Currently, structs are identified by their name only, and field types are
    /// stored in the `TypeMetadata` associated with the struct type.
    Struct(String),

    /// Built-in types, like `int`, `bool`...
    Builtin(BuiltinType),

    /// A special type that indicates the type is to be inferred during type checking. This is
    /// primarily used for integer literals that do not have an explicit type annotation, or for
    /// expressions where the type can be determined from context (e.g., the result of a binary
    /// operation where both operands have the same type). When type inference is complete, all
    /// instances of this type should be replaced with a concrete type, and any remaining instances
    /// of this type indicate a failure to infer the type and should be treated as a type error.
    Infer,

    /// A special type that indicates the type is unknown. This is primarily used for error recovery
    /// during type checking. When a type error is encountered, the type of the expression
    /// causing the error can be set to `Unknown` to allow type checking to continue for
    /// subsequent expressions. This helps to prevent cascading errors and provides more
    /// comprehensive error reporting. This type should not appear in a valid AST after type
    /// checking is complete.
    Unknown,
}

impl Type {
    /// Check if the type is valid, meaning it is not `Unknown` or `Infer`.
    #[must_use]
    pub fn is_valid(&self) -> bool {
        !matches!(self, Type::Unknown | Type::Infer)
    }

    /// Check if the type is the `void` type.
    #[must_use]
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Builtin(BuiltinType::Void))
    }

    /// Check if the type is a boolean type.
    #[must_use]
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Builtin(BuiltinType::Bool))
    }

    /// Check if the type is a struct type.
    #[must_use]
    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_))
    }

    /// Check if the type is an array type.
    #[must_use]
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }

    /// If the type is a struct, return its name. Otherwise, return `None`.
    #[must_use]
    pub fn as_struct_name(&self) -> Option<&str> {
        if let Type::Struct(name) = self {
            Some(name)
        } else {
            None
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Type::Array(ty, len) => write!(f, "{}[{}]", ty, len),
            Type::Struct(name) => write!(f, "struct {}", name),
            Type::Builtin(ty) => match ty {
                BuiltinType::Bool => write!(f, "bool"),
                BuiltinType::Int => write!(f, "int"),
                BuiltinType::Str => write!(f, "str"),
                BuiltinType::Void => write!(f, "void"),
            },
            Type::Infer => write!(f, "<infer>"),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}

/// Built-in types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Bool,
    Int,
    Str,
    Void,
}

/// A table of types defined in the program. Currently, this only includes struct types, since
/// other types does not require additional metadata.
#[derive(Debug)]
pub struct TypeTable {
    structs: HashMap<String, StructMetadata>,
}

impl TypeTable {
    /// Create a new, empty `TypeTable`.
    #[must_use]
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
        }
    }

    /// Insert a new struct type into the type table.
    pub fn insert_struct(&mut self, name: String, metadata: StructMetadata) {
        self.structs.insert(name, metadata);
    }

    /// Get the metadata for a struct type by its name.
    #[must_use]
    pub fn get_struct_metadata(&self, name: &str) -> Option<&StructMetadata> {
        self.structs.get(name)
    }

    /// Check if a struct type with the given name exists in the type table.
    #[must_use]
    pub fn struct_exists(&self, name: &str) -> bool {
        self.structs.contains_key(name)
    }

    /// Verify if a type supports a specific binary operation.
    ///
    /// # Panics
    /// This function will panic if the type does not exist, for example, if it's a struct type
    /// that is not defined in the `TypeTable`.
    #[must_use]
    pub fn support_binary_op(&self, op: BinaryOp, ty: &Type) -> bool {
        match ty {
            Type::Builtin(builtin_ty) => {
                let metadata = BuiltinMetadata::from(*builtin_ty);
                metadata.supported_binary_ops().contains(&op)
            }
            Type::Struct(name) => {
                let metadata = self
                    .get_struct_metadata(name)
                    .expect("Struct type not found");
                metadata.supported_binary_ops().contains(&op)
            }
            Type::Array(ty, len) => {
                let metadata = ArrayMetadata::from((ty.clone(), *len));
                metadata.supported_binary_ops().contains(&op)
            }
            Type::Infer => false,
            Type::Unknown => false,
        }
    }

    /// Verify if a type supports a specific unary operation.
    ///
    /// # Panics
    /// This function will panic if the type does not exist, for example, if it's a struct type
    /// that is not defined in the `TypeTable`.
    #[must_use]
    pub fn support_unary_op(&self, op: UnaryOp, ty: &Type) -> bool {
        match ty {
            Type::Builtin(builtin_ty) => {
                let metadata = BuiltinMetadata::from(*builtin_ty);
                metadata.supported_unary_ops().contains(&op)
            }
            Type::Struct(name) => {
                let metadata = self
                    .get_struct_metadata(name)
                    .expect("Struct type not found");
                metadata.supported_unary_ops().contains(&op)
            }
            Type::Array(ty, len) => {
                let metadata = ArrayMetadata::from((ty.clone(), *len));
                metadata.supported_unary_ops().contains(&op)
            }
            Type::Unknown | Type::Infer => false,
        }
    }

    /// Return fields of the type. If this is not applicable to the type, return `None`.
    #[must_use]
    pub fn fields_of(&self, ty: &Type) -> Option<&HashMap<String, Type>> {
        match ty {
            Type::Struct(name) => {
                let metadata = self.get_struct_metadata(name)?;
                metadata.fields()
            }
            _ => None,
        }
    }

    /// Get the type of a field by its name for the given type. Returns `None` if the field
    /// does not exist or if the type does not have fields.
    #[must_use]
    pub fn get_field(&self, ty: &Type, name: &str) -> Option<&Type> {
        if let Some(fields) = self.fields_of(ty) {
            fields.get(name)
        } else {
            None
        }
    }

    /// Check if the type has a field with the given name.
    #[must_use]
    pub fn has_field(&self, ty: &Type, name: &str) -> bool {
        self.get_field(ty, name).is_some()
    }
}

/// A trait for type metadata that are common across different types.
pub trait TypeMetadata {
    /// Returns a list of binary operations supported by this type.
    fn supported_binary_ops(&self) -> Vec<BinaryOp>;

    /// Returns a list of unary operations supported by this type.
    fn supported_unary_ops(&self) -> Vec<UnaryOp>;

    /// Returns the fields of the type if applicable. By default, returns `None`, indicating that
    /// the type does not have fields.
    #[must_use]
    fn fields(&self) -> Option<&HashMap<String, Type>> {
        None
    }
}

//// Metadata for struct types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructMetadata {
    /// A mapping from field names to their types.
    pub fields: HashMap<String, Type>,

    /// The span where the struct is defined. This is useful for error reporting.
    pub span: Span,
}

impl StructMetadata {
    /// Create a new `StructMetadata` with the given fields.
    #[must_use]
    pub fn new(fields: HashMap<String, Type>, span: Span) -> Self {
        Self { fields, span }
    }

    /// Get the type of a field by its name. Returns `None` if the field does not exist.
    #[must_use]
    pub fn get_field_type(&self, field_name: &str) -> Option<&Type> {
        self.fields.get(field_name)
    }

    /// Check if the struct has a field with the given name.
    #[must_use]
    pub fn has_field(&self, field_name: &str) -> bool {
        self.fields.contains_key(field_name)
    }
}

impl TypeMetadata for StructMetadata {
    fn supported_binary_ops(&self) -> Vec<BinaryOp> {
        vec![]
    }

    fn supported_unary_ops(&self) -> Vec<UnaryOp> {
        vec![]
    }

    fn fields(&self) -> Option<&HashMap<String, Type>> {
        Some(&self.fields)
    }
}

/// Metadata for array types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayMetadata {
    /// The type of the elements in the array.
    pub element_ty: Box<Type>,

    /// The number of elements in the array.
    pub size: u64,
}

impl From<(Box<Type>, u64)> for ArrayMetadata {
    fn from((element_ty, size): (Box<Type>, u64)) -> Self {
        Self { element_ty, size }
    }
}

impl TypeMetadata for ArrayMetadata {
    fn supported_binary_ops(&self) -> Vec<BinaryOp> {
        vec![]
    }

    fn supported_unary_ops(&self) -> Vec<UnaryOp> {
        vec![]
    }
}

/// Metadata for built-in types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinMetadata {
    pub ty: BuiltinType,
}

impl From<BuiltinType> for BuiltinMetadata {
    fn from(ty: BuiltinType) -> Self {
        Self { ty }
    }
}

impl TypeMetadata for BuiltinMetadata {
    fn supported_binary_ops(&self) -> Vec<BinaryOp> {
        match self.ty {
            BuiltinType::Int => vec![
                BinaryOp::Add,
                BinaryOp::Sub,
                BinaryOp::Mul,
                BinaryOp::Div,
                BinaryOp::Eq,
                BinaryOp::Neq,
                BinaryOp::Lt,
                BinaryOp::Lte,
                BinaryOp::Gt,
                BinaryOp::Gte,
            ],
            BuiltinType::Bool => vec![BinaryOp::And, BinaryOp::Or, BinaryOp::Eq, BinaryOp::Neq],
            BuiltinType::Str => vec![],
            BuiltinType::Void => vec![],
        }
    }

    fn supported_unary_ops(&self) -> Vec<UnaryOp> {
        match self.ty {
            BuiltinType::Int => vec![UnaryOp::Neg],
            BuiltinType::Bool => vec![UnaryOp::Not],
            BuiltinType::Str => vec![],
            BuiltinType::Void => vec![],
        }
    }
}
