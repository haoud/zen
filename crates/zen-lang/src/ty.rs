use std::collections::HashMap;

use crate::{BinaryOp, UnaryOp};

/// The types supported by the language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// An array type. The first element is the type of the elements in the array, and the second
    /// element is the size of the array. All elements in the array must have the same type.
    Array(Box<Type>, u64),

    /// A struct type. Currently, structs are identified by their name only, and field types are
    /// stored in the `TypeMetadata` associated with the struct type.
    Struct(String),

    /// A string type. Currently, strings are represented as a sequence of characters and do not
    /// have a fixed length. Future versions of the language may introduce more complex string
    /// types, such as fixed-length strings or string slices.
    Str,

    /// A boolean type. Can be either true or false.
    Bool,

    /// A signed integer type.
    Int,

    /// Void type. It is only used to indicate that a function does not return a value.
    Void,

    /// A type that represents an unknown type. This is primarily used to indicate
    /// an error in type checking, where the type could not be determined. This type
    /// should not appear in the final AST after type checking.
    Unknown,

    /// A special type that indicates the type is to be inferred during type checking. This is
    /// primarily used for integer literals that do not have an explicit type annotation, or for
    /// expressions where the type can be determined from context (e.g., the result of a binary
    /// operation where both operands have the same type). When type inference is complete, all
    /// instances of this type should be replaced with a concrete type, and any remaining instances
    /// of this type indicate a failure to infer the type.
    Infer,
}

impl Type {
    /// Check if the type is a boolean type.
    #[must_use]
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Bool)
    }

    /// Check if the type is a valid type (i.e., not Unknown or Infer).
    #[must_use]
    pub fn is_valid(&self) -> bool {
        !matches!(self, Type::Unknown | Type::Infer)
    }

    /// Check if the type is a built-in type or derived from built-in types. Built-in types are
    /// `Int`, `Bool`, `Str`... Array types derived from built-in types are also considered
    /// built-in types.
    #[must_use]
    pub fn is_builtin(&self) -> bool {
        !self.is_user_defined()
    }

    /// Check if the type is user-defined or not. User-defined types are struct, enum, and similar
    /// types. Array of those types are also considered user-defined, but array of built-in types
    /// are not.
    #[must_use]
    pub fn is_user_defined(&self) -> bool {
        match self {
            Type::Array(ty, _) => ty.is_user_defined(),
            Type::Struct(_) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Array(ty, size) => write!(f, "{}[{}]", ty, size),
            Type::Struct(name) => write!(f, "struct {}", name),
            Type::Unknown => write!(f, "<unknown>"),
            Type::Infer => write!(f, "<infer>"),
            Type::Str => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Void => write!(f, "void"),
        }
    }
}

/// Metadata about a type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMetadata {
    /// A list of unary operations supported by the type.
    pub unary_op: Vec<UnaryOp>,

    /// A list of binary operations supported by the type.
    pub binary_op: Vec<BinaryOp>,

    /// A map of field names to their types for struct types.
    pub fields: HashMap<String, Type>,
}

impl TypeMetadata {
    #[must_use]
    /// Create an empty type metadata.
    pub fn empty() -> Self {
        Self {
            unary_op: vec![],
            binary_op: vec![],
            fields: HashMap::new(),
        }
    }

    /// Create the type metadata for the built-in boolean type.
    #[must_use]
    pub fn bool() -> Self {
        Self {
            unary_op: vec![UnaryOp::Not],
            binary_op: vec![BinaryOp::And, BinaryOp::Or, BinaryOp::Eq, BinaryOp::Neq],
            fields: HashMap::new(),
        }
    }

    /// Create the type metadata for the built-in integer type.
    #[must_use]
    pub fn int() -> Self {
        Self {
            unary_op: vec![UnaryOp::Neg],
            binary_op: vec![
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
            fields: HashMap::new(),
        }
    }

    /// Create the type metadata for the built-in string type.
    #[must_use]
    pub fn str() -> Self {
        Self {
            unary_op: vec![],
            binary_op: vec![],
            fields: HashMap::new(),
        }
    }

    /// Build the type metadata for a given type.
    ///
    /// # Panics
    /// This function will panic if the metadata cannot be built for the given type. This occurs
    /// for types that are not valid for metadata generation, such as `Void`, `Unknown`, or `Infer`.
    #[must_use]
    pub fn build(ty: &Type) -> Self {
        match ty {
            Type::Bool => Self {
                unary_op: vec![UnaryOp::Not],
                binary_op: vec![BinaryOp::And, BinaryOp::Or, BinaryOp::Eq, BinaryOp::Neq],
                fields: HashMap::new(),
            },
            Type::Int => Self {
                unary_op: vec![UnaryOp::Neg],
                binary_op: vec![
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
                fields: HashMap::new(),
            },
            Type::Str => Self {
                binary_op: vec![],
                unary_op: vec![],
                fields: HashMap::new(),
            },
            Type::Array(_, _) => Self {
                binary_op: vec![],
                unary_op: vec![],
                fields: HashMap::new(),
            },
            Type::Struct(_) => Self {
                binary_op: vec![],
                unary_op: vec![],
                fields: HashMap::new(),
            },
            Type::Void => Self {
                binary_op: vec![],
                unary_op: vec![],
                fields: HashMap::new(),
            },
            Type::Unknown | Type::Infer => {
                panic!("TypeMetadata cannot be built for void, unknown, or infer types")
            }
        }
    }

    /// Check if the type supports the given binary operation.
    #[must_use]
    pub fn support_binary_op(&self, op: BinaryOp) -> bool {
        self.binary_op.contains(&op)
    }

    /// Check if the type supports the given unary operation.
    #[must_use]
    pub fn support_unary_op(&self, op: UnaryOp) -> bool {
        self.unary_op.contains(&op)
    }

    /// Check if the type has a field with the given name.
    #[must_use]
    pub fn has_field(&self, name: &str) -> bool {
        self.fields.contains_key(name)
    }

    /// Get the type of a field with the given name.
    #[must_use]
    pub fn get_field_type(&self, name: &str) -> Option<&Type> {
        self.fields.get(name)
    }
}

/// A table of types used in the program. This stores information about all the types defined
/// in the program, including built-in types and user-defined types. This allows for efficient
/// type checking and type inference during semantic analysis.
#[derive(Debug, Clone)]
pub struct TypeTable {
    types: HashMap<Type, TypeMetadata>,
}

impl TypeTable {
    /// Create a new, empty type table.
    pub fn new() -> Self {
        let mut table = Self {
            types: HashMap::new(),
        };

        table.add_type(Type::Void, TypeMetadata::empty());
        table.add_type(Type::Bool, TypeMetadata::bool());
        table.add_type(Type::Int, TypeMetadata::int());
        table.add_type(Type::Str, TypeMetadata::str());
        table
    }

    /// Check if a type exists in the type table.
    #[must_use]
    pub fn type_exists(&self, ty: &Type) -> bool {
        self.types.contains_key(ty)
    }

    /// Get the metadata for a type from the type table.
    #[must_use]
    pub fn get_type_metadata(&self, ty: &Type) -> Option<&TypeMetadata> {
        self.types.get(ty)
    }

    /// Add a new type to the type table.
    ///
    /// # Panics
    /// This function will panic if the type already exists in the type table.
    pub fn add_type(&mut self, ty: Type, metadata: TypeMetadata) {
        assert!(!self.type_exists(&ty));
        self.types.insert(ty, metadata);
    }

    /// Add a new type to the type table if it does not already exist, is a valid type and is
    /// not a built-in type. This is useful for automatically adding types that has no definition
    /// in the program but are still a built-in type or derived from built-in types, such as arrays
    /// of integers, booleans...
    pub fn try_add_builtin_type(&mut self, ty: &Type) {
        if !self.type_exists(&ty) && ty.is_builtin() && ty.is_valid() {
            let metadata = TypeMetadata::build(&ty);
            self.types.insert(ty.clone(), metadata);
        }
    }
}
