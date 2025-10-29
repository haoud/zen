use std::collections::HashMap;

use crate::{BinaryOp, UnaryOp};

/// The types supported by the language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// An array type. The first element is the type of the elements in the array, and the second
    /// element is the size of the array. All elements in the array must have the same type.
    Array(Box<Type>, u64),

    /// A string type. Currently, strings are represented as a sequence of characters and do not
    /// have a fixed length. Future versions of the language may introduce more complex string
    /// types, such as fixed-length strings or string slices.
    Str,

    /// A boolean type. Can be either true or false.
    Bool,

    /// An signed integer type.
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
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Array(ty, size) => write!(f, "{}[{}]", ty, size),
            Type::Unknown => write!(f, "<unknown>"),
            Type::Infer => write!(f, "<infer>"),
            Type::Str => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMetadata {
    /// The minimum alignment of the type in bytes.
    pub align: u32,

    /// The size of the type in bytes. In the case of unsized types (e.g. a string type), this value
    /// will be the size of the metadata required to represent the type (e.g., a pointer and a
    /// length for a string type).
    pub size: u32,

    /// A list of unary operations supported by the type.
    pub unary_op: Vec<UnaryOp>,

    /// A list of binary operations supported by the type.
    pub binary_op: Vec<BinaryOp>,
}

impl TypeMetadata {
    /// Build the type metadata for a given type.
    ///
    /// # Panics
    /// This function will panic if the metadata cannot be built for the given type. This occurs
    /// for types that are not valid for metadata generation, such as `Void`, `Unknown`, or `Infer`.
    #[must_use]
    pub fn build(ty: &Type) -> Self {
        match ty {
            Type::Bool => Self {
                align: 1,
                size: 1,
                unary_op: vec![UnaryOp::Not],
                binary_op: vec![BinaryOp::And, BinaryOp::Or, BinaryOp::Eq, BinaryOp::Neq],
            },
            Type::Int => Self {
                align: 8,
                size: 8,
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
            },
            Type::Str => Self {
                align: 8,
                size: 16,
                binary_op: vec![],
                unary_op: vec![],
            },
            Type::Array(ty, length) => {
                let metadata = TypeMetadata::build(ty);
                let size = metadata.size * (*length as u32);
                Self {
                    align: metadata.align,
                    size,
                    binary_op: vec![],
                    unary_op: vec![],
                }
            }
            Type::Void | Type::Unknown | Type::Infer => {
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
        Self {
            types: HashMap::new(),
        }
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

    /// Add a new type to the type table if it does not already exist. If the type already
    /// exists, this function does nothing.
    ///
    /// This is useful for types that are built-in in the language, but cannot be added beforehand
    /// in the type table. For example, array types. Since there are potentially huge number of
    /// array type combinations (element type + size), it is not feasible to add all of them
    /// beforehand. Instead, we add them on-demand when they are first encountered during type
    /// checking.
    ///
    /// # Returns
    /// Returns `true` if the type was added, `false` if it already existed.
    pub fn try_add_type(&mut self, ty: Type, metadata: TypeMetadata) -> bool {
        if !self.type_exists(&ty) {
            self.types.insert(ty, metadata);
            true
        } else {
            false
        }
    }

    /// Add a new type to the type table if it does not already exist, using the built-in metadata
    /// generation function. If the type already exists, this function does nothing.
    pub fn try_auto_add_type(&mut self, ty: &Type) {
        if !self.type_exists(&ty) {
            let metadata = TypeMetadata::build(&ty);
            self.types.insert(ty.clone(), metadata);
        }
    }
}
