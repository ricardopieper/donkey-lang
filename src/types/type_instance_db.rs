use std::collections::HashMap;

use crate::interner::InternedString;
use crate::{ast::lexer::Operator, semantic::hir::TypeParameter};

use super::type_constructor_db::{
    Bytes, TypeConstructor, TypeConstructorDatabase, TypeConstructorFunctionDeclaration,
    TypeConstructorId, TypeKind,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstructionError {
    IncorrectNumberOfArgs { expected: usize, received: usize },
    InsufficientInformation,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeInstanceId(pub usize);

impl TypeInstanceId {
    pub fn to_string(self, type_db: &TypeInstanceManager) -> String {
        type_db.find(self).name.to_string()
    }

    pub fn is_integer(self, type_db: &TypeInstanceManager) -> bool {
        let types = &type_db.common_types;
        self == types.i32
            || self == types.i64
            || self == types.u32
            || self == types.u64
            || self == types.u8
    }

    pub fn is_float(self, type_db: &TypeInstanceManager) -> bool {
        let types = &type_db.common_types;
        self == types.f32 || self == types.f64
    }
}

impl Default for TypeInstanceId {
    fn default() -> Self {
        //Deliberately setting usize::MAX so that, if a common type is not initialized,
        //this will cause an out-of-bounds access sometime, since this is used as an index...
        //OOB is safe in Rust
        Self(usize::MAX)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstanceStructMethod {
    pub name: InternedString,
    pub function_type: TypeInstanceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstanceStructField {
    pub name: InternedString,
    pub field_type: TypeInstanceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstance {
    pub type_constructor_id: TypeConstructorId,
    pub type_substitutions: HashMap<TypeParameter, TypeInstanceId>,
    pub name: InternedString,
}

impl TypeInstance {}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CommonTypeInstances {
    pub void: TypeInstanceId,
    pub i32: TypeInstanceId,
    pub u32: TypeInstanceId,
    pub i64: TypeInstanceId,
    pub u64: TypeInstanceId,
    pub f32: TypeInstanceId,
    pub f64: TypeInstanceId,
    pub u8: TypeInstanceId,
    pub char: TypeInstanceId,
    pub bool: TypeInstanceId,
}

pub struct TypeInstanceManager {
    pub constructors: TypeConstructorDatabase,
    pub types: Vec<TypeInstance>,
    pub common_types: CommonTypeInstances,
}

impl TypeInstanceManager {
    pub fn new(ctors: TypeConstructorDatabase) -> TypeInstanceManager {
        let item = TypeInstanceManager {
            types: vec![],
            constructors: ctors,
            common_types: Default::default(),
        };

        item
    }

    pub fn find(&self, id: TypeInstanceId) -> &TypeInstance {
        &self.types[id.0]
    }
}
