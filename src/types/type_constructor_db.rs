use crate::{ast::lexer::Operator, compiler::layouts::Bytes};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct TypeConstructorId(pub usize);

//Types arent simple, generic, function.... but rather primitive, struct and trait.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Primitive { size: Bytes },
    Struct,
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Primitive { size: Bytes(0) }
    }
}

//Whether a type is signed or unsigned
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum TypeSign {
    #[default]
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParameter(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFunctionDeclaration {
    pub name: String,
    //pub type_args: Vec<GenericParameter>,
    pub args: Vec<TypeUsage>,
    pub return_type: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFieldDeclaration {
    pub name: String,
    pub field_type: TypeUsage,
}

//A type usage informs how the user used a type in a given context, Parameter types may not be known in case of generics
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeUsage {
    Given(TypeConstructorId),
    Generic(GenericParameter),
    Parameterized(TypeConstructorId, Vec<TypeUsage>), //on generics, the base root type has to be known
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeConstructor {
    pub id: TypeConstructorId,
    pub kind: TypeKind,
    pub sign: TypeSign,
    pub name: String,
    //Type args are just Generic parameters, in the future we can add type bounds, in which we would add a Type enum here as well
    pub type_args: Vec<GenericParameter>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeUsage>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeUsage, TypeUsage)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeUsage)>,
    //fields (name, type)
    pub fields: Vec<TypeConstructorFieldDeclaration>,
    //method (name, args, return type)
    pub methods: Vec<TypeConstructorFunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CommonTypeConstructors {
    pub void: TypeConstructorId,
    pub i32: TypeConstructorId,
    pub u32: TypeConstructorId,
    pub i64: TypeConstructorId,
    pub u64: TypeConstructorId,
    pub f32: TypeConstructorId,
    pub f64: TypeConstructorId,
    pub u8: TypeConstructorId,
    pub bool: TypeConstructorId,
    pub array: TypeConstructorId,
    pub function: TypeConstructorId,
    pub ptr: TypeConstructorId,
}

pub struct TypeConstructorDatabase {
    pub types: Vec<TypeConstructor>,
    pub common_types: CommonTypeConstructors,
}

impl TypeConstructorDatabase {
    pub fn new() -> Self {
        let mut item = Self {
            types: vec![],
            common_types: CommonTypeConstructors {
                ..Default::default()
            },
        };
        item.init_builtin();
        item
    }

    pub fn add(&mut self, kind: TypeKind, sign: TypeSign, name: &str) -> TypeConstructorId {
        let next_id = TypeConstructorId(self.types.len());
        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name: name.into(),
            sign,
            ..TypeConstructor::default()
        });
        next_id
    }

    pub fn add_binary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        rhs_type: TypeUsage,
        result_type: TypeUsage,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record
            .rhs_binary_ops
            .push((operator, rhs_type, result_type));
    }

    pub fn add_generic(
        &mut self,
        kind: TypeKind,
        name: &str,
        type_args: Vec<GenericParameter>,
        _rep_size: Option<Bytes>,
    ) -> TypeConstructorId {
        let next_id = TypeConstructorId(self.types.len());

        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name: name.into(),
            type_args,
            ..TypeConstructor::default()
        });
        next_id
    }

    pub fn add_unary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        result_type: TypeUsage,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.unary_ops.push((operator, result_type));
    }

    pub fn find(&self, id: TypeConstructorId) -> &TypeConstructor {
        self.types
            .get(id.0)
            .unwrap_or_else(|| panic!("Type ID not found: {}", id.0))
    }

    pub fn find_by_name(&self, name: &str) -> Option<&TypeConstructor> {
        self.types.iter().find(|t| t.name == name)
    }

    fn register_primitive_number(
        &mut self,
        name: &str,
        size: Bytes,
        sign: TypeSign,
    ) -> TypeConstructorId {
        let type_id = self.add(TypeKind::Primitive { size }, sign, name);
        self.add_binary_operator(
            type_id,
            Operator::Plus,
            TypeUsage::Given(type_id),
            TypeUsage::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Multiply,
            TypeUsage::Given(type_id),
            TypeUsage::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Minus,
            TypeUsage::Given(type_id),
            TypeUsage::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Divide,
            TypeUsage::Given(type_id),
            TypeUsage::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Mod,
            TypeUsage::Given(type_id),
            TypeUsage::Given(type_id),
        );

        let bool_id = self.find_by_name("bool").unwrap().id;
        self.add_binary_operator(
            type_id,
            Operator::Equals,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::NotEquals,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Less,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::LessEquals,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Greater,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::GreaterEquals,
            TypeUsage::Given(type_id),
            TypeUsage::Given(bool_id),
        );

        self.add_unary_operator(type_id, Operator::Plus, TypeUsage::Given(type_id));
        self.add_unary_operator(type_id, Operator::Minus, TypeUsage::Given(type_id));

        type_id
    }

    pub fn add_method(
        &mut self,
        type_id: TypeConstructorId,
        signature: TypeConstructorFunctionDeclaration,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.methods.push(signature);
    }

    pub fn add_simple_field(
        &mut self,
        type_id: TypeConstructorId,
        name: &str,
        field_type: TypeConstructorId,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeConstructorFieldDeclaration {
            name: name.to_string(),
            field_type: TypeUsage::Given(field_type),
        });
    }

    pub fn add_field(&mut self, type_id: TypeConstructorId, name: &str, type_usage: &TypeUsage) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeConstructorFieldDeclaration {
            name: name.to_string(),
            field_type: type_usage.clone(),
        });
    }


    #[allow(clippy::similar_names)]
    fn init_builtin(&mut self) {
        //ptr + len

        let void_type = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            "Void",
        );
        self.common_types.void = void_type;

        //nullptr type
        self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<usize>(),
            },
            TypeSign::Unsigned,
            "None",
        );
        self.common_types.bool = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            "bool",
        );

        let i32_type =
            self.register_primitive_number("i32", Bytes::size_of::<i32>(), TypeSign::Signed);
        let u32_type =
            self.register_primitive_number("u32", Bytes::size_of::<u32>(), TypeSign::Unsigned);
        self.common_types.i32 = i32_type;
        self.common_types.u32 = u32_type;

        self.common_types.i64 =
            self.register_primitive_number("i64", Bytes::size_of::<i64>(), TypeSign::Signed);
        self.common_types.u64 =
            self.register_primitive_number("u64", Bytes::size_of::<u64>(), TypeSign::Unsigned);
        self.common_types.f32 =
            self.register_primitive_number("f32", Bytes::size_of::<f32>(), TypeSign::Signed);
        self.common_types.f64 =
            self.register_primitive_number("f64", Bytes::size_of::<f64>(), TypeSign::Signed);

        self.common_types.u8 = self.register_primitive_number("u8", Bytes::size_of::<u8>(), TypeSign::Unsigned);

        //internal type for pointers, ptr<i32> points to a buffer of i32, and so on
        let ptr_type = self.add_generic(
            TypeKind::Primitive {
                size: Bytes::size_of::<usize>(),
            },
            "ptr",
            vec![GenericParameter("TPtr".into())],
            Some(Bytes::size_of::<usize>()),
        );
        self.common_types.ptr = ptr_type;

        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            "array",
            vec![GenericParameter("TItem".into())],
            Some(Bytes::size_of::<usize>() + Bytes::size_of::<u32>()),
        );

        self.common_types.function = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            "function",
        );
        self.add_method(
            arr_type,
            TypeConstructorFunctionDeclaration {
                name: "__index__".to_string(),
                args: vec![TypeUsage::Given(u32_type)],
                return_type: TypeUsage::Generic(GenericParameter("TItem".into())),
            },
        );

        //u32_type
        self.add_simple_field(arr_type, "length", u32_type);

        self.common_types.array = arr_type;
    }
}

impl Default for TypeConstructorDatabase {
    fn default() -> Self {
        Self::new()
    }
}
