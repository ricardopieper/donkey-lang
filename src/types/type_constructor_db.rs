use crate::{ast::lexer::Operator, compiler::layouts::Bytes};

use crate::interner::InternedString;
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct TypeConstructorId(pub usize);

impl TypeConstructorId {
    pub fn to_string(&self, constructor_db: &TypeConstructorDatabase) -> String {
        constructor_db.get_name(*self)
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter(pub InternedString);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFunctionDeclaration {
    pub name: InternedString,
    pub args: Vec<TypeConstructParams>,
    pub return_type: TypeConstructParams,
    pub is_variadic: bool,
}

impl TypeConstructorFunctionDeclaration {
    pub fn into_function_signature(&self, root_constructor: TypeConstructorId, type_constructors: &TypeConstructorDatabase) -> FunctionSignature<TypeConstructParams> {
        let type_data = &type_constructors.find(root_constructor);
        
        let mut args =  if type_data.type_params.is_empty() {
            vec![
                TypeConstructParams::Given(root_constructor)
            ]
        }   else {
            let type_construct_params = TypeConstructParams::Parameterized(
                TypeConstructParams::Given(root_constructor).into(),
                type_data.type_params.iter().map(|param| TypeConstructParams::Generic(param.clone())).collect()
            );
            vec![type_construct_params]
        };
        args.extend(self.args.clone());
        
        FunctionSignature {
            generics: type_data.type_params.clone(),
            params: args,
            return_type: Box::new(self.return_type.clone()),
            variadic: Variadic(self.is_variadic),
        }

    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFieldDeclaration {
    pub name: InternedString,
    pub field_type: TypeConstructParams,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Variadic(pub bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature<TExpr> {
    pub generics: Vec<TypeParameter>,
    pub params: Vec<TExpr>,
    pub return_type: Box<TExpr>, //boxed so that we can use it in a recursive type
    pub variadic: Variadic,
}

//A type usage informs how the user used a type in a given context, Parameter types may not be known in case of generics.
//This type may be copied a bunch of times but usually it should be cheap, since it's all references...
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstructParams {
    Given(TypeConstructorId),
    Generic(TypeParameter),
    //generics, params, return
    //@TODO I think this FunctionSignature doesn't really belong here....
    FunctionSignature(FunctionSignature<TypeConstructParams>),
    //on generics when the base root type is not known. This is basically the HIRType.
    //The base shouldn't really be a FunctionSignature, never...
    Parameterized(Box<TypeConstructParams>, Vec<TypeConstructParams>),
}

impl TypeConstructParams {
    pub fn to_string(&self, type_db: &TypeConstructorDatabase) -> String {
        match self {
            TypeConstructParams::Given(id) => type_db.find(*id).name.to_string(),
            TypeConstructParams::Generic(generic) => format!("generic {}", generic.0.to_string()),
            TypeConstructParams::Parameterized(id, args) => {
                let mut s = id.to_string(type_db); // type_db.find(*id).name.into();
                s.push('<');
                for arg in args {
                    s.push_str(&arg.to_string(type_db));
                    s.push(',');
                }
                s.push('>');
                s
            }
            TypeConstructParams::FunctionSignature(FunctionSignature {
                generics,
                params,
                return_type,
                ..
            }) => {
                let mut s = String::new();
                s.push_str("fn");
                if !generics.is_empty() {
                    s.push('<');
                    for param in generics {
                        s.push_str("generic ");
                        s.push_str(&param.0.to_string());
                        s.push_str(", ");
                    }
                    s.push('>')
                }

                s.push_str("(");
                for param in params {
                    s.push_str(&param.to_string(type_db));
                    s.push_str(", ");
                }
                s.push_str("): ");

                s.push_str(&return_type.to_string(type_db));

                s
            }
        }
    }

    pub fn try_get_root_type_constructor_id(&self) -> Option<TypeConstructorId> {
        match self {
            TypeConstructParams::Given(id) => Some(*id),
            TypeConstructParams::Parameterized(id, _) => id.try_get_root_type_constructor_id(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub id: TypeConstructorId,
    pub kind: TypeKind,
    pub sign: TypeSign,
    pub name: InternedString,
    //Type args are just Generic parameters, in the future we can add type bounds, in which we would add a Type enum here as well
    pub type_params: Vec<TypeParameter>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeConstructParams>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeConstructParams, TypeConstructParams)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeConstructParams)>,
    //fields (name, type)
    pub fields: Vec<TypeConstructorFieldDeclaration>,
    //method (name, args, return type)
    pub methods: Vec<TypeConstructorFunctionDeclaration>,
    pub needs_intrinsic_codegen: bool,
}

impl TypeConstructor {
    pub fn find_method(&self, name: InternedString) -> Option<&TypeConstructorFunctionDeclaration> {
        self.methods.iter().find(|m| m.name == name)
    }

    pub fn find_field(&self, name: InternedString) -> Option<&TypeConstructorFieldDeclaration> {
        self.fields.iter().find(|m| m.name == name)
    }
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
    pub char: TypeConstructorId,
    pub bool: TypeConstructorId,
    pub array: TypeConstructorId,
    pub function: TypeConstructorId,
    pub ptr: TypeConstructorId,
    pub str: TypeConstructorId,
}

pub struct TypeConstructorDatabase {
    pub types: Vec<TypeConstructor>,
    pub common_types: CommonTypeConstructors,
}

impl TypeConstructorDatabase {
    pub fn new() -> Self {
        let mut item = Self {
            types: vec![],
            common_types: Default::default(),
        };
        item.init_builtin();
        item
    }

    pub fn add(
        &mut self,
        kind: TypeKind,
        sign: TypeSign,
        name: InternedString,
    ) -> TypeConstructorId {
        let next_id = TypeConstructorId(self.types.len());
        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name,
            sign,
            allowed_casts: vec![],
            fields: vec![],
            methods: vec![],
            rhs_binary_ops: vec![],
            type_params: vec![],
            unary_ops: vec![],
            needs_intrinsic_codegen: false
        });
        next_id
    }

    pub fn add_binary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        rhs_type: TypeConstructParams,
        result_type: TypeConstructParams,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record
            .rhs_binary_ops
            .push((operator, rhs_type, result_type));
    }

    pub fn add_generic(
        &mut self,
        kind: TypeKind,
        name: InternedString,
        type_args: Vec<TypeParameter>,
        _rep_size: Option<Bytes>,
    ) -> TypeConstructorId {
        let next_id = TypeConstructorId(self.types.len());

        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name,
            type_params: type_args,
            allowed_casts: vec![],
            fields: vec![],
            methods: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            sign: TypeSign::Unsigned,
            needs_intrinsic_codegen: false
        });
        next_id
    }

    fn mark_as_intrisic(&mut self, type_id: TypeConstructorId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.needs_intrinsic_codegen = true;
    }

    pub fn add_unary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        result_type: TypeConstructParams,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.unary_ops.push((operator, result_type));
    }

    pub fn find(&self, id: TypeConstructorId) -> &TypeConstructor {
        self.types
            .get(id.0)
            .unwrap_or_else(|| panic!("Type ID not found: {}", id.0))
    }

    pub fn get_name(&self, id: TypeConstructorId) -> String {
        let type_data = self
            .types
            .get(id.0)
            .unwrap_or_else(|| panic!("Type ID not found: {}", id.0));
        let base_name = type_data.name;
        base_name.to_string()
    }

    pub fn find_by_name(&self, name: InternedString) -> Option<&TypeConstructor> {
        //@TODO maybe it would be faster to have a map of ids -> instances, this has the potential to grow a lot and be slow
        self.types.iter().find(|t| t.name == name)
    }

    fn register_primitive_number(
        &mut self,
        name: InternedString,
        size: Bytes,
        sign: TypeSign,
    ) -> TypeConstructorId {
        let type_id = self.add(TypeKind::Primitive { size }, sign, name);
        self.add_binary_operator(
            type_id,
            Operator::Plus,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Multiply,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Minus,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Divide,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Mod,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(type_id),
        );

        let bool_id = self.find_by_name(InternedString::new("bool")).unwrap().id;
        self.add_binary_operator(
            type_id,
            Operator::Equals,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::NotEquals,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Less,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::LessEquals,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Greater,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::GreaterEquals,
            TypeConstructParams::Given(type_id),
            TypeConstructParams::Given(bool_id),
        );

        self.add_unary_operator(type_id, Operator::Plus, TypeConstructParams::Given(type_id));
        self.add_unary_operator(
            type_id,
            Operator::Minus,
            TypeConstructParams::Given(type_id),
        );

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
        name: InternedString,
        field_type: TypeConstructorId,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeConstructorFieldDeclaration {
            name,
            field_type: TypeConstructParams::Given(field_type),
        });
    }

    pub fn add_field(
        &mut self,
        type_id: TypeConstructorId,
        name: InternedString,
        type_usage: TypeConstructParams,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeConstructorFieldDeclaration {
            name,
            field_type: type_usage,
        });
    }

    //Note: this could be more complex and allow a whole TypeConstructorParams, but for now we just allow a type constructor id
    pub fn add_cast_to_type(
        &mut self,
        type_id: TypeConstructorId,
        cast_type: TypeConstructorId,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.allowed_casts.push(TypeConstructParams::Given(cast_type));
    }

    #[allow(clippy::similar_names)]
    fn init_builtin(&mut self) {
        let istr = |s| InternedString::new(s);

        let void_type = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            istr("Void"),
        );
        self.common_types.void = void_type;

        self.common_types.bool = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            istr("bool"),
        );

        let i32_type =
            self.register_primitive_number(istr("i32"), Bytes::size_of::<i32>(), TypeSign::Signed);
        let u32_type = self.register_primitive_number(
            istr("u32"),
            Bytes::size_of::<u32>(),
            TypeSign::Unsigned,
        );
        self.common_types.i32 = i32_type;
        self.common_types.u32 = u32_type;

        self.common_types.i64 =
            self.register_primitive_number(istr("i64"), Bytes::size_of::<i64>(), TypeSign::Signed);
        self.common_types.u64 = self.register_primitive_number(
            istr("u64"),
            Bytes::size_of::<u64>(),
            TypeSign::Unsigned,
        );
        self.common_types.f32 =
            self.register_primitive_number(istr("f32"), Bytes::size_of::<f32>(), TypeSign::Signed);
        self.common_types.f64 =
            self.register_primitive_number(istr("f64"), Bytes::size_of::<f64>(), TypeSign::Signed);

        self.common_types.u8 =
            self.register_primitive_number(istr("u8"), Bytes::size_of::<u8>(), TypeSign::Unsigned);

        self.common_types.char = self.register_primitive_number(
            istr("char"),
            Bytes::size_of::<u8>(),
            TypeSign::Unsigned,
        );

        //internal type for pointers, ptr<i32> points to a buffer of i32, and so on

        let ptr_type = self.add_generic(
            TypeKind::Primitive {
                size: Bytes::size_of::<usize>(),
            },
            istr("ptr"),
            vec![TypeParameter(istr("TPtr"))],
            Some(Bytes::size_of::<usize>()),
        );
        self.add_method(
            ptr_type,
            TypeConstructorFunctionDeclaration {
                name: istr("write"),
                args: vec![
                    TypeConstructParams::Given(u32_type), /* offset * sizeof(TPtr)*/
                    TypeConstructParams::Generic(TypeParameter(istr("TPtr"))),
                ],
                return_type: TypeConstructParams::Given(void_type),
                is_variadic: false,
            },
        );
        self.add_method(
            ptr_type,
            TypeConstructorFunctionDeclaration {
                name: istr("read"),
                args: vec![TypeConstructParams::Given(u32_type)],/* offset * sizeof(TPtr) */
                return_type: TypeConstructParams::Generic(TypeParameter(istr("TPtr"))),
                is_variadic: false,
            },
        );
        self.mark_as_intrisic(ptr_type);
        self.common_types.ptr = ptr_type;
        
        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            istr("array"),
            vec![TypeParameter(istr("TItem"))],
            Some(Bytes::size_of::<usize>() + Bytes::size_of::<u32>()),
        );

        self.common_types.function = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            istr("function"),
        );

        self.add_method(
            arr_type,
            TypeConstructorFunctionDeclaration {
                name: istr("__index_ptr__"),
                args: vec![TypeConstructParams::Given(u32_type)],
                return_type: TypeConstructParams::Parameterized(
                    TypeConstructParams::Given(ptr_type).into(),
                    vec![TypeConstructParams::Generic(TypeParameter(istr("TItem")))],
                ),
                is_variadic: false,
            },
        );

        //u32_type
        self.add_simple_field(arr_type, istr("length"), u32_type);

        self.common_types.array = arr_type;

        //self cast is ok... silly but ok
        self.add_cast_to_type(self.common_types.i32, self.common_types.i32);
        self.add_cast_to_type(self.common_types.i32, self.common_types.i64);
        self.add_cast_to_type(self.common_types.i32, self.common_types.f32);
        self.add_cast_to_type(self.common_types.i32, self.common_types.f64);
        self.add_cast_to_type(self.common_types.i32, self.common_types.char);
        self.add_cast_to_type(self.common_types.i32, self.common_types.u8);

        self.add_cast_to_type(self.common_types.i64, self.common_types.i32);
        self.add_cast_to_type(self.common_types.i64, self.common_types.i64);
        self.add_cast_to_type(self.common_types.i64, self.common_types.f32);
        self.add_cast_to_type(self.common_types.i64, self.common_types.f64);
        self.add_cast_to_type(self.common_types.i64, self.common_types.char);
        self.add_cast_to_type(self.common_types.i64, self.common_types.u8);

        self.add_cast_to_type(self.common_types.f32, self.common_types.i32);
        self.add_cast_to_type(self.common_types.f32, self.common_types.i64);
        self.add_cast_to_type(self.common_types.f32, self.common_types.f32);
        self.add_cast_to_type(self.common_types.f32, self.common_types.f64);
        self.add_cast_to_type(self.common_types.f32, self.common_types.char);
        self.add_cast_to_type(self.common_types.f32, self.common_types.u8);

        self.add_cast_to_type(self.common_types.f64, self.common_types.i32);
        self.add_cast_to_type(self.common_types.f64, self.common_types.i64);
        self.add_cast_to_type(self.common_types.f64, self.common_types.f32);
        self.add_cast_to_type(self.common_types.f64, self.common_types.f64);
        self.add_cast_to_type(self.common_types.f64, self.common_types.char);
        self.add_cast_to_type(self.common_types.f64, self.common_types.u8);

        self.add_cast_to_type(self.common_types.char, self.common_types.i32);
        self.add_cast_to_type(self.common_types.char, self.common_types.i64);
        self.add_cast_to_type(self.common_types.char, self.common_types.f32);
        self.add_cast_to_type(self.common_types.char, self.common_types.f64);
        self.add_cast_to_type(self.common_types.char, self.common_types.char);
        self.add_cast_to_type(self.common_types.char, self.common_types.u8);

        self.add_cast_to_type(self.common_types.u8, self.common_types.i32);
        self.add_cast_to_type(self.common_types.u8, self.common_types.i64);
        self.add_cast_to_type(self.common_types.u8, self.common_types.f32);
        self.add_cast_to_type(self.common_types.u8, self.common_types.f64);
        self.add_cast_to_type(self.common_types.u8, self.common_types.char);
        self.add_cast_to_type(self.common_types.u8, self.common_types.u8);
    }
        
}
