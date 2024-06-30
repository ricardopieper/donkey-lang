use crate::semantic::type_name_printer::TypeNamePrinter;
use crate::{ast::lexer::Operator, compiler::layouts::Bytes};

use crate::interner::InternedString;

#[cfg(not(test))] 
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeConstructorId(pub usize);

#[cfg(test)] 
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeConstructorId(pub usize, pub InternedString);


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
    Function
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
    pub signature: TypeConstructorId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFieldDeclaration {
    pub name: InternedString,
    pub field_type: TypeConstructParams,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Variadic(pub bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub type_parameters: Vec<TypeParameter>,
    pub params: Vec<TypeConstructParams>,
    pub return_type: TypeConstructParams, //boxed so that we can use it in a recursive type
    pub variadic: Variadic,
}

//A type usage informs how the user used a type in a given context, Parameter types may not be known in case of generics.
//In this case, only generic parameters are specified as strings, all other types are known.
//It's basically HIRType with TypeConstructorId instead of strings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstructParams {
    Generic(TypeParameter),
    Parameterized(TypeConstructorId, Vec<TypeConstructParams>),
}

impl TypeConstructParams {

    pub fn simple(type_constructor_id: TypeConstructorId) -> Self {
        TypeConstructParams::Parameterized(type_constructor_id, vec![])
    }

    pub fn to_string(&self, type_db: &TypeConstructorDatabase) -> String {
        match self {
            TypeConstructParams::Generic(generic) => format!("generic {}", generic.0.to_string()),
            TypeConstructParams::Parameterized(id, args) => {
                let mut s = id.to_string(type_db); // type_db.find(*id).name.into();
                if args.is_empty() {
                    return s;
                }
                s.push('<');
                for arg in args {
                    s.push_str(&arg.to_string(type_db));
                    s.push(',');
                }
                s.push('>');
                s
            }
        }
    }

    pub fn try_get_base(&self) -> Option<TypeConstructorId> {
        match self {
            TypeConstructParams::Parameterized(base, _) => Some(*base),
            _ => None,
        }
    }

    pub fn args_and_return(&self) -> (Vec<TypeConstructParams>, TypeConstructParams) {
        match self {
            TypeConstructParams::Parameterized(_, args) => {

                if args.len() < 2 {
                    return (vec![], args.first().unwrap().clone());
                }
                let (args, ret) = args.split_at(args.len() - 1);
                (args.to_vec(), ret.first().unwrap().clone())
            }
            _ => panic!("Not a Parameterized type, cannot be a function"),
        }
    }


}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub id: TypeConstructorId,
    pub kind: TypeKind,
    pub sign: TypeSign,
    pub name: InternedString,
    //Type args are just Generic parameters, in the future we can add type bounds, in which we would add a Type enum here as well.
    pub type_params: Vec<TypeParameter>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeConstructParams>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeConstructParams, TypeConstructParams)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeConstructParams)>,
    //fields (name, type)
    pub fields: Vec<TypeConstructorFieldDeclaration>,
    //method (name, args (if includes self then it's a method), return type)
    pub functions: Vec<TypeConstructorFunctionDeclaration>,
    //The return type of this function, if it's a function.
    pub function_return_type: Option<TypeConstructParams>, 
    pub function_params: Vec<TypeConstructParams>,
    pub function_variadic: Variadic,
    pub is_intrinsic: bool
}

impl TypeConstructor {
    pub fn find_method(&self, name: InternedString) -> Option<&TypeConstructorFunctionDeclaration> {
        self.functions.iter().find(|m| m.name == name)
    }

    pub fn find_field(&self, name: InternedString) -> Option<&TypeConstructorFieldDeclaration> {
        self.fields.iter().find(|m| m.name == name)
    }

    /*pub fn as_function_type_construct_params(&self) -> TypeConstructParams {
        let mut parameters = vec![];
        for p in self.function_params.iter() {
            parameters.push(p.clone());
        }
        
        parameters.push(self.function_return_type.clone().unwrap());

        return TypeConstructParams::Parameterized(self.id, parameters);
    }*/
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        let default_type_constructor_id =  {
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(0,  InternedString::new("NONE"))
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(0)
            }
        };

        let mut item = Self {
            types: vec![],
            common_types: CommonTypeConstructors {
                void: default_type_constructor_id.clone(),
                i32: default_type_constructor_id.clone(),
                u32: default_type_constructor_id.clone(),
                i64: default_type_constructor_id.clone(),
                u64: default_type_constructor_id.clone(),
                f32: default_type_constructor_id.clone(),
                f64: default_type_constructor_id.clone(),
                u8: default_type_constructor_id.clone(),
                char: default_type_constructor_id.clone(),
                bool: default_type_constructor_id.clone(),
                array: default_type_constructor_id.clone(),
                function: default_type_constructor_id.clone(),
                ptr: default_type_constructor_id.clone(),
                str: default_type_constructor_id.clone(),
            }
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
        let next_id ={
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(self.types.len(), name)
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(self.types.len())
            }
        };
        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name,
            sign,
            allowed_casts: vec![],
            fields: vec![],
            functions: vec![],
            rhs_binary_ops: vec![],
            type_params: vec![],
            unary_ops: vec![],
            is_intrinsic: false,
            function_params: vec![],
            function_return_type: None,
            function_variadic: Variadic(false),
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
    ) -> TypeConstructorId {
        let next_id ={
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(self.types.len(), name)
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(self.types.len())
            }
        };
        self.types.push(TypeConstructor {
            id: next_id,
            kind,
            name,
            type_params: type_args,
            allowed_casts: vec![],
            fields: vec![],
            functions: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            sign: TypeSign::Unsigned,
            is_intrinsic: false,
            function_params: vec![],
            function_return_type: None,
            function_variadic: Variadic(false),
        });
        next_id
    }

    pub fn add_function_signature(
        &mut self,
        signature: FunctionSignature,
    ) -> TypeConstructorId {
        let next_id ={
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(self.types.len(), InternedString::new("fn"))
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(self.types.len())
            }
        };
        self.types.push(TypeConstructor {
            id: next_id,
            kind: TypeKind::Function,
            name: InternedString::new("fn"),
            sign: TypeSign::Unsigned,
            allowed_casts: vec![],
            fields: vec![],
            functions: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            is_intrinsic: false,
            type_params: signature.type_parameters,
            function_params: signature.params,
            function_return_type: Some(signature.return_type),
            function_variadic: signature.variadic,
        });
        next_id
    }

    fn mark_as_intrisic(&mut self, type_id: TypeConstructorId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.is_intrinsic = true;
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
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Multiply,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Minus,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Divide,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Mod,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(type_id),
        );

        let bool_id = self.find_by_name(InternedString::new("bool")).unwrap().id;
        self.add_binary_operator(
            type_id,
            Operator::Equals,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );
        self.add_binary_operator(
            type_id,
            Operator::NotEquals,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );
        self.add_binary_operator(
            type_id,
            Operator::Less,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );
        self.add_binary_operator(
            type_id,
            Operator::LessEquals,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );
        self.add_binary_operator(
            type_id,
            Operator::Greater,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );
        self.add_binary_operator(
            type_id,
            Operator::GreaterEquals,
            TypeConstructParams::simple(type_id),
            TypeConstructParams::simple(bool_id)
        );

        self.add_unary_operator(type_id, Operator::Plus, TypeConstructParams::simple(type_id));
        self.add_unary_operator(
            type_id,
            Operator::Minus,
            TypeConstructParams::simple(type_id),
        );

        type_id
    }


    pub fn add_function_to_type(
        &mut self,
        type_id: TypeConstructorId,
        name: InternedString,
        signature: FunctionSignature,
    ) -> TypeConstructorId {
        //search through the functions to see if we already have something similar
        //not by name, but by signature
        let found = self.try_find_function_by_signature(&signature);

        let function_type_id = if let None = found {
            let next_id ={
                //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
                #[cfg(test)]
                {
                    TypeConstructorId(self.types.len(), InternedString::new("fn"))
                }
                #[cfg(not(test))]
                {
                    TypeConstructorId(self.types.len())
                }
            };
            self.types.push(TypeConstructor {
                id: next_id,
                kind: TypeKind::Function,
                name,
                sign: TypeSign::Unsigned,
                allowed_casts: vec![],
                fields: vec![],
                functions: vec![],
                rhs_binary_ops: vec![],
                type_params: vec![],
                unary_ops: vec![],
                is_intrinsic: false,
                function_params: signature.params,
                function_return_type: Some(signature.return_type),
                function_variadic: signature.variadic,
            });
            next_id
        } else {
            found.unwrap()
        };

        let record = self.types.get_mut(type_id.0).unwrap();

        record.functions.push(TypeConstructorFunctionDeclaration {
            name,
            signature: function_type_id,
        });

        return function_type_id;
    }

    //This function can be smarter by reworking the signature into a strict naming convention for type parameters,
    //or by using unification (maybe).
    fn try_find_function_by_signature(&mut self, signature: &FunctionSignature) -> Option<TypeConstructorId> {
        let mut found = None;
        for ty in self.types.iter() {
            if ty.kind != TypeKind::Function {
                continue;
            }

            if let Some(f) = &ty.function_return_type && f != &signature.return_type {
                continue;
            }
    
            if ty.function_params != signature.params {
                continue;
            }

            if ty.type_params != signature.type_parameters {
                continue;
            }
          
            if ty.function_variadic != signature.variadic {
                continue;
            }
            found = Some(ty.id);
            break;
        }
        found
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
            field_type: TypeConstructParams::simple(field_type),
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
    pub fn add_cast_to_type(&mut self, type_id: TypeConstructorId, cast_type: TypeConstructorId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record
            .allowed_casts
            .push(TypeConstructParams::simple(cast_type));
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
            vec![TypeParameter(istr("TPtr"))]
        );
        self.add_function_to_type(
            ptr_type,
            istr("write"),
            FunctionSignature {
                type_parameters: vec![],
                params: vec![
                    TypeConstructParams::Parameterized(
                        ptr_type,
                        vec![TypeConstructParams::Generic(TypeParameter(istr("TPtr")))],
                    ), //self
                    TypeConstructParams::simple(self.common_types.u64), /*index*/
                    TypeConstructParams::Generic(TypeParameter(istr("TPtr"))), /*item to write*/
                ],
                return_type: TypeConstructParams::simple(void_type),
                variadic: Variadic(false),
            
            },
        );
        self.add_function_to_type(
            ptr_type,
            istr("read"),
            FunctionSignature {
                    type_parameters: vec![],
                    params: vec![
                        TypeConstructParams::Parameterized(
                            ptr_type,
                            vec![TypeConstructParams::Generic(TypeParameter(istr("TPtr")))],
                        ), //self
                        TypeConstructParams::simple(self.common_types.u64)],/* offset * sizeof(TPtr) */
                    return_type: TypeConstructParams::Generic(TypeParameter(istr("TPtr"))).into(),
                    variadic: Variadic(false),
                
            },
        );
        self.add_function_to_type(
            ptr_type,
            istr("__index_ptr__"),
            FunctionSignature {
                    params: vec![
                        TypeConstructParams::Parameterized(
                            ptr_type,
                            vec![TypeConstructParams::Generic(TypeParameter(istr("TPtr")))],
                        ), //self
                        TypeConstructParams::simple(self.common_types.u64)],/* offset * sizeof(TPtr) */
                    return_type: TypeConstructParams::Parameterized(
                        ptr_type,
                        vec![TypeConstructParams::Generic(TypeParameter(istr("TPtr")))],
                    ).into(),
                    variadic: Variadic(false),
                    type_parameters: vec![]
            },
        );
        self.mark_as_intrisic(ptr_type);
        self.common_types.ptr = ptr_type;

        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            istr("array"),
            vec![TypeParameter(istr("TItem"))]
        );

        self.common_types.function = self.add(
            TypeKind::Primitive {
                size: Bytes::size_of::<()>(),
            },
            TypeSign::Unsigned,
            istr("function"),
        );

        self.add_function_to_type(
            arr_type,
            istr("__index_ptr__"),
             FunctionSignature {
                    params: vec![
                        TypeConstructParams::Parameterized(
                            arr_type,
                            vec![TypeConstructParams::Generic(TypeParameter(istr("TItem")))],
                        ), //self
                        TypeConstructParams::simple(u32_type)
                    ],
                    return_type: TypeConstructParams::Parameterized(
                        ptr_type,
                        vec![TypeConstructParams::Generic(TypeParameter(istr("TItem")))],
                    )
                    .into(),
                    variadic: Variadic(false),
                    type_parameters: vec![],
            },
        );



        //u32_type
        self.add_simple_field(arr_type, istr("length"), u32_type);

        self.common_types.array = arr_type;

        //self cast is ok... silly but ok
        self.add_cast_to_type(self.common_types.i32, self.common_types.i32);
        self.add_cast_to_type(self.common_types.i32, self.common_types.i64);
        self.add_cast_to_type(self.common_types.i32, self.common_types.u32);
        self.add_cast_to_type(self.common_types.i32, self.common_types.u64);
        self.add_cast_to_type(self.common_types.i32, self.common_types.f32);
        self.add_cast_to_type(self.common_types.i32, self.common_types.f64);
        self.add_cast_to_type(self.common_types.i32, self.common_types.char);
        self.add_cast_to_type(self.common_types.i32, self.common_types.u8);

        self.add_cast_to_type(self.common_types.i64, self.common_types.i32);
        self.add_cast_to_type(self.common_types.i64, self.common_types.i64);
        self.add_cast_to_type(self.common_types.i64, self.common_types.u32);
        self.add_cast_to_type(self.common_types.i64, self.common_types.u64);
        self.add_cast_to_type(self.common_types.i64, self.common_types.f32);
        self.add_cast_to_type(self.common_types.i64, self.common_types.f64);
        self.add_cast_to_type(self.common_types.i64, self.common_types.char);
        self.add_cast_to_type(self.common_types.i64, self.common_types.u8);

        self.add_cast_to_type(self.common_types.f32, self.common_types.i32);
        self.add_cast_to_type(self.common_types.f32, self.common_types.i64);
        self.add_cast_to_type(self.common_types.f32, self.common_types.u32);
        self.add_cast_to_type(self.common_types.f32, self.common_types.u64);
        self.add_cast_to_type(self.common_types.f32, self.common_types.f32);
        self.add_cast_to_type(self.common_types.f32, self.common_types.f64);
        self.add_cast_to_type(self.common_types.f32, self.common_types.char);
        self.add_cast_to_type(self.common_types.f32, self.common_types.u8);

        self.add_cast_to_type(self.common_types.f64, self.common_types.i32);
        self.add_cast_to_type(self.common_types.f64, self.common_types.i64);
        self.add_cast_to_type(self.common_types.f64, self.common_types.u32);
        self.add_cast_to_type(self.common_types.f64, self.common_types.u64);
        self.add_cast_to_type(self.common_types.f64, self.common_types.f32);
        self.add_cast_to_type(self.common_types.f64, self.common_types.f64);
        self.add_cast_to_type(self.common_types.f64, self.common_types.char);
        self.add_cast_to_type(self.common_types.f64, self.common_types.u8);

        self.add_cast_to_type(self.common_types.char, self.common_types.i32);
        self.add_cast_to_type(self.common_types.char, self.common_types.i64);
        self.add_cast_to_type(self.common_types.char, self.common_types.u32);
        self.add_cast_to_type(self.common_types.char, self.common_types.u64);
        self.add_cast_to_type(self.common_types.char, self.common_types.f32);
        self.add_cast_to_type(self.common_types.char, self.common_types.f64);
        self.add_cast_to_type(self.common_types.char, self.common_types.char);
        self.add_cast_to_type(self.common_types.char, self.common_types.u8);

        self.add_cast_to_type(self.common_types.u8, self.common_types.i32);
        self.add_cast_to_type(self.common_types.u8, self.common_types.i64);
        self.add_cast_to_type(self.common_types.u8, self.common_types.u32);
        self.add_cast_to_type(self.common_types.u8, self.common_types.u64);
        self.add_cast_to_type(self.common_types.u8, self.common_types.f32);
        self.add_cast_to_type(self.common_types.u8, self.common_types.f64);
        self.add_cast_to_type(self.common_types.u8, self.common_types.char);
        self.add_cast_to_type(self.common_types.u8, self.common_types.u8);

        self.add_cast_to_type(self.common_types.u32, self.common_types.i32);
        self.add_cast_to_type(self.common_types.u32, self.common_types.i64);
        self.add_cast_to_type(self.common_types.u32, self.common_types.u32);
        self.add_cast_to_type(self.common_types.u32, self.common_types.u64);
        self.add_cast_to_type(self.common_types.u32, self.common_types.f32);
        self.add_cast_to_type(self.common_types.u32, self.common_types.f64);
        self.add_cast_to_type(self.common_types.u32, self.common_types.char);
        self.add_cast_to_type(self.common_types.u32, self.common_types.u8);

        self.add_cast_to_type(self.common_types.u64, self.common_types.i32);
        self.add_cast_to_type(self.common_types.u64, self.common_types.i64);
        self.add_cast_to_type(self.common_types.u64, self.common_types.u32);
        self.add_cast_to_type(self.common_types.u64, self.common_types.u64);
        self.add_cast_to_type(self.common_types.u64, self.common_types.f32);
        self.add_cast_to_type(self.common_types.u64, self.common_types.f64);
        self.add_cast_to_type(self.common_types.u64, self.common_types.char);
        self.add_cast_to_type(self.common_types.u64, self.common_types.u8);
    }
}
