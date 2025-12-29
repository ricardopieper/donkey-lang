use crate::semantic::hir::{MonoType, TypeParameter, TypeVariable};
use crate::semantic::typer::Substitution;
use crate::ast::lexer::Operator;

use crate::interner::InternedString;

#[cfg(not(test))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeConstructorId(pub usize);

#[cfg(test)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeConstructorId(pub usize, pub InternedString);

impl TypeConstructorId {
    pub fn to_string(&self, constructor_db: &TypeConstructorDatabase) -> String {
        constructor_db.get_name(*self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Primitive { size: Bytes },
    Struct,
    Function,
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
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFunctionDeclaration {
    pub name: InternedString,
    pub signature: TypeConstructorId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructorFieldDeclaration {
    pub name: InternedString,
    pub field_type: MonoType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Variadic(pub bool);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    //These type parameters must include the ones in the struct itself, so that we can treat them as
    //any other function and run more or less the same algorithms
    pub type_parameters: Vec<TypeParameter>,
    pub parameters: Vec<MonoType>,
    pub return_type: MonoType, //boxed so that we can use it in a recursive type
    pub variadic: Variadic,
}

impl FunctionSignature {
    pub fn print_name(&self, type_db: &TypeConstructorDatabase) -> String {
        let mut name = String::new();
        name.push('(');
        for (i, p) in self.parameters.iter().enumerate() {
            name.push_str(&p.print_name(type_db));
            if i != self.parameters.len() - 1 {
                name.push_str(", ");
            }
        }
        name.push_str(") -> ");
        name.push_str(&self.return_type.print_name(type_db));
        name
    }

    pub fn apply_substitution(&self, substitution: &Substitution) -> Self {
        let mut new_params = vec![];
        for param in self.parameters.iter() {
            new_params.push(param.apply_substitution(substitution));
        }
        let new_return_type = self.return_type.apply_substitution(substitution);
        let new_type_parameters = self
            .type_parameters
            .iter()
            .map(|p| p.apply_substitution(substitution))
            .collect();
        Self {
            type_parameters: new_type_parameters,
            parameters: new_params,
            return_type: new_return_type,
            variadic: self.variadic,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
//You can make a TypeConstructor into a PolyType
pub struct TypeConstructor {
    pub id: TypeConstructorId,
    pub kind: TypeKind,
    pub sign: TypeSign,
    pub name: InternedString,
    //Type args are just Generic parameters, in the future we can add type bounds, in which we would add a Type enum here as well.
    pub type_params: Vec<TypeParameter>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<MonoType>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, MonoType, MonoType)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, MonoType)>,
    //fields (name, type)
    pub fields: Vec<TypeConstructorFieldDeclaration>,
    //method (name, args (if includes self then it's a method), return type)
    pub functions: Vec<TypeConstructorFunctionDeclaration>,
    //The return type of this function, if it's a function.
    pub function_return_type: Option<MonoType>,
    pub function_params: Vec<MonoType>,
    pub function_variadic: Variadic,
    pub is_intrinsic: bool,
    pub is_external: bool,
}

impl TypeConstructor {
    pub fn find_method(&self, name: InternedString) -> Option<&TypeConstructorFunctionDeclaration> {
        self.functions.iter().find(|m| m.name == name)
    }

    pub fn find_field(&self, name: InternedString) -> Option<&TypeConstructorFieldDeclaration> {
        self.fields.iter().find(|m| m.name == name)
    }
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
    pub ptr: TypeConstructorId,
}

pub struct TypeConstructorDatabase {
    pub types: Vec<TypeConstructor>,
    pub common_types: CommonTypeConstructors,
}

impl TypeConstructorDatabase {
    pub fn new() -> Self {
        let default_type_constructor_id = {
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(0, InternedString::new("NONE"))
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(0)
            }
        };

        let mut item = Self {
            types: vec![],
            common_types: CommonTypeConstructors {
                void: default_type_constructor_id,
                i32: default_type_constructor_id,
                u32: default_type_constructor_id,
                i64: default_type_constructor_id,
                u64: default_type_constructor_id,
                f32: default_type_constructor_id,
                f64: default_type_constructor_id,
                u8: default_type_constructor_id,
                char: default_type_constructor_id,
                bool: default_type_constructor_id,
                array: default_type_constructor_id,
                ptr: default_type_constructor_id,
            },
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
        let next_id = {
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
            is_external: false,
        });
        next_id
    }

    pub fn add_binary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        rhs_type: MonoType,
        result_type: MonoType,
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
        let next_id = {
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
            sign: TypeSign::None,
            is_intrinsic: false,
            function_params: vec![],
            function_return_type: None,
            function_variadic: Variadic(false),
            is_external: false,
        });
        next_id
    }

    pub fn add_function_signature(&mut self, signature: FunctionSignature) -> TypeConstructorId {
        let generated_name = signature.print_name(self);
        let next_id = {
            //if test, build with interned, otherwise build without it. DO a compiler cfg test conditional
            #[cfg(test)]
            {
                TypeConstructorId(self.types.len(), InternedString::new(&generated_name))
            }
            #[cfg(not(test))]
            {
                TypeConstructorId(self.types.len())
            }
        };
        self.types.push(TypeConstructor {
            id: next_id,
            kind: TypeKind::Function,
            name: InternedString::new(&generated_name),
            sign: TypeSign::Unsigned,
            allowed_casts: vec![],
            fields: vec![],
            functions: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            is_intrinsic: false,
            type_params: signature.type_parameters,
            function_params: signature.parameters,
            function_return_type: Some(signature.return_type),
            function_variadic: signature.variadic,
            is_external: false,
        });
        next_id
    }

    pub fn mark_as_intrisic(&mut self, type_id: TypeConstructorId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.is_intrinsic = true;
    }

    pub fn mark_as_external(&mut self, type_id: TypeConstructorId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.is_external = true;
    }

    pub fn add_unary_operator(
        &mut self,
        type_id: TypeConstructorId,
        operator: Operator,
        result_type: MonoType,
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
            MonoType::simple(type_id),
            MonoType::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Multiply,
            MonoType::simple(type_id),
            MonoType::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Minus,
            MonoType::simple(type_id),
            MonoType::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Divide,
            MonoType::simple(type_id),
            MonoType::simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Mod,
            MonoType::simple(type_id),
            MonoType::simple(type_id),
        );

        let bool_id = self.find_by_name(InternedString::new("bool")).unwrap().id;
        self.add_binary_operator(
            type_id,
            Operator::Equals,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::NotEquals,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Less,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::LessEquals,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Greater,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::GreaterEquals,
            MonoType::simple(type_id),
            MonoType::simple(bool_id),
        );

        self.add_unary_operator(type_id, Operator::Plus, MonoType::simple(type_id));
        self.add_unary_operator(type_id, Operator::Minus, MonoType::simple(type_id));

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

        let function_type_id = if found.is_none() {
            let next_id = {
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
                type_params: signature.type_parameters,
                unary_ops: vec![],
                is_intrinsic: false,
                function_params: signature.parameters,
                function_return_type: Some(signature.return_type),
                function_variadic: signature.variadic,
                is_external: false,
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

        function_type_id
    }

    //This function can be smarter by reworking the signature into a strict naming convention for type parameters,
    //or by using unification (maybe).
    fn try_find_function_by_signature(
        &mut self,
        signature: &FunctionSignature,
    ) -> Option<TypeConstructorId> {
        let mut found = None;
        for ty in self.types.iter() {
            if ty.kind != TypeKind::Function {
                continue;
            }

            if let Some(f) = &ty.function_return_type
                && f != &signature.return_type
            {
                continue;
            }

            if ty.function_params != signature.parameters {
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
            field_type: MonoType::Application(field_type, vec![]),
        });
    }

    pub fn add_field(
        &mut self,
        type_id: TypeConstructorId,
        name: InternedString,
        type_usage: MonoType,
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
        record.allowed_casts.push(MonoType::simple(cast_type));
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
        );
        self.add_function_to_type(
            ptr_type,
            istr("write"),
            FunctionSignature {
                type_parameters: vec![TypeParameter(istr("TPtr"))],
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![MonoType::Application(
                            ptr_type,
                            vec![MonoType::Variable(TypeVariable(istr("TPtr")))],
                        )],
                    ), //self
                    MonoType::simple(self.common_types.u64), /*index*/
                    MonoType::Variable(TypeVariable(istr("TPtr"))), /*item to write*/
                ],
                return_type: MonoType::simple(void_type),
                variadic: Variadic(false),
            },
        );
        self.add_function_to_type(
            ptr_type,
            istr("read"),
            FunctionSignature {
                type_parameters: vec![TypeParameter(istr("TPtr"))],
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![
                            MonoType::Application(
                                ptr_type,
                                vec![MonoType::Variable(TypeVariable(istr("TPtr")))],
                            ), //self
                        ],
                    ),
                    MonoType::simple(self.common_types.u64),
                ], /* offset * sizeof(TPtr) */
                return_type: MonoType::Variable(TypeVariable(istr("TPtr"))),
                variadic: Variadic(false),
            },
        );

        self.add_function_to_type(
            ptr_type,
            istr("read_i32"),
            FunctionSignature {
                type_parameters: vec![TypeParameter(istr("TPtr"))],
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![MonoType::Variable(TypeVariable(istr("TPtr")))],
                    ), //self
                    MonoType::simple(self.common_types.i32),
                ], /* offset * sizeof(TPtr) */
                return_type: MonoType::Variable(TypeVariable(istr("TPtr"))),
                variadic: Variadic(false),
            },
        );
        self.add_function_to_type(
            ptr_type,
            istr("__index_ptr__"),
            FunctionSignature {
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![
                            MonoType::Application(
                                ptr_type,
                                vec![MonoType::Variable(TypeVariable(istr("TPtr")))],
                            ), //self
                        ],
                    ),
                    MonoType::simple(self.common_types.u64),
                ], /* offset * sizeof(TPtr) */
                return_type: MonoType::Application(
                    ptr_type,
                    vec![MonoType::Variable(TypeVariable(istr("TPtr")))],
                ),
                variadic: Variadic(false),
                type_parameters: vec![TypeParameter(istr("TPtr"))],
            },
        );
        self.mark_as_intrisic(ptr_type);
        self.common_types.ptr = ptr_type;

        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            istr("array"),
            vec![TypeParameter(istr("TItem"))],
        );

        self.add_function_to_type(
            arr_type,
            istr("__index_ptr__"),
            FunctionSignature {
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![
                            MonoType::Application(
                                arr_type,
                                vec![MonoType::Variable(TypeVariable(istr("TItem")))],
                            ), //self
                        ],
                    ),
                    MonoType::simple(u32_type),
                ],
                return_type: MonoType::Application(
                    ptr_type,
                    vec![MonoType::Variable(TypeVariable(istr("TItem")))],
                ),
                variadic: Variadic(false),
                type_parameters: vec![TypeParameter(istr("TItem"))],
            },
        );

        self.add_function_to_type(
            arr_type,
            istr("len"),
            FunctionSignature {
                parameters: vec![
                    MonoType::Application(
                        ptr_type,
                        vec![
                            MonoType::Application(
                                arr_type,
                                vec![MonoType::Variable(TypeVariable(istr("TItem")))],
                            ), //self
                        ],
                    ),
                    MonoType::simple(u32_type),
                ],
                return_type: MonoType::Application(u32_type, vec![]),
                variadic: Variadic(false),
                type_parameters: vec![TypeParameter(istr("TItem"))],
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

use std::{
    fmt::Display,
    iter::Sum,
    ops::{Add, AddAssign, Shl, Sub, SubAssign},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct Bytes(pub u32);

impl Display for Bytes {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<u32> for Bytes {
    #[inline(always)]
    fn from(value: u32) -> Self {
        Bytes(value)
    }
}

impl Shl<u32> for Bytes {
    type Output = Bytes;

    fn shl(self, rhs: u32) -> Self::Output {
        Bytes(self.0 << rhs)
    }
}

impl Shl<i32> for Bytes {
    type Output = Bytes;

    fn shl(self, rhs: i32) -> Self::Output {
        Bytes(self.0 << rhs)
    }
}

impl TryInto<u8> for Bytes {
    type Error = <u32 as TryInto<u8>>::Error;

    fn try_into(self) -> Result<u8, Self::Error> {
        self.0.try_into()
    }
}

impl From<Bytes> for u32 {
    #[inline(always)]
    fn from(val: Bytes) -> Self {
        val.0
    }
}

impl AsRef<u32> for Bytes {
    fn as_ref(&self) -> &u32 {
        &self.0
    }
}

impl PartialEq<i32> for Bytes {
    fn eq(&self, other: &i32) -> bool {
        self.0 as i32 == *other
    }
}

impl Add<Bytes> for Bytes {
    type Output = Bytes;

    #[inline(always)]
    fn add(self, rhs: Bytes) -> Self::Output {
        Bytes(self.0 + rhs.0)
    }
}

impl Sub<Bytes> for Bytes {
    type Output = Bytes;

    #[inline(always)]
    fn sub(self, rhs: Bytes) -> Self::Output {
        Bytes(self.0 - rhs.0)
    }
}

macro_rules! impl_bytes_traits {
    ($type:ty) => {
        impl Add<$type> for Bytes {
            type Output = Bytes;

            #[inline(always)]
            fn add(self, rhs: $type) -> Self::Output {
                Bytes(self.0 + rhs as u32)
            }
        }

        impl Sub<$type> for Bytes {
            type Output = Bytes;

            #[inline(always)]
            fn sub(self, rhs: $type) -> Self::Output {
                Bytes(self.0 - rhs as u32)
            }
        }

        impl AddAssign<$type> for Bytes {
            #[inline(always)]
            fn add_assign(&mut self, rhs: $type) {
                self.0 += rhs as u32;
            }
        }

        impl SubAssign<$type> for Bytes {
            #[inline(always)]
            fn sub_assign(&mut self, rhs: $type) {
                self.0 -= rhs as u32;
            }
        }

        impl Add<&$type> for Bytes {
            type Output = Bytes;

            #[inline(always)]
            fn add(self, rhs: &$type) -> Self::Output {
                Bytes(self.0 + *rhs as u32)
            }
        }

        impl Sub<&$type> for Bytes {
            type Output = Bytes;

            #[inline(always)]
            fn sub(self, rhs: &$type) -> Self::Output {
                Bytes(self.0 - *rhs as u32)
            }
        }

        impl AddAssign<&$type> for Bytes {
            #[inline(always)]
            fn add_assign(&mut self, rhs: &$type) {
                self.0 += *rhs as u32;
            }
        }

        impl SubAssign<&$type> for Bytes {
            #[inline(always)]
            fn sub_assign(&mut self, rhs: &$type) {
                self.0 -= *rhs as u32;
            }
        }
    };
}

impl_bytes_traits!(i32);
impl_bytes_traits!(i64);
impl_bytes_traits!(u32);
impl_bytes_traits!(u64);
impl_bytes_traits!(u8);
impl_bytes_traits!(i8);
impl_bytes_traits!(i128);

impl AddAssign<Bytes> for Bytes {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Bytes) {
        self.0 += rhs.0;
    }
}

impl SubAssign<Bytes> for Bytes {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Bytes) {
        self.0 -= rhs.0;
    }
}

impl Sum<Bytes> for Bytes {
    #[inline(always)]
    fn sum<I: Iterator<Item = Bytes>>(iter: I) -> Self {
        let mut sum = Bytes(0);
        for b in iter {
            sum += b;
        }
        sum
    }
}

impl Bytes {
    #[inline(always)]
    pub const fn size_of<T>() -> Bytes {
        Bytes(std::mem::size_of::<T>() as u32)
    }
}
