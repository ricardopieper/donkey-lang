use std::collections::HashMap;

use crate::ast::lexer::Operator;

use either::Either;

#[derive(Debug, Clone, PartialEq, Eq)]
/*Represents a fully resolved type, with generics already substituted */
pub enum TypeInstance {
    Simple(TypeId),                     //Built-in types, non-generic structs, etc
    Generic(TypeId, Vec<TypeInstance>), //each TypeId in the vec is a type parameter used in this specific usage of the type, this is positional.
    //parameters, return type
    Function(Vec<TypeInstance>, Box<TypeInstance>), //In this case there is not even a base type like in generics, functions are functions
}

impl TypeInstance {
    pub fn expect_simple(&self) -> TypeId {
        let TypeInstance::Simple(id) = self else {
            panic!("Not a simple type");
        };
        *id
    }

    fn compute_size(&self, type_db: &TypeDatabase) -> usize {
        match self {
            TypeInstance::Simple(s) => {
                let type_record = type_db.find(*s);
                if let Some(size) = type_record.rep_size {
                    return size;
                }
                //this is a struct with no generic args, sum the size of the fields
                let mut size = 0usize;
                for field in type_record.fields.iter() {
                    let type_resolved = field.field_type.create_instance(type_db, None);
                    let field_size = type_resolved.size(type_db);
                    size += field_size;
                }
                return size;
                
            },
            TypeInstance::Generic(id, params) => {
                let type_record = type_db.find(*id);
                if let Some(size) = type_record.rep_size {
                    return size;
                }
                let positional_type_args = &type_record.type_args;

                if positional_type_args.len() != params.len() {
                    panic!("Type {typename} has {num_type_args} type arguments, but {num_passed_args} were passed", 
                        typename = type_record.name,
                        num_type_args = positional_type_args.len(),
                        num_passed_args = params.len())
                }

                let mut type_substitution = std::collections::HashMap::new();

                for (name, type_instance) in positional_type_args.iter().zip(params) {
                    type_substitution.insert(name.0.to_string(), type_instance.clone());
                }

                //this is a struct with no generic args, sum the size of the fields
                let mut size = 0usize;
                for field in type_record.fields.iter() {
                    let type_resolved = field.field_type.create_instance(type_db, Some(&type_substitution));
                    let field_size = type_resolved.size(type_db);
                    size += field_size;
                }
                return size;

            },
            TypeInstance::Function(_, _) => std::mem::size_of::<u32>(), //this is a ptr
        }
    }

    pub fn size(&self, type_db: &TypeDatabase) -> usize {
        let computed = self.compute_size(type_db);
        if computed > 255 {
            let name = self.as_string(type_db);
            eprintln!("WARNING: Type {name} has size {computed}, compiler will probably generate bad code because push instructions have a max size of 255");
        }
        return computed;
    }

    pub fn as_string(&self, type_db: &TypeDatabase) -> String {
        match self {
            TypeInstance::Simple(id) => type_db.get_name(*id).into(),
            TypeInstance::Generic(id, args) => {
                let args_str = args
                    .iter()
                    .map(|x| x.as_string(type_db))
                    .collect::<Vec<_>>()
                    .join(", ");
                let base_str = type_db.get_name(*id);
                format!("{}<{}>", base_str, args_str)
            }
            TypeInstance::Function(args, return_type) => {
                let args_str = args
                    .iter()
                    .map(|x| x.as_string(type_db))
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_type_str = return_type.as_string(type_db);
                format!("fn ({}) -> {}", args_str, return_type_str)
            }
        }
    }

    pub fn is_compatible(&self, other: &TypeInstance, _type_db: &TypeDatabase) -> bool {
        //for now we just compare by equality
        self == other
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(pub usize);

//Types arent simple, generic, function.... but rather primitive, struct and trait.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Primitive,
    Struct,
}

//Whether a type is signed or unsigned
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSign {
    Signed,
    Unsigned,
}

//represents a type on a field declaration, method return type, method args, etc
//this is type information we store during compilation.
//It can be a fully resolved type, trivially convertible to TypeInstance, but in some cases there are generic params.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstructor {
    Simple(Either<GenericParameter, TypeId>),
    Generic(TypeId, Vec<TypeConstructor>), //on generics, the base root type has to be known
    //Function(Vec<Type>, Box<Type>), //on functions, both return or args can use generics
}
/**
 
struct SomeType<T1, T2> {
    field1: T1
    field2: T2
}

struct SomeOtherType {
    field1: SomeType<i32, i32>
}

struct SomeOtherTypeInt<T> {
    field1: SomeType<i32, T>
}

 */
impl TypeConstructor {

     //Expects that all generic parameters are already given
     pub fn create_instance(&self, type_db: &TypeDatabase, type_args_opt: Option<&HashMap<String, TypeInstance>>) -> TypeInstance {
        match self {
            TypeConstructor::Simple(generic_or_id) => match generic_or_id {
                Either::Left(generic) => {
                    let arg = type_args_opt.and_then(|type_args| type_args.get(&generic.0));
                    match arg {
                        Some(found_type) => found_type.clone(),
                        None => panic!("Missing generic argument for {}", generic.0),
                    }
                }
                Either::Right(s) => TypeInstance::Simple(*s),
            },
            TypeConstructor::Generic(s, generic_arguments) => {
                let generics = generic_arguments.iter()
                    .map(|x| x.create_instance(type_db, type_args_opt));
                TypeInstance::Generic(*s, generics.collect())
            },
        }
    }
}

//@TODO must implement a way to perform generic substitution on every type instance...
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub name: String,
    pub type_args: Vec<GenericParameter>,
    pub args: Vec<TypeConstructor>,
    pub return_type: TypeConstructor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeField {
    pub name: String,
    pub field_type: TypeConstructor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParameter(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRecord {
    pub id: TypeId,
    pub kind: TypeKind,
    pub sign: TypeSign,
    //Size of native types, such as numbers, ptrs, etc
    pub rep_size: Option<usize>,
    pub name: String,
    //Type args are just Generic parameters, in the future we can add type bounds, in which we would add a Type enum here as well
    pub type_args: Vec<GenericParameter>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeInstance>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeInstance, TypeInstance)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeInstance)>,
    //fields (name, type)
    pub fields: Vec<TypeField>,
    //method (name, args, return type)
    pub methods: Vec<FunctionSignature>,
}

impl TypeRecord {
    pub fn as_type(&self) -> TypeConstructor {
        if self.type_args.is_empty() {
            TypeConstructor::Simple(Either::Right(self.id))
        } else {
            return TypeConstructor::Generic(
                self.id,
                self.type_args
                    .iter()
                    .map(|x| TypeConstructor::Simple(Either::Left(x.clone())))
                    .collect::<Vec<_>>(),
            );
        }
    }

    pub fn to_instance(&self) -> TypeInstance {
        TypeInstance::Simple(self.id)
    }

    pub fn is_integer(&self, type_db: &TypeDatabase) -> bool {
        let as_instance = self.to_instance();
        as_instance == type_db.special_types.i32
            || as_instance == type_db.special_types.i64
            || as_instance == type_db.special_types.u32
            || as_instance == type_db.special_types.u64
    }

    pub fn is_float(&self, type_db: &TypeDatabase) -> bool {
        let as_instance = self.to_instance();
        as_instance == type_db.special_types.f32 || as_instance == type_db.special_types.f64
    }
}

impl Default for TypeRecord {
    fn default() -> TypeRecord {
        TypeRecord {
            id: TypeId(0),
            kind: TypeKind::Primitive,
            sign: TypeSign::Unsigned,
            rep_size: None,
            name: String::new(),
            allowed_casts: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            fields: vec![],
            methods: vec![],
            type_args: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpecialTypes {
    pub void: TypeInstance,
    pub i32: TypeInstance,
    pub u32: TypeInstance,
    pub i64: TypeInstance,
    pub u64: TypeInstance,
    pub f32: TypeInstance,
    pub f64: TypeInstance,
    pub bool: TypeInstance,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericTypeInstance {
    pub base_type: TypeId,
    pub rep_size: usize,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeInstance>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeInstance, TypeInstance)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeInstance)>,
    //fields (name, type)
    pub fields: Vec<TypeField>,
    //method (name, args, return type)
    pub methods: Vec<FunctionSignature>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDatabase {
    pub types: Vec<TypeRecord>,
    pub special_types: SpecialTypes
}

impl TypeDatabase {
    pub fn new() -> Self {
        let mut item = Self {
            types: vec![],
            special_types: SpecialTypes {
                void: TypeInstance::Simple(TypeId(0)),
                i32: TypeInstance::Simple(TypeId(0)),
                i64: TypeInstance::Simple(TypeId(0)),
                u32: TypeInstance::Simple(TypeId(0)),
                u64: TypeInstance::Simple(TypeId(0)),
                bool: TypeInstance::Simple(TypeId(0)),
                f32: TypeInstance::Simple(TypeId(0)),
                f64: TypeInstance::Simple(TypeId(0)),
            },
        };
        item.init_builtin();
        item
    }

    pub fn add(&mut self, kind: TypeKind, sign: TypeSign, name: &str, rep_size: Option<usize>) -> TypeId {
        let next_id = TypeId(self.types.len());
        self.types.push(TypeRecord {
            id: next_id,
            kind,
            name: name.into(),
            rep_size,
            sign,
            ..TypeRecord::default()
        });
        next_id
    }

    pub fn add_binary_operator(
        &mut self,
        type_id: TypeId,
        operator: Operator,
        rhs_type: TypeInstance,
        result_type: TypeInstance,
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
        rep_size: Option<usize>,
    ) -> TypeId {
        let next_id = TypeId(self.types.len());

        self.types.push(TypeRecord {
            id: next_id,
            kind,
            name: name.into(),
            rep_size,
            type_args,
            ..TypeRecord::default()
        });
        next_id
    }

    pub fn add_unary_operator(
        &mut self,
        type_id: TypeId,
        operator: Operator,
        result_type: TypeInstance,
    ) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.unary_ops.push((operator, result_type));
    }

    pub fn get_name(&self, id: TypeId) -> &str {
        &self
            .types
            .get(id.0)
            .unwrap_or_else(|| panic!("Type ID not found: {}", id.0))
            .name
    }

    pub fn find(&self, id: TypeId) -> &TypeRecord {
        self.types
            .get(id.0)
            .unwrap_or_else(|| panic!("Type ID not found: {}", id.0))
    }

    pub fn find_by_name(&self, name: &str) -> Option<&TypeRecord> {
        self.types.iter().find(|t| t.name == name)
    }

    pub fn expect_find_by_name(&self, name: &str) -> &TypeRecord {
        match self.types.iter().find(|t| t.name == name) {
            Some(r) => r,
            None => panic!("Could not find type by name {}", name),
        }
    }

    pub fn get_binary_operations(
        &self,
        type_instance: &TypeInstance,
    ) -> &[(Operator, TypeInstance, TypeInstance)] {
        match type_instance {
            TypeInstance::Simple(id) => &self.find(*id).rhs_binary_ops,
            TypeInstance::Generic(_, _) => {
                //@TODO basically we need to check the base type of the type instance,
                //then make a big list of all operations that it supports
                //maybe we will need to return an owned type instead of a reference...
                panic!("Unimplemented, read comment here to see what needs to be done")
            }
            TypeInstance::Function(_, _) => {
                panic!("Binary operations on functions are not supported")
            }
        }
    }

    pub fn get_unary_operations(
        &self,
        type_instance: &TypeInstance,
    ) -> &[(Operator, TypeInstance)] {
        match type_instance {
            TypeInstance::Simple(id) => &self.find(*id).unary_ops,
            TypeInstance::Generic(_, _) => {
                //@TODO basically we need to check the base type of the type instance,
                //then make a big list of all operations that it supports
                //maybe we will need to return an owned type instead of a reference...
                panic!("Unimplemented, read comment here to see what needs to be done")
            }
            TypeInstance::Function(_, _) => {
                panic!("Unary operations on functions are not supported")
            }
        }
    }

    fn register_primitive_number(&mut self, name: &str, size: usize, sign: TypeSign) -> TypeId {
        let type_id = self.add(TypeKind::Primitive, sign, name, Some(size));
        self.add_binary_operator(
            type_id,
            Operator::Plus,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Multiply,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Minus,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Divide,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(type_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::Mod,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(type_id),
        );

        let bool_id = self.find_by_name("bool").unwrap().id;
        self.add_binary_operator(
            type_id,
            Operator::Equals,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(bool_id),
        );
        self.add_binary_operator(
            type_id,
            Operator::NotEquals,
            TypeInstance::Simple(type_id),
            TypeInstance::Simple(bool_id),
        );

        self.add_unary_operator(type_id, Operator::Plus, TypeInstance::Simple(type_id));
        self.add_unary_operator(type_id, Operator::Minus, TypeInstance::Simple(type_id));

        type_id
    }

    pub fn add_method(&mut self, type_id: TypeId, signature: FunctionSignature) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.methods.push(signature);
    }

    /*pub fn add_field(&mut self, type_id: TypeId, name: &str, field_type: TypeConstructor) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeField {
            name: name.to_string(),
            field_type,
        });
    }*/

    pub fn add_simple_field(&mut self, type_id: TypeId, name: &str, field_type: TypeId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeField {
            name: name.to_string(),
            field_type: TypeConstructor::Simple(Either::Right(field_type)),
        });
    }
    
    #[allow(clippy::similar_names)]
    fn init_builtin(&mut self) {
        use std::mem;

        let void_type = self.add(
            TypeKind::Primitive,
            TypeSign::Unsigned,
            "Void",
            Some(mem::size_of::<()>()),
        );
        self.special_types.void = TypeInstance::Simple(void_type);

        self.add(
            TypeKind::Primitive,
            TypeSign::Unsigned,
            "None",
            Some(mem::size_of::<()>()),
        );
        self.special_types.bool = TypeInstance::Simple(self.add(
            TypeKind::Primitive,
            TypeSign::Unsigned,
            "bool",
            Some(mem::size_of::<bool>()),
        ));

        let i32_type =
            self.register_primitive_number("i32", mem::size_of::<i32>(), TypeSign::Signed);
        let u32_type =
            self.register_primitive_number("u32", mem::size_of::<u32>(), TypeSign::Unsigned);
        self.special_types.i32 = TypeInstance::Simple(i32_type);
        self.special_types.u32 = TypeInstance::Simple(u32_type);

        self.special_types.i64 = TypeInstance::Simple(self.register_primitive_number(
            "i64",
            mem::size_of::<i64>(),
            TypeSign::Signed,
        ));
        self.special_types.u64 = TypeInstance::Simple(self.register_primitive_number(
            "u64",
            mem::size_of::<u64>(),
            TypeSign::Unsigned,
        ));
        self.special_types.f32 = TypeInstance::Simple(self.register_primitive_number(
            "f32",
            mem::size_of::<f32>(),
            TypeSign::Signed,
        ));
        self.special_types.f64 = TypeInstance::Simple(self.register_primitive_number(
            "f64",
            mem::size_of::<f64>(),
            TypeSign::Signed,
        ));

        //internal type for pointers, ptr<i32> points to a buffer of i32, and so on
        let ptr_type = self.add_generic(
            TypeKind::Primitive,
            "ptr",
            vec![GenericParameter("TPtr".into())],
            Some(mem::size_of::<usize>()),
        );

        //ptr + len
        let str_type = self.add(
            TypeKind::Struct,
            TypeSign::Unsigned,
            "str",
            None,
        );
 
        self.add_simple_field(str_type, "ptr", ptr_type);
        self.add_simple_field(str_type, "len", u32_type);


        self.add_method(
            str_type,
            FunctionSignature {
                name: "as_i32".to_string(),
                type_args: vec![],
                args: vec![TypeConstructor::Simple(Either::Right(str_type))],
                return_type: TypeConstructor::Simple(Either::Right(i32_type)),
            },
        );

        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            "array",
            vec![GenericParameter("TItem".into())],
            Some(mem::size_of::<usize>() + mem::size_of::<u32>()),
        );

        self.add_method(
            arr_type,
            FunctionSignature {
                name: "__index__".to_string(),
                type_args: vec![],
                args: vec![TypeConstructor::Simple(Either::Right(u32_type))],
                return_type: TypeConstructor::Simple(Either::Left(GenericParameter("TItem".into()))),
            },
        );

        //u32_type
        self.add_simple_field(arr_type, "length", u32_type);
    }
}
