use crate::ast::lexer::Operator;
use crate::semantic::hir::*;
use either::*;

use std::{
    any::Any,
    collections::{HashMap, HashSet},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(pub usize);

//Types arent simple, generic, function.... but rather primitive, struct and trait.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Primitive,
    Struct,
}

//represents a type on a field declaration, method return type, method args, etc
//this is type information we store during compilation.
//It can be a fully resolved type, trivially convertible to TypeInstance, but in some cases there are generic params.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Simple(Either<GenericParameter, TypeId>),
    Generic(TypeId, Vec<Type>), //on generics, the base root type has to be known
    Function(Vec<Type>, Box<Type>), //on functions, both return or args can use generics
}

//@TODO must implement a way to perform generic substitution on every type instance...
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub name: String,
    pub type_args: Vec<GenericParameter>,
    pub args: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeField {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParameter(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRecord {
    pub id: TypeId,
    pub kind: TypeKind,
    pub size: usize,
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
    pub fn as_type(&self) -> Type {
        if self.type_args.len() == 0 {
            return Type::Simple(Either::Right(self.id));
        } else {
            return Type::Generic(
                self.id,
                self.type_args
                    .iter()
                    .map(|x| Type::Simple(Either::Left(x.clone())))
                    .collect::<Vec<_>>(),
            );
        }
    }

    pub fn to_instance(&self) -> TypeInstance {
        TypeInstance::Simple(self.id)
    }
}

impl Default for TypeRecord {
    fn default() -> TypeRecord {
        TypeRecord {
            id: TypeId(0),
            kind: TypeKind::Primitive,
            size: 0,
            name: "".into(),
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
    pub void: TypeInstance
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDatabase {
    pub types: Vec<TypeRecord>,
    pub special_types: SpecialTypes
}

impl TypeDatabase {
    pub fn new() -> Self {
        let mut item = Self { types: vec![], special_types: SpecialTypes { void: TypeInstance::Simple(TypeId(0)) } };
        item.init_builtin();
        return item;
    }

    pub fn add(&mut self, kind: TypeKind, name: &str, size: usize) -> TypeId {
        let next_id = TypeId(self.types.len());
        self.types.push(TypeRecord {
            id: next_id,
            kind: kind,
            name: name.into(),
            size,
            ..TypeRecord::default()
        });
        return next_id;
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
        size: usize,
    ) -> TypeId {
        let next_id = TypeId(self.types.len());

        self.types.push(TypeRecord {
            id: next_id,
            kind: kind,
            name: name.into(),
            size,
            type_args,
            ..TypeRecord::default()
        });
        return next_id;
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
            .expect(&format!("Type ID not found: {}", id.0))
            .name
    }

    pub fn find(&self, id: TypeId) -> &TypeRecord {
        self.types
            .get(id.0)
            .expect(&format!("Type ID not found: {}", id.0))
    }

    fn find_by_name(&self, name: &str) -> Option<&TypeRecord> {
        self.types.iter().find(|t| t.name == name)
    }

    pub fn expect_find_by_name(&self, name: &str) -> &TypeRecord {
        match self.types.iter().find(|t| t.name == name){
            Some(r) => r,
            None => panic!("Could not find type by name {}", name)
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

    fn register_primitive_number(&mut self, name: &str) -> TypeId {
        use std::mem;
        let type_id = self.add(TypeKind::Primitive, name, mem::size_of::<i32>());
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

        return type_id;
    }

    pub fn add_method(&mut self, type_id: TypeId, signature: FunctionSignature) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.methods.push(signature)
    }

    pub fn add_field(&mut self, type_id: TypeId, name: &str, field_type: TypeId) {
        let record = self.types.get_mut(type_id.0).unwrap();
        record.fields.push(TypeField { name: name.to_string(), field_type: Type::Simple(Either::Right(field_type)) })
    }

    fn init_builtin(&mut self) {
        use std::mem;

        let void_type = self.add(TypeKind::Primitive, "Void", mem::size_of::<()>());
        self.special_types.void = TypeInstance::Simple(void_type);

        self.add(TypeKind::Primitive, "None", mem::size_of::<()>());
        self.add(TypeKind::Primitive, "bool", mem::size_of::<bool>());

        let i32_type = self.register_primitive_number("i32");
        let u32_type = self.register_primitive_number("u32");
        self.register_primitive_number("i64");
        self.register_primitive_number("u64");
        self.register_primitive_number("f32");
        self.register_primitive_number("f64");

        //internal type for pointers, ptr<i32> points to a buffer of i32, and so on
        self.add_generic(
            TypeKind::Primitive,
            "ptr",
            vec![GenericParameter("TPtr".into())],
            mem::size_of::<usize>(),
        );

        //ptr + len
        let str_type = self.add(
            TypeKind::Struct,
            "str",
            mem::size_of::<usize>() + mem::size_of::<i32>(),
        );
        self.add_method(
            str_type,
            FunctionSignature {
                name: "as_i32".to_string(),
                type_args: vec![],
                args: vec![Type::Simple(Either::Right(str_type))],
                return_type: Type::Simple(Either::Right(i32_type)),
            },
        );

        //ptr + num items
        let arr_type = self.add_generic(
            TypeKind::Struct,
            "array",
            vec![GenericParameter("TItem".into())],
            mem::size_of::<usize>() + mem::size_of::<u32>(),
        );

        self.add_method(
            arr_type,
            FunctionSignature {
                name: "__index__".to_string(),
                type_args: vec![],
                args: vec![Type::Simple(Either::Right(u32_type))],
                return_type: Type::Simple(Either::Left(GenericParameter("TItem".into()))),
            },
        );

        //u32_type
        self.add_field(arr_type, "length", u32_type);
    }
}
