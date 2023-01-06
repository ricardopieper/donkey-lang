use std::collections::HashMap;

use crate::{ast::lexer::Operator, compiler::layouts::Bytes};

use super::type_constructor_db::{
    TypeConstructor, TypeConstructorDatabase, TypeConstructorId, TypeKind, TypeUsage,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstructionError {
    TypeNotFound { name: String },
    //GenericArgumentNotFound { name: String },
    IncorrectNumberOfArgs { expected: usize, received: usize },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeInstanceId(pub usize);

impl TypeInstanceId {
    pub fn as_string(self, type_db: &TypeInstanceManager) -> String {
        type_db.get_instance(self).name.to_string()
    }

    pub fn size(self, type_db: &TypeInstanceManager) -> Bytes {
        type_db.get_instance(self).size
    }

    pub fn is_integer(self, type_db: &TypeInstanceManager) -> bool {
        let types = &type_db.common_types;
        self == types.i32 || self == types.i64 || self == types.u32 || self == types.u64
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
    pub name: String,
    pub function_type: TypeInstanceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstanceStructField {
    pub name: String,
    pub field_type: TypeInstanceId,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeInstance {
    pub id: TypeInstanceId,
    pub base: TypeConstructorId,
    pub size: Bytes,
    pub name: String,
    pub is_function: bool,
    //only valid if this is a function
    pub function_args: Vec<TypeInstanceId>,
    //only valid if this is a function
    pub function_return_type: Option<TypeInstanceId>,
    pub is_method_of: Option<TypeInstanceId>,
    //these fields inform type capabilities, i.e. operators it can deal with, fields and functions it has if its a struct, types it can be casted to, etc
    pub allowed_casts: Vec<TypeInstanceId>,
    //operator, rhs_type, result_type, for now cannot be generic
    pub rhs_binary_ops: Vec<(Operator, TypeInstanceId, TypeInstanceId)>,
    //operator, result_type, for now cannot be generic
    pub unary_ops: Vec<(Operator, TypeInstanceId)>,
    //fields (name, type)
    pub fields: Vec<TypeInstanceStructField>,
    //method (name, args, return type)
    pub methods: Vec<TypeInstanceStructMethod>,
    //The arguments used in the type constructor
    pub type_args: Vec<TypeInstanceId>,
}

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
    pub bool: TypeInstanceId,
}

pub struct TypeInstanceManager {
    pub constructors: TypeConstructorDatabase,
    pub types: Vec<TypeInstance>,
    pub common_types: CommonTypeInstances,
}

pub enum StructMember<'type_db> {
    Field(&'type_db TypeInstanceStructField, usize),
    Method(&'type_db TypeInstanceStructMethod),
    NotFound,
}

impl TypeInstanceManager {
    pub fn new() -> TypeInstanceManager {
        let mut item = TypeInstanceManager {
            types: vec![],
            constructors: TypeConstructorDatabase::new(),
            common_types: CommonTypeInstances {
                ..Default::default()
            },
        };
        item.init_builtin();
        item
    }

    pub fn find_by_name(&self, name: &str) -> Option<&TypeInstance> {
        self.types.iter().find(|t| t.name == name)
    }

    pub fn get_instance(&self, id: TypeInstanceId) -> &TypeInstance {
        &self.types[id.0]
    }

    pub fn find_struct_member<'this>(
        &'this self,
        id: TypeInstanceId,
        name: &str,
    ) -> StructMember<'this> {
        let instance = self.get_instance(id);
        if let Some((index, field)) = instance
            .fields
            .iter()
            .enumerate()
            .find(|x| x.1.name == name)
        {
            return StructMember::Field(field, index);
        }
        if let Some(method) = instance.methods.iter().find(|x| x.name == name) {
            return StructMember::Method(method);
        }
        StructMember::NotFound
    }

    pub fn construct_function(
        &mut self,
        arg_types: &[TypeInstanceId],
        return_type: TypeInstanceId,
    ) -> TypeInstanceId {
        self.construct_function_method(None, arg_types, return_type)
    }

    pub fn construct_method(
        &mut self,
        method_of: TypeInstanceId,
        arg_types: &[TypeInstanceId],
        return_type: TypeInstanceId,
    ) -> TypeInstanceId {
        self.construct_function_method(Some(method_of), arg_types, return_type)
    }

    fn construct_function_method(
        &mut self,
        method_of: Option<TypeInstanceId>,
        arg_types: &[TypeInstanceId],
        return_type: TypeInstanceId,
    ) -> TypeInstanceId {
        for instance in &self.types {
            if !instance.is_function {
                continue;
            };
            if instance.function_args == arg_types
                && instance.function_return_type.unwrap() == return_type
                && method_of == instance.is_method_of
            {
                return instance.id;
            }
        }

        let function_type = self
            .constructors
            .find(self.constructors.common_types.function);

        let TypeKind::Primitive { size } = function_type.kind else {
            panic!("Function type should be a primitive type")
        };

        let new_type_id = TypeInstanceId(self.types.len());
        let constructed = TypeInstance {
            id: new_type_id,
            base: self.constructors.common_types.function,
            is_function: true,
            size,
            name: String::new(), //@TODO build name
            function_args: arg_types.to_vec(),
            function_return_type: Some(return_type),
            allowed_casts: vec![],
            rhs_binary_ops: vec![],
            unary_ops: vec![],
            fields: vec![],
            methods: vec![],
            type_args: vec![],
            is_method_of: method_of,
        };
        self.types.push(constructed);

        let name = {
            let args = arg_types
                .iter()
                .map(|x| self.get_instance(*x).name.clone())
                .collect::<Vec<_>>()
                .join(", ");
            let return_name = &self.get_instance(return_type).name;
            format!("fn ({args}) -> {return_name}")
        };
        self.types[new_type_id.0].name = name;

        new_type_id
    }

    pub fn construct_usage(
        &mut self,
        usage: &TypeUsage,
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        self.construct_usage_generic(usage, &HashMap::new())
    }

    pub fn construct_usage_generic(
        &mut self,
        usage: &TypeUsage,
        type_args: &HashMap<String, TypeInstanceId>,
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        match usage {
            TypeUsage::Given(constructor_id) => self.construct_type(*constructor_id, &[]),
            TypeUsage::Generic(param) => type_args
                .get(&param.0)
                .ok_or_else(|| TypeConstructionError::TypeNotFound {
                    name: param.0.to_string(),
                })
                .map(|op| *op),
            TypeUsage::Parameterized(constructor_id, params) => {
                let mut constructed = vec![];

                for item in params.iter() {
                    let type_id = self.construct_usage_generic(item, &HashMap::new())?;
                    constructed.push(type_id);
                }
                self.construct_type(*constructor_id, &constructed)
            }
        }
    }

    pub fn construct_type(
        &mut self,
        constructor_id: TypeConstructorId,
        positional_args: &[TypeInstanceId],
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        let c = self.constructors.find(constructor_id);

        for existing_type in &self.types {
            if existing_type.base == constructor_id && existing_type.type_args == positional_args {
                return Ok(existing_type.id);
            }
        }

        let id = TypeInstanceId(self.types.len());
        self.types.push(make_base_instance(id, c, positional_args));

        //build a map of argname => type id
        let mut type_args = HashMap::new();
        {
            let constructor = self.constructors.find(constructor_id);
            if constructor.type_args.len() != positional_args.len() {
                return Err(TypeConstructionError::IncorrectNumberOfArgs {
                    expected: constructor.type_args.len(),
                    received: positional_args.len(),
                });
            }

            for (index, type_arg) in constructor.type_args.iter().enumerate() {
                type_args.insert(type_arg.0.to_string(), positional_args[index]);
            }
        }

        let allowed_casts = self.make_casts(constructor_id)?;
        let rhs_binary_ops = self.make_binary_ops(constructor_id)?;
        let unary_ops = self.make_unary_ops(constructor_id)?;
        let fields = self.make_fields(constructor_id)?;
        let methods = self.make_methods(constructor_id, &type_args, id)?;

        let name = if positional_args.is_empty() {
            let constructor = self.constructors.find(constructor_id);
            constructor.name.to_string()
        } else {
            let constructor = self.constructors.find(constructor_id);
            let generics = positional_args
                .iter()
                .map(|x| self.get_instance(*x).name.clone())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{base}<{generics}>", base = constructor.name)
        };

        {
            let mut cur = &mut self.types[id.0];
            cur.allowed_casts = allowed_casts;
            cur.rhs_binary_ops = rhs_binary_ops;
            cur.unary_ops = unary_ops;
            cur.fields = fields;
            cur.methods = methods;
            cur.name = name;
        }
        {
            let size = self.compute_size(&self.types[id.0]);
            self.types[id.0].size = size;
        }
        Ok(id)
    }

    fn make_casts(
        &mut self,
        constructor_id: TypeConstructorId,
    ) -> Result<Vec<TypeInstanceId>, TypeConstructionError> {
        let allowed_casts = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let casts = constructor.allowed_casts.clone();
            for type_usage in casts {
                let constructed = self.construct_usage(&type_usage)?;
                result.push(constructed);
            }
            result
        };
        Ok(allowed_casts)
    }

    fn make_binary_ops(
        &mut self,
        constructor_id: TypeConstructorId,
    ) -> Result<Vec<(Operator, TypeInstanceId, TypeInstanceId)>, TypeConstructionError> {
        let rhs_binary_ops = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let rhs_bin_ops = constructor.rhs_binary_ops.clone();
            for (operator, rhs, op_result) in &rhs_bin_ops {
                result.push((
                    *operator,
                    self.construct_usage(rhs)?,
                    self.construct_usage(op_result)?,
                ));
            }
            result
        };
        Ok(rhs_binary_ops)
    }

    fn make_unary_ops(
        &mut self,
        constructor_id: TypeConstructorId,
    ) -> Result<Vec<(Operator, TypeInstanceId)>, TypeConstructionError> {
        let unary_ops = {
            let constructor = self.constructors.find(constructor_id);

            let mut result = vec![];
            let unary_ops = constructor.unary_ops.clone();
            for (operator, rhs) in &unary_ops {
                result.push((*operator, self.construct_usage(rhs)?));
            }
            result
        };
        Ok(unary_ops)
    }

    fn make_fields(
        &mut self,
        constructor_id: TypeConstructorId,
    ) -> Result<Vec<TypeInstanceStructField>, TypeConstructionError> {
        let fields = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let fields = constructor.fields.clone();
            for field_decl in &fields {
                result.push(TypeInstanceStructField {
                    name: field_decl.name.to_string(),
                    field_type: self.construct_usage(&field_decl.field_type)?,
                });
            }
            result
        };
        Ok(fields)
    }

    fn make_methods(
        &mut self,
        constructor_id: TypeConstructorId,
        type_args: &HashMap<String, TypeInstanceId>,
        id: TypeInstanceId,
    ) -> Result<Vec<TypeInstanceStructMethod>, TypeConstructionError> {
        let methods = {
            let constructor = self.constructors.find(constructor_id);

            let mut result = vec![];
            let methods = constructor.methods.clone();
            for method_decl in methods {
                let args = {
                    let mut args = vec![];
                    for arg in &method_decl.args {
                        let arg_constructed = self.construct_usage(arg)?;
                        args.push(arg_constructed);
                    }
                    args
                };

                let return_type =
                    self.construct_usage_generic(&method_decl.return_type, type_args)?;

                let method_type_id = self.construct_method(id, &args, return_type);

                result.push(TypeInstanceStructMethod {
                    name: method_decl.name.to_string(),
                    function_type: method_type_id,
                });
            }
            result
        };
        Ok(methods)
    }

    fn compute_size(&self, constructed: &TypeInstance) -> Bytes {
        //compute size of fields if they are struct, if they are primitive then the type constructor already has this info
        let constructor = self.constructors.find(constructed.base);
        if let TypeKind::Primitive { size } = constructor.kind {
            size
        } else {
            constructed
                .fields
                .iter()
                .map(|x| self.get_instance(x.field_type).size)
                .sum()
        }
    }

    fn init_builtin(&mut self) {
        macro_rules! make_type {
            ($type:tt) => {
                self.common_types.$type = self
                    .construct_usage(&TypeUsage::Given(self.constructors.common_types.$type))
                    .unwrap();
            };
        }

        make_type!(i32);
        make_type!(void);
        make_type!(bool);
        make_type!(i64);
        make_type!(u32);
        make_type!(u64);
        make_type!(f32);
        make_type!(f64);
        make_type!(u8);
    }
}

fn make_base_instance(
    id: TypeInstanceId,
    constructor: &TypeConstructor,
    positional_args: &[TypeInstanceId],
) -> TypeInstance {
    TypeInstance {
        id,
        base: constructor.id,
        size: Bytes(0),
        name: constructor.name.to_string(),
        allowed_casts: vec![],
        rhs_binary_ops: vec![],
        unary_ops: vec![],
        fields: vec![],
        methods: vec![],
        type_args: positional_args.to_vec(),
        is_function: false,
        function_args: vec![],
        function_return_type: None,
        is_method_of: None,
    }
}

impl Default for TypeInstanceManager {
    fn default() -> Self {
        Self::new()
    }
}
