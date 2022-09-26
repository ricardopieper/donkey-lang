use std::collections::HashMap;

use crate::ast::lexer::Operator;

use super::type_constructor_db::{TypeConstructorDatabase, TypeConstructorId, TypeConstructor, TypeUsage, TypeKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstructionError {
    TypeNotFound { name: String },
    GenericArgumentNotFound { name: String },
    IncorrectNumberOfArgs{ expected: usize, received: usize},
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeInstanceId(pub usize);

impl TypeInstanceId {
    pub fn as_string(&self, type_db: &TypeInstanceManager) -> String {
        type_db.get_instance(*self).name.to_string()
    }

    pub fn size(&self, type_db: &TypeInstanceManager) -> usize {
        type_db.get_instance(*self).size
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
    pub size: usize,
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
    pub type_args: Vec<TypeInstanceId>
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
    pub bool: TypeInstanceId,
    pub string: TypeInstanceId,
}


pub struct TypeInstanceManager {
    pub constructors: TypeConstructorDatabase,
    pub types: Vec<TypeInstance>,
    pub common_types: CommonTypeInstances,
    pub types_by_constructor_id: std::collections::HashMap<TypeConstructorId, TypeInstanceId>
}

impl TypeInstanceManager {
    pub fn new() -> TypeInstanceManager {
        let mut item = TypeInstanceManager {
            types: vec![],
            constructors: TypeConstructorDatabase::new(),
            common_types: CommonTypeInstances {
                ..Default::default()
            },
            types_by_constructor_id: std::collections::HashMap::new()
        };
        item.init_builtin();
        item
    }

    pub fn is_function(&self, id: TypeInstanceId) -> bool {
        self.types[id.0].is_function
    }

    pub fn get_instance(&self, id: TypeInstanceId) -> &TypeInstance {
        &self.types[id.0]
    }

    pub fn construct_function(&mut self, arg_types: &[TypeInstanceId], return_type: TypeInstanceId) -> TypeInstanceId {
        self.construct_function_method(None, arg_types, return_type)
    }

    pub fn construct_method(&mut self, method_of: TypeInstanceId, arg_types: &[TypeInstanceId], return_type: TypeInstanceId) -> TypeInstanceId {
        self.construct_function_method(Some(method_of), arg_types, return_type)
    }

    fn construct_function_method(&mut self, method_of: Option<TypeInstanceId>, arg_types: &[TypeInstanceId], return_type: TypeInstanceId) -> TypeInstanceId {
        
        for instance in self.types.iter() {
            if !instance.is_function {continue;};
            if instance.function_args == arg_types && instance.function_return_type.unwrap() == return_type && method_of == instance.is_method_of {
                return instance.id
            }
        }

        let new_type_id = TypeInstanceId(self.types.len());
        let constructed = TypeInstance {
            id: new_type_id,
            base: self.constructors.common_types.function,
            is_function: true,
            size: self.constructors.find(self.constructors.common_types.function).rep_size.unwrap(),
            name: "".to_string(), //@TODO build name
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

    pub fn construct_usage(&mut self, usage: &TypeUsage) -> Result<TypeInstanceId, TypeConstructionError> {
        self.construct_usage_generic(usage, &HashMap::new())
    }

    pub fn construct_usage_generic(&mut self, usage: &TypeUsage, type_args: &HashMap<String, TypeInstanceId>) -> Result<TypeInstanceId, TypeConstructionError> {
        match usage {
            TypeUsage::Given(constructor_id) => {
                self.construct_type(*constructor_id, &[])
            },
            TypeUsage::Generic(param) => {
                type_args.get(&param.0).ok_or_else(|| TypeConstructionError::TypeNotFound { name: param.0.to_string()})
                    .map(|op| *op)
            }
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
        positional_args: &[TypeInstanceId]
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        let c = self.constructors.find(constructor_id);

        for existing_type in self.types.iter() {
            if existing_type.base == constructor_id &&
                existing_type.type_args == positional_args {
                    return Ok(existing_type.id);
                }
        }

        let id = TypeInstanceId(self.types.len());
        self.types.push(TypeInstance {
            id,
            base: constructor_id,
            size: 0,
            name: c.name.to_string(),
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
        });
       
        //build a map of argname => type id
        let mut type_args = HashMap::new();
        {
            let constructor = self.constructors.find(constructor_id);
            if constructor.type_args.len() != positional_args.len() {
                return Err(TypeConstructionError::IncorrectNumberOfArgs { expected: constructor.type_args.len(), received: positional_args.len() })
            }

            for (index, type_arg) in constructor.type_args.iter().enumerate() {
                type_args.insert(type_arg.0.to_string(), positional_args[index]);
            }
        }

        {
            let constructor = self.constructors.find(constructor_id);
            if constructor.type_args.len() == 0 {
                self.types_by_constructor_id.insert(constructor.id, id);
            }
        }

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

        let rhs_binary_ops = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let rhs_bin_ops = constructor.rhs_binary_ops.clone();
            for (operator, rhs, op_result) in rhs_bin_ops.iter() {
                result.push((
                    *operator,
                    self.construct_usage(rhs)?,
                    self.construct_usage(op_result)?,
                ));
            }
            result
        };

        let unary_ops = {
            let constructor = self.constructors.find(constructor_id);

            let mut result = vec![];
            let unary_ops = constructor.unary_ops.clone();
            for (operator, rhs) in unary_ops.iter() {
                result.push((
                    *operator,
                    self.construct_usage(rhs)?
                ));
            }
            result
        };

        let fields = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let fields = constructor.fields.clone();
            for field_decl in fields.iter() {
                result.push(TypeInstanceStructField {
                    name: field_decl.name.to_string(),
                    field_type: self.construct_usage(&field_decl.field_type)?
                });
            }
            result
        };

        let methods = {
            let constructor = self.constructors.find(constructor_id);

            let mut result = vec![];
            let methods = constructor.methods.clone();
            for method_decl in methods {
                let args = {
                    let mut args = vec![];
                    for arg in method_decl.args.iter() {
                        let arg_constructed = self.construct_usage(arg)?;
                        args.push(arg_constructed);
                    }
                    args
                };
                
                let return_type = self.construct_usage_generic(&method_decl.return_type, &type_args)?;

                let method_type_id = self.construct_method(id, &args, return_type);

                result.push(TypeInstanceStructMethod {
                    name: method_decl.name.to_string(),
                    function_type: method_type_id
                });
            }
            result
        };

        let name = if positional_args.len() == 0 {
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
        return Ok(id);
    }


    fn try_find_constructor_by_name(&mut self, name: &str) -> Result<&TypeConstructor, TypeConstructionError> {
        let base_constructor = self.constructors.find_by_name(name);
        if let Some(c) = base_constructor {
            return Ok(c);
        }
        Err(TypeConstructionError::TypeNotFound { name: name.to_string() })
    }
    
    fn compute_size(&self, constructed: &TypeInstance) -> usize {
        //compute size of fields if they are struct, if they are primitive then the type constructor already has this info
        let constructor = self.constructors.find(constructed.base);
        if constructor.kind == TypeKind::Primitive {
            constructor.rep_size.expect("Primitive type should have rep_size")
        } else {
            constructed.fields.iter()
                .map(|x| {
                    self.get_instance(x.field_type).size
                })
                .sum()
        }
    }

    fn init_builtin(&mut self) {
        macro_rules! make_type {
            ($type:tt) => {
                self.common_types.$type =
                    self.construct_usage(&TypeUsage::Given(self.constructors.common_types.$type)).unwrap();
            };
        }

        make_type!(i32);
        make_type!(void);
        make_type!(bool);
        make_type!(string);
        make_type!(i64);
        make_type!(u32);
        make_type!(u64);
        make_type!(f32);
        make_type!(f64);
    }
}

