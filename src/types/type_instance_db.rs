use std::collections::HashMap;

use crate::interner::InternedString;
use crate::semantic::type_name_printer::TypeNamePrinter;
use crate::{ast::lexer::Operator, compiler::layouts::Bytes};

use super::type_constructor_db::{
    TypeConstructParams, TypeConstructor, TypeConstructorDatabase,
    TypeConstructorFunctionDeclaration, TypeConstructorId, TypeKind, TypeParameter,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstructionError {
    IncorrectNumberOfArgs { expected: usize, received: usize },
    InsufficientInformation,
}

impl TypeNamePrinter for TypeConstructorId {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        return self.to_string(&type_db.constructors);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeInstanceId(pub usize);

impl TypeInstanceId {
    pub fn to_string(self, type_db: &TypeInstanceManager) -> String {
        type_db.get_instance(self).name.to_string()
    }

    pub fn size(self, type_db: &TypeInstanceManager) -> Bytes {
        type_db.get_instance(self).size
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
    pub id: TypeInstanceId,
    pub base: TypeConstructorId,
    pub size: Bytes,
    pub name: String,
    pub is_function: bool,
    pub is_variadic: bool,
    //only valid if this is a function
    pub function_args: Vec<TypeInstanceId>,
    //only valid if this is a function, and if it's a function this is always Some(...)
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

impl TypeInstance {
    pub fn find_method_by_name(&self, name: InternedString) -> Option<&TypeInstanceStructMethod> {
        self.methods.iter().find(|x| x.name == name)
    }

    pub fn find_field_by_name(&self, name: InternedString) -> Option<&TypeInstanceStructField> {
        self.fields.iter().find(|x| x.name == name)
    }
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
    pub char: TypeInstanceId,
    pub bool: TypeInstanceId,
}

pub enum StructMember<'type_db> {
    Field(&'type_db TypeInstanceStructField, usize),
    Method(&'type_db TypeInstanceStructMethod),
    NotFound,
}

pub struct TypeInstanceManager {
    pub constructors: TypeConstructorDatabase,
    pub types: Vec<TypeInstance>,
    pub common_types: CommonTypeInstances,
}

impl TypeInstanceManager {
    pub fn new() -> TypeInstanceManager {
        let mut item = TypeInstanceManager {
            types: vec![],
            constructors: TypeConstructorDatabase::new(),
            common_types: Default::default(),
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

    pub fn find_struct_member(&self, id: TypeInstanceId, name: InternedString) -> StructMember {
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

    pub fn construct_method(
        &mut self,
        method_of: TypeInstanceId,
        arg_types: &[TypeInstanceId],
        return_type: TypeInstanceId,
        is_variadic: bool,
    ) -> TypeInstanceId {
        self.construct_function_method(Some(method_of), arg_types, return_type, is_variadic)
    }

    fn construct_function_method(
        &mut self,
        method_of: Option<TypeInstanceId>,
        arg_types: &[TypeInstanceId],
        return_type: TypeInstanceId,
        is_variadic: bool,
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
        let new_type_id = TypeInstanceId(self.types.len());
        let constructed = TypeInstance {
            id: new_type_id,
            base: self.constructors.common_types.function,
            is_function: true,
            size: Bytes(0),
            is_variadic,
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
        usage: &TypeConstructParams,
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        self.construct_usage_generic(usage, &vec![], &HashMap::new())
    }

    fn print_type_args_pretty(&self, type_args: &HashMap<TypeParameter, TypeInstanceId>) -> String {
        let mut type_args_str = String::new();
        for (i, (param, type_id)) in type_args.iter().enumerate() {
            if i > 0 {
                type_args_str.push_str(", ");
            }
            type_args_str.push_str(&format!(
                "{}: {}",
                param.0,
                self.get_instance(*type_id).name
            ));
        }
        type_args_str
    }

    pub fn construct_usage_generic(
        &mut self,
        //Function types should pass the generics here too
        usage: &TypeConstructParams,
        positional_args: &[TypeInstanceId],
        type_args: &HashMap<TypeParameter, TypeInstanceId>,
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        log!(
            "construct_usage_generic {} type_args: {}",
            usage.to_string(&self.constructors),
            self.print_type_args_pretty(type_args)
        );
        match usage {
            TypeConstructParams::Generic(param) => {
                let arg = type_args.get(&param);

                match arg {
                    Some(type_id) => Ok(*type_id),
                    None => {
                        let param_str = param.0;
                        log!(
                            "Insufficient information because param {param_str} wasn't found in map"
                        );
                        Err(TypeConstructionError::InsufficientInformation)
                    }
                }
            }
            TypeConstructParams::Parameterized(base, params) => {
                log!("Base is given");
                let mut constructed = vec![];

                for item in params.iter() {
                    let type_id = self.construct_usage_generic(item, positional_args, type_args)?;
                    constructed.push(type_id);
                }
                log!("Will construct type now");
                let result = self.construct_type(*base, &constructed);
                log!("Result is {result:?}");
                return result;
            }
        }
    }

    pub fn construct_type(
        &mut self,
        constructor_id: TypeConstructorId,
        positional_args: &[TypeInstanceId],
    ) -> Result<TypeInstanceId, TypeConstructionError> {
        //log!("Construct_type called with parameters {constructor_id:?} {positional_args:?}");

        let c = self.constructors.find(constructor_id);
        for existing_type in &self.types {
            if existing_type.base == constructor_id && existing_type.type_args == positional_args {
                /*log!(
                    "Returning cached type id {existing_type_id:?} {name}",
                    existing_type_id = existing_type.id,
                    name = existing_type.name
                );*/
                return Ok(existing_type.id);
            }
        }
        let kind = c.kind.clone();
        let return_type_constructor = c.function_return_type.clone();
        let function_arguments_constructors = c.function_params.clone();
        let variadic = c.function_variadic.clone();

        log!("Type not yet cached, will construct {constructor_id:?} {positional_args:?}");

        let id = TypeInstanceId(self.types.len());
        self.types.push(make_base_instance(id, c, positional_args));

        //build a map of argname => type id
        let mut type_args = HashMap::new();
        {
            let constructor = self.constructors.find(constructor_id);
            if constructor.type_params.len() != positional_args.len() {
                return Err(TypeConstructionError::IncorrectNumberOfArgs {
                    expected: constructor.type_params.len(),
                    received: positional_args.len(),
                });
            }

            for (index, type_arg) in constructor.type_params.iter().enumerate() {
                type_args.insert(type_arg.clone(), positional_args[index]);
            }
        }
        let return_type = if kind == TypeKind::Function {
            let return_type = self.construct_usage_generic(
                &return_type_constructor.expect("Function type should have a return type"),
                &positional_args,
                &type_args,
            )?;
            Some(return_type)
        } else {
            None
        };

        let function_args = if kind == TypeKind::Function {
            let args: Result<Vec<TypeInstanceId>, TypeConstructionError> =
                function_arguments_constructors
                    .iter()
                    .map(|x| self.construct_usage_generic(x, &positional_args, &type_args))
                    .collect();
            args?
        } else {
            vec![]
        };

        let allowed_casts = self.make_casts(constructor_id)?;
        let rhs_binary_ops = self.make_binary_ops(constructor_id)?;
        let unary_ops = self.make_unary_ops(constructor_id)?;

        log!("MAKE_FIELDS about to be called");

        let fields = self.make_fields(constructor_id, &type_args)?;

        let methods = self.make_methods(constructor_id, &type_args, id)?;

        let name = if kind == TypeKind::Function {
            let param_types = function_args
                .iter()
                .map(|x| self.get_instance(*x).name.clone())
                .collect::<Vec<_>>()
                .join(", ");

            let return_type = self.get_instance(return_type.unwrap()).name.clone();

            format!("fn ({param_types}) -> {return_type}")
        } else if positional_args.is_empty() {
            self.constructors.get_name(constructor_id)
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
            let cur = &mut self.types[id.0];
            cur.allowed_casts = allowed_casts;
            cur.rhs_binary_ops = rhs_binary_ops;
            cur.unary_ops = unary_ops;
            cur.fields = fields;
            cur.methods = methods;
            cur.name = name;
            if kind == TypeKind::Function {
                cur.function_args = function_args;
                cur.function_return_type = return_type;
                cur.is_variadic = variadic.0;
            }
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
            for type_usage in constructor.allowed_casts.clone().iter() {
                let constructed = self.construct_usage(type_usage)?;
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
            //@cloneless: this clone unfortunately seems necessary, otherwise the borrow is held for all operations
            //because of the borrow on constructor.rhs_binary_ops.
            for (operator, rhs, op_result) in constructor.rhs_binary_ops.clone().iter() {
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
            for (operator, rhs) in constructor.unary_ops.clone().iter() {
                result.push((*operator, self.construct_usage(rhs)?));
            }
            result
        };
        Ok(unary_ops)
    }

    fn make_fields(
        &mut self,
        constructor_id: TypeConstructorId,
        type_args: &HashMap<TypeParameter, TypeInstanceId>,
    ) -> Result<Vec<TypeInstanceStructField>, TypeConstructionError> {
        log!(
            "Constructing fields, type args = {}",
            self.print_type_args_pretty(type_args)
        );
        let fields = {
            let constructor = self.constructors.find(constructor_id);
            let mut result = vec![];
            let fields = constructor.fields.clone();
            for field_decl in &fields {
                log!("Constructing field {}", field_decl.name);
                result.push(TypeInstanceStructField {
                    name: field_decl.name,
                    field_type: self.construct_usage_generic(
                        &field_decl.field_type,
                        &vec![],
                        type_args,
                    )?,
                });
            }
            result
        };
        Ok(fields)
    }

    fn make_methods(
        &mut self,
        constructor_id: TypeConstructorId,
        type_args: &HashMap<TypeParameter, TypeInstanceId>,
        id: TypeInstanceId,
    ) -> Result<Vec<TypeInstanceStructMethod>, TypeConstructionError> {
        let methods = {
            let constructor = self.constructors.find(constructor_id).clone();

            let mut result = vec![];

            for TypeConstructorFunctionDeclaration { name, signature } in &constructor.functions {
                let sig_type = self.constructors.find(*signature);
                //cloning because we borrowed self in the line above... :(
                let function_params = sig_type.function_params.clone();
                let function_return_type = sig_type.function_return_type.clone();
                let variadic = sig_type.function_variadic.clone();

                assert!(
                    sig_type.kind == TypeKind::Function,
                    "Method type should be a function type"
                );

                let args = {
                    let mut args = vec![];
                    for arg in function_params {
                        let arg_constructed =
                            self.construct_usage_generic(&arg, &vec![], type_args)?;
                        args.push(arg_constructed);
                    }
                    args
                };

                let return_type =
                    self.construct_usage_generic(&function_return_type
                        .expect("Function associated with a type is not a function type, return type is null. If void, it should be the void type and not None"),
                        &args, type_args)?;

                let method_type_id = self.construct_method(id, &args, return_type, variadic.0);

                result.push(TypeInstanceStructMethod {
                    name: *name,
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
                    .construct_usage(&TypeConstructParams::simple(
                        self.constructors.common_types.$type,
                    ))
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
        make_type!(char);
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
        is_variadic: false,
        function_args: vec![],
        function_return_type: None,
        is_method_of: None,
    }
}
