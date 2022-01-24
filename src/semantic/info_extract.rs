use crate::semantic::mir::*;

use std::collections::{ HashSet, HashMap };

type TypeId = usize;

//represents the usage of a type. Useful for specific generic instantiations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstance {
    type_id: TypeId,
    generic_type_args: Vec<TypeId>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Simple,
    Generic,
    Function
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRecord {
    pub id: TypeId,
    pub kind: TypeKind,
    pub size: usize,
    pub name: String,
    pub num_generic_args: i32,
    pub func_arguments: Vec<TypeInstance>,
    pub func_return_type: Option<TypeInstance>
}

impl Default for TypeRecord {
    fn default() -> TypeRecord {
        TypeRecord {
            id: 0,
            kind: TypeKind::Simple,
            size: 0,
            name: "".into(),
            num_generic_args: 0,
            func_arguments: vec![],
            func_return_type: None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDatabase {
    pub types: Vec<TypeRecord>,
    pub by_name: HashMap<String, usize>
}

impl TypeDatabase {

    pub fn new() -> Self {
        let mut item = Self {
            types: vec![],
            by_name: HashMap::new()
        };
        item.init_builtin();
        return item;
    }

    pub fn add(&mut self, name: &str, size: usize) {
        let next_id = self.types.len();
        self.by_name.insert(name.into(), next_id);
        self.types.push(TypeRecord {
            id: next_id,
            kind: TypeKind::Simple,
            name: name.into(),
            size,
            .. TypeRecord::default()
        });
    }


    pub fn add_generic(&mut self, name: &str, num_generic_args: i32, size: usize) {
        let next_id = self.types.len();
        self.by_name.insert(
            format!("{}<{}>", name, num_generic_args), next_id);

        
        self.types.push(TypeRecord {
            id: next_id,
            kind: TypeKind::Generic,
            name: name.into(), 
            num_generic_args, 
            size,
            .. TypeRecord::default()
        });
    }

    pub fn find(&self, name: &str) -> Option<TypeRecord> {
        self.by_name.get(name).map(|i| self.types[*i].clone())
    }

    pub fn find_generic(&self, name: &str, args: i32) -> Option<TypeRecord> {
        self.by_name.get(&format!("{}<{}>", name, args)).map(|i| self.types[*i].clone())
    }

    fn init_builtin(&mut self) {
        use std::mem;

        self.add("Void", mem::size_of::<()>());

        self.add("i32", mem::size_of::<i32>());
        self.add("i64", mem::size_of::<i64>());

        self.add("u32", mem::size_of::<u32>());
        self.add("u64", mem::size_of::<u64>());
        
        //ptr + len
        self.add("str", mem::size_of::<usize>() + mem::size_of::<i32>());

        //ptr + num items
        self.add_generic("List", 1,  mem::size_of::<usize>() + mem::size_of::<i32>());
    } 
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalSymbolTypes {
    //stores global names and types, ex: a function name and its type
    pub by_name: HashMap<String, TypeInstance>
}

impl GlobalSymbolTypes {
    fn new() -> Self {
        Self {
            by_name: HashMap::new()
        }
    }

    fn register_function(&mut self, name: &str, positional_type_args: &[&str], return_type: TypeId) {

    }
}

/*

The extraction process goes as follows:
Each typed element has dependencies on other types. For instance, when typing a function,
we have arguments and return types, it depends on these other types existing.

Therefore, lets iterate over all MIR elements twice:

    - The first iteration assigns types to all elements, even if they don't 

*/

pub fn extract(mir: Vec<MIR>) {
    for node in mir {
        match node {
            MIR::DeclareFunction {
                function_name,
                parameters, //{name, name_type: MIRType}
                body, 
                return_type //MIRType
            } => {
                //here we have to build a Function type kind
                //and register it
                
                //the arguments of the function are assumed to already exist.



                //let function_type_kind = TypeData::Function(arguments_type_instances, return_type_instance);

            }
            _ => {}
        }
        
    }
    
}