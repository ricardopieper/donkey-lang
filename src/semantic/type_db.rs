use crate::semantic::mir::*;

use std::collections::{ HashSet, HashMap };

type TypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Simple,
    Generic
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRecord {
    pub id: TypeId,
    pub kind: TypeKind,
    pub size: usize,
    pub name: String,
    pub num_generic_args: i32
}

impl Default for TypeRecord {
    fn default() -> TypeRecord {
        TypeRecord {
            id: 0,
            kind: TypeKind::Simple,
            size: 0,
            name: "".into(),
            num_generic_args: 0
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
        self.add_generic("List", 1, mem::size_of::<usize>() + mem::size_of::<i32>());
    } 
}

