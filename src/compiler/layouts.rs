use std::{
    borrow::Borrow,
    collections::HashMap,
    convert::Infallible,
    fmt::Display,
    iter::Sum,
    num::TryFromIntError,
    ops::{Add, AddAssign, Deref, Shl, Sub, SubAssign},
};

use crate::{
    donkey_vm::vm::memory::NativeNumericType, semantic::mir::{MIRScope, ScopeId},
    types::type_instance_db::TypeInstanceManager,
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

impl Into<u32> for Bytes {
    #[inline(always)]
    fn into(self) -> u32 {
        self.0
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
                Bytes(self.0 + rhs as u32)
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
                Bytes(self.0 + *rhs as u32)
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
        return sum;
    }
}

impl Bytes {
    #[inline(always)]
    pub const fn size_of<T>() -> Bytes {
        Bytes(std::mem::size_of::<T>() as u32)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ByteRange {
    pub begin: Bytes,
    pub end: Bytes,
}

impl ByteRange {
    pub fn size(&self) -> Bytes {
        self.end - self.begin
    }
}

fn build_write_scope_byte_layout(
    scope: &MIRScope,
    all_scopes: &[MIRScope],
    type_db: &TypeInstanceManager,
) -> HashMap<String, (ByteRange, ScopeId)> {
    let mut current_index = scope.id;
    let mut found_var = vec![];
    loop {
        let scope = &all_scopes[current_index.0];

        for var in &scope.boundnames {
            found_var.push((var.name.clone(), var.type_instance.size(type_db), scope.id));
        }

        if current_index.0 == 0 {
            break;
        }
        current_index = scope.inherit;
    }

    let mut map: HashMap<String, (ByteRange, ScopeId)> = HashMap::new();
    let mut used_bytes = Bytes(0);
    for (name, size, scope) in found_var.into_iter().rev() {
        map.insert(
            name,
            (ByteRange {
                begin: used_bytes,
                end: used_bytes + size,
            }, scope)
        );
        used_bytes += size;
    }

    map
}

pub type ScopeVariables = HashMap<String, (ByteRange, ScopeId)>;
pub type ScopesVariables = Vec<ScopeVariables>;

#[derive(Debug)]
pub struct FunctionLayout {
    //Tells all variables accessible on a given scope, and on which scope it was declared
    pub variables_for_each_scope: ScopesVariables,

    pub largest_scope_size: Bytes,
}

pub fn generate_function_layout(
    scopes: &[MIRScope],
    type_db: &TypeInstanceManager,
) -> FunctionLayout {
    let scope_byte_layout = scopes
        .iter()
        .map(|scope| build_write_scope_byte_layout(scope, scopes, type_db))
        .collect::<Vec<_>>();

    //println!("Scope byte layout built: {scope_byte_layout:#?}");
    let mut largest_scope = Bytes(0);
    for sbl in &scope_byte_layout {
        let sum: Bytes = sbl.values().map(|x| x.0.size()).sum();
        if sum > largest_scope {
            largest_scope = sum;
        }
    }
    FunctionLayout {
        variables_for_each_scope: scope_byte_layout,
        largest_scope_size: largest_scope,
    }
}
