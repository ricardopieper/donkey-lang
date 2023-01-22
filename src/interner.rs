#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct InternedString(usize);

//The string interner uses RefCell so that we can share it freely
pub struct StringInterner {
    strings: RefCell<Vec<String>>,
    //The key is actually a string inside the strings vec.
    //However, we never return this reference. Instead we return a reference to strings
    //which will have a lifetime bounded to Self.
    table: RefCell<HashMap<&'static str, InternedString>>,
}

impl StringInterner {
    pub fn new() -> Self {
        return StringInterner {
            strings: RefCell::new(vec![]),
            table: RefCell::new(HashMap::new()),
        };
    }

    pub fn get(&self, string: &str) -> InternedString {
        {
            if let Some(v) = self.table.borrow().get(string) {
                return *v;
            }
        }
        return self.insert_new_string(string.to_string());
    }

    fn insert_new_string(&self, string: String) -> InternedString {
        let index = InternedString(self.strings.borrow().len());
        {
            self.strings.borrow_mut().push(string);
        }
        //get a 'static str from the last pushed string, which now lives in the
        //vec and will be dropped when Self is dropped
        let last_pushed_str: &'static str = unsafe {
            let strings_ref = self.strings.borrow();
            let ptr = strings_ref.last().unwrap();
            let const_str = ptr.as_ref() as *const str;
            &*const_str
        };

        {
            self.table.borrow_mut().insert(last_pushed_str, index);
        }
        return index;
    }

    pub fn get_string<'intern>(&'intern self, string: InternedString) -> String {
        return self.strings.borrow()[string.0].to_string();
    }

    pub fn borrow<'intern>(&'intern self, string: InternedString) -> Ref<'intern, String> {
        let borrow = self.strings.borrow();
        Ref::map(borrow, |x| &x[string.0])
    }
}

pub trait PrintableInternedString {
    fn to_string(self, interner: &StringInterner) -> String;
    fn write_str(
        self,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}
pub trait JoinableInternedStringSlice {
    fn join_interned(self, interner: &StringInterner, sep: &str) -> String;
}

impl PrintableInternedString for InternedString {
    fn to_string(self, interner: &StringInterner) -> String {
        interner.get_string(self).to_string()
    }
    fn write_str(
        self,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str(&interner.borrow(self))
    }
}

impl JoinableInternedStringSlice for &[InternedString] {
    fn join_interned(self, interner: &StringInterner, sep: &str) -> String {
        let strings = self
            .iter()
            .map(|x| x.to_string(interner))
            .collect::<Vec<_>>();
        strings.join(sep)
    }
}

impl JoinableInternedStringSlice for &Vec<InternedString> {
    fn join_interned(self, interner: &StringInterner, sep: &str) -> String {
        self.as_slice().join_interned(interner, sep)
    }
}

impl InternedString {
    pub fn to_string(self, interner: &StringInterner) -> String {
        interner.get_string(self)
    }

    pub fn borrow<'intern>(self, interner: &'intern StringInterner) -> Ref<'intern, String> {
        interner.borrow(self)
    }
}

macro_rules! interner {
    ($interner:expr) => {
        macro_rules! istr {
            ($str:expr) => {
                $interner.get($str)
            };
        }
    };
}
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
};
