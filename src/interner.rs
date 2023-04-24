use std::{collections::HashMap, ops::Deref};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct InternedString(pub usize);

//The string interner uses RefCell so that we can share it freely
pub struct StringInterner {
    strings: Vec<String>,
    //The key is actually a string inside the strings vec.
    //However, we never return this reference. Instead we return a reference to strings
    //which will have a lifetime bounded to Self.
    table: HashMap<&'static str, InternedString>,
}

//I swear I know what I'm doing!
impl !Send for StringInterner {}
impl !Sync for StringInterner {}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            strings: vec![],
            table: HashMap::new(),
        }
    }

    pub fn get(&self, string: &str) -> InternedString {
        if let Some(v) = self.table.get(string) {
            return *v;
        }

        self.insert_new_string(string.to_string())
    }

    fn insert_new_string(&self, string: String) -> InternedString {
        let index = InternedString(self.strings.len());
        unsafe {
            //@SAFETY As long as we execute this code in a single thread, this should be safe.
            //And we specifically marked the struct as !Send and !Sync.
            //We also never return anything that references the hashmap table,
            //we only use the hashmap on lookups. We do return the InternedString value, but
            //it's returned by copy, and the value itself is just a usize.
            //If the strings vec is resized, the vec inside String is are still valid,
            //because the strings inside the vec are not changed, the pointers are the same
            //and the values themselves are not dropped or mutated. It is that still valid
            //pointer address (and len) that we are storing in the hashmap.
            //Also, the strings cannot be deleted from the string vec, none of the methods in
            //the interner delete any values, so they are never dropped and remain valid as long as
            //the interner is alive.

            let vec = &mut *(&self.strings as *const Vec<String> as *mut Vec<String>);
            vec.push(string);

            let str_ref: &str = (self.strings[index.0]).deref();
            let as_static: &'static str = std::mem::transmute(str_ref);
            type MutMap = HashMap<&'static str, InternedString>;
            let table = &mut *(&self.table as *const MutMap as *mut MutMap);
            table.insert(as_static, index);
        };

        index
    }

    pub fn get_string(&self, string: InternedString) -> String {
        self.borrow(string).to_string()
    }

    pub fn borrow(&self, string: InternedString) -> &str {
        self.strings[string.0].as_ref()
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
        interner.get_string(self)
    }
    fn write_str(
        self,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str(interner.borrow(self))
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

    pub fn borrow(self, interner: &StringInterner) -> &str {
        interner.borrow(self)
    }
}

mod test {
    #[allow(unused_imports)] //I don't know why this is necessary, cargo clippy --fix removes it resulting in a compiltaion error
    use super::StringInterner;


    #[test]
    fn test_interning() {
        let interner = StringInterner::new();
        let a = interner.get("a");
        let b = interner.get("b");
        let c = interner.get("c");

        let a2 = interner.get("a");
        let b2 = interner.get("b");
        let c2 = interner.get("c");
        assert_eq!(a, a2);
        assert_eq!(b, b2);
        assert_eq!(c, c2);
    }

    #[test]
    fn force_internal_vec_reallocation() {
        let all_strings = (0..65536).map(|x| x.to_string()).collect::<Vec<_>>();

        for _ in 0..100 {
            let interner = StringInterner::new();

            //add a bunch of numbers
            for i in 0..32 {
                interner.get(all_strings[i].as_str());
            }

            let some_num = interner.get("17");
            let borrow = some_num.borrow(&interner);

            //force a couple reallocations
            for i in 32..512 {
                interner.get(all_strings[i].as_str());
            }

            assert_eq!(borrow, "17");
        }
    }
}
