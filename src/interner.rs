use std::collections::HashMap;

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
            //SAFETY As long as we execute this code in a single thread, this should be safe.
            //And we specifically marked the struct as !Send and !Sync
            let vec = &mut *(&self.strings as *const Vec<String> as *mut Vec<String>);
            vec.push(string);

            //SAFETY Same deal, plus we never return anything that references the hashmap table,
            //we only use it on lookups
            let as_static = std::mem::transmute::<&str, &'static str>(&self.strings[index.0]);
            let table = &mut *(&self.table as *const HashMap<&'static str, InternedString>
                as *mut HashMap<&'static str, InternedString>);
            table.insert(as_static, index);
        };

        return index;
    }

    pub fn get_string(&self, string: InternedString) -> String {
        self.borrow(string).to_string()
    }

    pub fn borrow(&self, string: InternedString) -> &str {
        &self.strings[string.0]
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
