use std::{
    collections::BTreeMap,
    sync::{Mutex, OnceLock},
};

#[derive(Hash, Eq, PartialEq, Clone, Copy, PartialOrd, Ord)]
pub struct InternedString {
    pub index: usize,
    #[cfg(test)]
    pub str_interned: &'static str,
}

static GLOBAL_INTERNER: OnceLock<StringInterner> = OnceLock::new();

pub struct StringInternerState {
    pub strings: Vec<String>,
    pub table: BTreeMap<&'static str, InternedString>,
}

pub struct StringInterner {
    state: Mutex<StringInternerState>,
}

impl StringInterner {
    pub fn get() -> &'static StringInterner {
        GLOBAL_INTERNER.get_or_init(|| StringInterner {
            state: Mutex::new(StringInternerState {
                strings: Vec::new(),
                table: BTreeMap::new(),
            }),
        })
    }

    pub fn intern(&self, string: &str) -> InternedString {
        let mut state = self.state.lock().unwrap();
        if let Some(v) = state.table.get(string) {
            return *v;
        }

        let index = state.strings.len();
        state.strings.push(string.to_string());
        let last = state.strings.last().unwrap();
        let unsafe_str_key = unsafe { std::mem::transmute::<&str, &'static str>(last) };
        let interned = InternedString {
            index,
            #[cfg(test)]
            str_interned: unsafe_str_key,
        };
        state.table.insert(unsafe_str_key, interned);
        interned
    }

    pub fn borrow(&'static self, string: InternedString) -> &'static str {
        let strings = self.state.lock().unwrap();
        let string = strings.strings.get(string.index).unwrap();
        unsafe {
            //SAFETY: it's OK because we know that the string is never removed
            std::mem::transmute::<&str, &'static str>(string)
        }
    }

    pub fn get_string(&self, string: InternedString) -> String {
        let strings = self.state.lock().unwrap();
        strings.strings.get(string.index).unwrap().to_string()
    }

    pub fn write_to(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        string: InternedString,
    ) -> std::fmt::Result {
        let strings = self.state.lock().unwrap();
        f.write_str(strings.strings.get(string.index).unwrap())
    }
}

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        StringInterner::get().write_to(f, *self)
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        StringInterner::get().write_to(f, *self)
    }
}

impl Into<String> for &InternedString {
    fn into(self) -> String {
        StringInterner::get().get_string(*self)
    }
}

impl Into<InternedString> for &str {
    fn into(self) -> InternedString {
        StringInterner::get().intern(self)
    }
}

impl Into<InternedString> for String {
    fn into(self) -> InternedString {
        StringInterner::get().intern(&self)
    }
}

impl InternedString {
    pub fn new(string: &str) -> InternedString {
        StringInterner::get().intern(string)
    }

    pub fn write_str(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        StringInterner::get().write_to(f, *self)
    }
}

impl PartialEq<str> for InternedString {
    fn eq(&self, other: &str) -> bool {
        StringInterner::get().borrow(*self) == other
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &str {
        StringInterner::get().borrow(*self)
    }
}

mod test {

    #[test]
    fn test_interning() {
        let interner = crate::interner::StringInterner::get();
        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");

        let a2 = interner.intern("a");
        let b2 = interner.intern("b");
        let c2 = interner.intern("c");
        assert_eq!(a, a2);
        assert_eq!(b, b2);
        assert_eq!(c, c2);
    }
}
