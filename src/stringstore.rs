use std::collections::HashMap;

/// A StringID that identifies a String inside the stringstore. This is only valid for the 
/// StringStore that created the ID. These StringIDs can be trivialy and cheaply copied
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Sid(usize);

/// A Datastructure that stores strings, handing out StringIDs that can be used to retrieve the
/// real strings at a later point. This is called interning.
#[derive(Clone, Default)]
pub struct StringStore {
    /// The actual strings that are stored in the StringStore. The StringIDs match the index of the
    /// string inside of this strings vector
    strings: Vec<String>,
    /// A Hashmap that allows to match already interned Strings to their StringID. This allows for
    /// deduplication since the same string won't be stored twice
    sids: HashMap<String, Sid>,
}

impl StringStore {

    /// Create a new empty StringStore
    pub fn new() -> Self {
        Self { strings: Vec::new(), sids: HashMap::new() }
    }

    /// Put the given string into the StringStore and get a StringID in return. If the string is 
    /// not yet stored, it will be after this.
    /// 
    /// Note: The generated StringIDs are only valid for the StringStore that created them. Using 
    /// the IDs with another StringStore is undefined behavior. It might return wrong Strings or 
    /// None.
    pub fn intern_or_lookup(&mut self, text: &str) -> Sid {
        self.sids.get(text).copied().unwrap_or_else(|| {
            let sid = Sid(self.strings.len());
            self.strings.push(text.to_string());
            self.sids.insert(text.to_string(), sid);
            sid
        })
    }

    /// Lookup and retrieve a string by the StringID. If the String is not found, None is returned.
    /// 
    /// Note: The generated StringIDs are only valid for the StringStore that created them. Using 
    /// the IDs with another StringStore is undefined behavior. It might return wrong Strings or 
    /// None.
    pub fn lookup(&self, sid: Sid) -> Option<&String> {
        self.strings.get(sid.0)
    }

}

#[cfg(test)]
mod tests {
    use super::StringStore;

    #[test]
    fn test_stringstore_intern_lookup() {
        let mut ss = StringStore::new();
        let s1 = "Hello";
        let s2 = "World";

        let id1 = ss.intern_or_lookup(s1);
        assert_eq!(ss.lookup(id1).unwrap().as_str(), s1);

        let id2 = ss.intern_or_lookup(s2);
        assert_eq!(ss.lookup(id2).unwrap().as_str(), s2);
        assert_eq!(ss.lookup(id1).unwrap().as_str(), s1);
    }

    #[test]
    fn test_stringstore_no_duplicates() {
        let mut ss = StringStore::new();
        let s1 = "Hello";
        let s2 = "World";

        let id1_1 = ss.intern_or_lookup(s1);
        assert_eq!(ss.lookup(id1_1).unwrap().as_str(), s1);

        let id1_2 = ss.intern_or_lookup(s1);
        assert_eq!(ss.lookup(id1_2).unwrap().as_str(), s1);

        // Check that the string is the same
        assert_eq!(id1_1, id1_2);

        // Check that only one string is actually stored
        assert_eq!(ss.strings.len(), 1);
        assert_eq!(ss.sids.len(), 1);



        let id2_1 = ss.intern_or_lookup(s2);
        assert_eq!(ss.lookup(id2_1).unwrap().as_str(), s2);

        let id2_2 = ss.intern_or_lookup(s2);
        assert_eq!(ss.lookup(id2_2).unwrap().as_str(), s2);

        // Check that the string is the same
        assert_eq!(id2_1, id2_2);

        assert_eq!(ss.strings.len(), 2);
        assert_eq!(ss.sids.len(), 2);
    }
}
