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
