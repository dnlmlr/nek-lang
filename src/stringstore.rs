use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Sid(usize);

#[derive(Clone, Default)]
pub struct StringStore {
    strings: Vec<String>,
    sids: HashMap<String, Sid>,
}

impl StringStore {

    pub fn new() -> Self {
        Self { strings: Vec::new(), sids: HashMap::new() }
    }

    pub fn intern_or_lookup(&mut self, text: &str) -> Sid {
        self.sids.get(text).copied().unwrap_or_else(|| {
            let sid = Sid(self.strings.len());
            self.strings.push(text.to_string());
            self.sids.insert(text.to_string(), sid);
            sid
        })
    }

    pub fn lookup(&self, sid: Sid) -> Option<&String> {
        self.strings.get(sid.0)
    }

}
