use super::{
    object::*,
};

use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment { store: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn remove(&mut self, name: &str) -> Option<Object> {
        self.store.remove(name)
    }

    pub fn insert(&mut self, name: String, val: Object) -> Option<Object> {
        self.store.insert(name, val)
    }
}