use super::{
    object::*,
};

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct EnvironmentContent {
    pub store: BTreeMap<String, Object>,
    pub outer: Option<Environment>,
}

impl EnvironmentContent {
    pub fn new(outer: Option<Environment>) -> EnvironmentContent {
        EnvironmentContent { store: BTreeMap::new(), outer }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let obj = self.store.get(name);
        if obj.is_none() && self.outer.is_some() {
            self.outer.as_ref().unwrap().get(name)
        } else {
            obj.cloned()
        }
    }

    pub fn remove(&mut self, name: &str) -> Option<Object> {
        self.store.remove(name)
    }

    pub fn insert(&mut self, name: String, val: Object) -> Option<Object> {
        self.store.insert(name, val)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Environment(Rc<RefCell<EnvironmentContent>>);

impl Environment {
    pub fn new() -> Environment {
        Environment(Rc::new(RefCell::new(
            EnvironmentContent::new(None)
        )))
    }

    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment(Rc::new(RefCell::new(
            EnvironmentContent::new(Some(outer))
        )))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.0.borrow().get(name)
    }

    pub fn remove(&self, name: &str) -> Option<Object> {
        self.0.borrow_mut().remove(name)
    }

    pub fn insert(&self, name: String, val: Object) -> Option<Object> {
        self.0.borrow_mut().insert(name, val)
    }
}