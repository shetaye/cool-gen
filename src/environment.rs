use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use crate::symbol::Symbol;
use crate::tree::{Class, Type};

#[derive(Default)]
struct Scope {
    bindings: HashMap<Symbol, Type>
}

pub struct Environment {
    current_class: Rc<RefCell<Class>>,
    scopes: Vec<Scope>
}

impl Environment {
    pub fn new(class: Rc<RefCell<Class>>) -> Self {
        Self {
            current_class: class,
            scopes: Vec::new()
        }
    }

    /// List all in-scope methods. Returns tuple of (defining class, method name)
    pub fn materialize_methods(&self) -> Vec<(Symbol, Symbol)> {
	    vec![]
    }

    /// List all methods defined, even overriden ones. Returns tuple of (defining class, method name)
    pub fn enumerate_methods(&self) -> Vec<(Symbol, Symbol)> {
	    vec![]
    }

    /// List all bindings defined and their types. Returns tuple of (name, type)
    pub fn enumerate_bindings(&self) -> Vec<(Symbol, Type)> {
	    vec![]
    }

    pub fn push_scope(&mut self) {
	    self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
	    self.scopes.pop();
    }
}
