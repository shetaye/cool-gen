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
	// let ref_c = self.current_class;
	vec![]
    }

    /// List all bindings defined and their types. Returns tuple of (name, type)
    pub fn enumerate_bindings(&self) -> Vec<(Symbol, Type)> {
	let mut bindings: HashMap<Symbol, Type> = HashMap::new();
	for scope in self.scopes.iter() {
	    for (sym, typ) in scope.bindings.iter() {
		bindings.insert(sym.clone(), typ.clone());
	    }
	}
	bindings.into_iter().collect()
    }

    /// List all bindings defined in the local scope. Returns tuple of (name, type)
    pub fn enumerate_local_bindings(&self) -> Vec<(Symbol, Type)> {
	if self.scopes.is_empty() {
	    vec![]
	} else {
	    self.scopes[self.scopes.len() - 1]
		.bindings
		.iter()
		.map(|(s,t)| (s.clone(), t.clone()))
		.collect()
	}
    }

    pub fn push_scope(&mut self) {
	    self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
	    self.scopes.pop();
    }
}
