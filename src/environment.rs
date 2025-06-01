use std::collections::HashMap;

use la_arena::Idx;

use crate::symbol::{Symbol, SymbolTable};
use crate::tree::*;

#[derive(Default)]
struct Scope {
    bindings: HashMap<Symbol, Type>,
    methods: HashMap<Symbol, Symbol>
}

impl Scope {
    fn new(bindings: HashMap<Symbol, Type>, methods: HashMap<Symbol, Symbol>) -> Self {
	Self { bindings, methods }
    }
}

pub struct Environment<'a> {
    current_class: Option<Idx<Class>>,
    scopes: Vec<Scope>,
    prog: &'a mut Program,
}

impl<'a> Environment<'a> {
    pub fn new(class: Option<Idx<Class>>, prog: &'a mut Program) -> Self {
	// Generate attribute bindings & method bindings from class hierarchy
	let mut scopes: Vec<Scope> = Vec::new();
	if let Some(initial_c) = class {
	    let mut current_c = Some(initial_c);
	    while let Some(c) = current_c {
		let class = prog.get_class(c);

		let mut bindings = HashMap::new();
		for a in class.attributes.iter() {
		    bindings.insert(class.name, a.type_);
		}
		let mut methods = HashMap::new();
		for m in class.methods.iter() {
		    methods.insert(m.name, class.name);
		}

		scopes.push(Scope::new(bindings, methods));

		current_c = class.inherits;
	    }
	}
        Self {
            current_class: class,
            scopes: scopes.into_iter().rev().collect(),
	    prog,
        }
    }

    pub fn to_sym(&mut self, name: &str) -> Symbol {
	self.prog.symbol_table.to_sym(name)
    }

    pub fn from_sym(&self, sym: Symbol) -> &str {
	self.prog.symbol_table.from_sym(sym)
    }

    pub fn get_class(&self, class: Idx<Class>) -> &Class {
	self.prog.get_class(class)
    }

    pub fn classes(&self) -> Vec<(Symbol, Idx<Class>)> {
	self.prog.class_arena
	    .iter()
	    .map(|(idx, c)| (c.name, idx))
	    .collect()
    }

    /// List all in-scope methods. Returns tuple of (method name, defining class)
    pub fn materialize_methods(&self) -> Vec<(Symbol, Symbol)> {
	    vec![]
    }

    /// List all methods defined, even overriden ones. Returns tuple of (method name, defining clas)
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

    /// Lookup a symbol in all scopes
    pub fn lookup_binding(&self, sym: &Symbol) -> Option<Type> {
	for s in self.scopes.iter().rev() {
	    if let Some(t) = s.bindings.get(sym) {
		return Some(t.clone());
	    }
	}
	None
    }

    /// Peek in local scope for a symbol
    pub fn peek_binding(&self, s: &Symbol) -> Option<Type>{
	match self.scopes.len() {
	    0 => None,
	    l => self.scopes[l-1].bindings.get(s).map(|t| t.clone())
	}
    }

    pub fn push_scope(&mut self) {
	    self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
	    self.scopes.pop();
    }

    pub fn materialize_type(&self, t: Type) -> Option<Idx<Class>> {
	match t {
	    Type::Concrete(t) => self.prog.lookup_class(t),
	    Type::SelfType => self.current_class
	}
    }

    pub fn subtypes_of(&self, t: Type) -> Vec<Type> {
	if let Some(materialized_supertype) = self.materialize_type(t) {
	    let mut subtypes = vec![];
	    let mut dfs_stack = vec![materialized_supertype];
	    while !dfs_stack.is_empty() {
		let next_idx = dfs_stack.pop().unwrap();
		let next_class = self.prog.get_class(next_idx);

		subtypes.push(Type::Concrete(next_class.name));

		// SELF_TYPE
		if Some(next_idx) == self.current_class {
		    subtypes.push(Type::SelfType);
		}
		
		for c in next_class.children.iter() {
		    dfs_stack.push(*c);
		}
	    }
	    subtypes
	} else {
	    vec![]
	}
    }
}
