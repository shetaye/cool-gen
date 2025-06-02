use std::collections::HashMap;

use la_arena::Idx;

use crate::symbol::{SymbolTable, Symbol, MethodSymbol, ClassSymbol, ObjectSymbol};
use crate::tree::*;

#[derive(Default)]
struct Scope {
    bindings: HashMap<ObjectSymbol, Type>,
    methods: HashMap<MethodSymbol, ClassSymbol>
}

impl Scope {
    fn new(bindings: HashMap<ObjectSymbol, Type>, methods: HashMap<MethodSymbol, ClassSymbol>) -> Self {
	Self { bindings, methods }
    }
}

pub struct Environment<'a> {
    current_class: Option<Idx<Class>>,
    scopes: Vec<Scope>,
    prog: &'a Program,
    symbol_table: &'a mut SymbolTable
}

impl<'a> Environment<'a> {
    pub fn new(class: Option<Idx<Class>>, prog: &'a Program, symbol_table: &'a mut SymbolTable) -> Self {
	// Generate attribute bindings & method bindings from class hierarchy
	let mut scopes: Vec<Scope> = Vec::new();
	if let Some(initial_c) = class {
	    let mut current_c = Some(initial_c);
	    while let Some(c) = current_c {
		let class = prog.get_class(c);

		let mut bindings = HashMap::new();
		for a in class.attributes.iter() {
		    bindings.insert(a.name, a.type_);
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
	    symbol_table
        }
    }

    pub fn class(&self) -> Option<Idx<Class>> {
	self.current_class
    }

    pub fn to_sym(&mut self, name: &str) -> Symbol {
	self.symbol_table.to_sym(name)
    }

    pub fn from_sym(&self, sym: Symbol) -> &str {
	self.symbol_table.from_sym(sym)
    }

    pub fn get_class(&self, class: Idx<Class>) -> &Class {
	self.prog.get_class(class)
    }

    pub fn lookup_class(&self, class: ClassSymbol) -> Option<Idx<Class>> {
	self.prog.lookup_class(class)
    }

    pub fn classes(&self) -> Vec<(ClassSymbol, Idx<Class>)> {
	self.prog.class_arena
	    .iter()
	    .map(|(idx, c)| (c.name, idx))
	    .collect()
    }

    /// List all in-scope methods. Returns tuple of (method name, defining class)
    pub fn materialize_methods(&self) -> Vec<(MethodSymbol, ClassSymbol)> {
	let mut materialized: HashMap<MethodSymbol, ClassSymbol> = HashMap::new();
	for s in self.scopes.iter() {
	    for (k, v) in s.methods.iter() {
		materialized.insert(*k, *v);
	    }
	}
	materialized.into_iter().collect()
    }

    /// List all methods defined, even overriden ones. Returns tuple of (method name, defining clas)
    pub fn enumerate_methods(&self) -> Vec<(MethodSymbol, ClassSymbol)> {
	self.scopes.iter()
	    .map(|s| s.methods.iter())
	    .flatten()
	    .map(|(s1,s2)| (s1.clone(), s2.clone()))
	    .collect()
    }

    /// List all methods defined in the current class
    pub fn enumerate_local_methods(&self) -> Vec<MethodSymbol> {
	if let Some(c_idx) = self.current_class {
	    let c = self.get_class(c_idx);
	    c.methods
		.iter()
		.map(|m| m.name.clone())
		.collect()
	} else {
	    vec![]
	}
    }

    /// List all bindings defined and their types. Returns tuple of (name, type)
    pub fn enumerate_bindings(&self) -> Vec<(ObjectSymbol, Type)> {
	let mut bindings: HashMap<ObjectSymbol, Type> = HashMap::new();
	for scope in self.scopes.iter() {
	    for (sym, typ) in scope.bindings.iter() {
		bindings.insert(sym.clone(), typ.clone());
	    }
	}
	bindings.into_iter().collect()
    }

    /// List all bindings defined in the local scope. Returns tuple of (name, type)
    pub fn enumerate_local_bindings(&self) -> Vec<(ObjectSymbol, Type)> {
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

    pub fn bind(&mut self, sym: &ObjectSymbol, type_: Type) {
	if self.scopes.is_empty() {
	    panic!("Binding to no scope!");
	}
	let n = self.scopes.len();
	let scope = &mut self.scopes[n - 1];
	scope.bindings.insert(*sym, type_);
    }

    /// Lookup a binding in all scopes
    pub fn lookup_binding(&self, sym: &ObjectSymbol) -> Option<Type> {
	for s in self.scopes.iter().rev() {
	    if let Some(t) = s.bindings.get(sym) {
		return Some(t.clone());
	    }
	}
	None
    }

    /// Lookup a method in all scopes. Returns the defining class
    pub fn lookup_method(&self, sym: &MethodSymbol) -> Option<ClassSymbol> {
	for s in self.scopes.iter().rev() {
	    if let Some(t) = s.methods.get(sym) {
		return Some(t.clone());
	    }
	}
	None
    }

    /// Peek in a local scope for a binding
    pub fn peek_binding(&self, s: &ObjectSymbol) -> Option<Type> {
	match self.scopes.len() {
	    0 => None,
	    l => self.scopes[l-1].bindings.get(s).map(|s| s.clone())
	}
    }

    /// Peek in local scope for a method. Returns its presence
    pub fn peek_method(&self, s: &MethodSymbol) -> bool {
	match self.scopes.len() {
	    0 => false,
	    l => self.scopes[l-1].methods.get(s).is_some()
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
            Type::Void => None,
	    Type::Concrete(t) => self.prog.lookup_class(t),
	    Type::SelfType => self.current_class
	}
    }

    pub fn is_subtype(&self, lhs: Type, rhs: Type) -> bool {
	if let Some(lhs_class) = self.materialize_type(lhs) {
	    if let Some(rhs_class) = self.materialize_type(rhs) {
		let mut current_c = Some(lhs_class);
		while let Some(c) = current_c {
		    if c == rhs_class {
			return true;
		    }
		    current_c = self.get_class(c).inherits;
		}
	    }
	}
	false
    }

    pub fn subtypes_of(&self, t: Type, selftype: bool) -> Vec<Type> {
        match t {
            Type::Concrete(supertype) => {
                let materialized_supertype = self.prog.lookup_class(supertype).unwrap();
                let mut subtypes = vec![];
                let mut dfs_stack = vec![materialized_supertype];
                while !dfs_stack.is_empty() {
                    let next_idx = dfs_stack.pop().unwrap();
                    let next_class = self.prog.get_class(next_idx);

                    subtypes.push(Type::Concrete(next_class.name));

                    // SELF_TYPE
                    if selftype && Some(next_idx) == self.current_class {
                        subtypes.push(Type::SelfType);
                    }

                    for c in next_class.children.iter() {
                        dfs_stack.push(*c);
                    }
                }
                subtypes
            },
            Type::SelfType => if selftype { vec![t] } else { vec![] },
            Type::Void => vec![t]
        }
    }
}
