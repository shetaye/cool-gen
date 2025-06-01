use std::rc::Rc;
use std::cell::RefCell;
use la_arena::{Arena, Idx};
use crate::symbol::{Symbol, SymbolTable};
use crate::environment::Environment;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    SelfType,
    Concrete(Symbol)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Formal {
    pub name: Symbol,
    pub type_: Type
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CaseArm {
    pub name: Symbol,
    pub type_: Type
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ArithmeticOp {
    Plus,
    Minus,
    Divide,
    Multiply,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ComparisonOp {
    LessThan,
    LessThanEqual,
    Equal
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    String(String),
    Int(i64),
    Bool(bool),
    Assignment {
        to: Symbol,
        val: Box<Expr>
    },
    Dispatch {
        on: Box<Expr>,
        at: Option<Type>,
        formals: Vec<Expr>
    },
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>
    },
    Loop {
        condition: Box<Expr>,
        body: Box<Expr>
    },
    Block {
        body: Vec<Expr>
    },
    Let {
        binding: Symbol,
        type_: Type,
        body: Box<Expr>
    },
    Case {
        arms: Vec<Expr>
    },
    New {
        type_: Type
    },
    Isvoid {
        expression: Box<Expr>
    },
    Arithmetic {
        op: ArithmeticOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Comparison {
        op: ComparisonOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Complement(Box<Expr>),
    Not(Box<Expr>),
    Hole(Type)
}

pub struct Method {
    pub name: Symbol,
    pub formals: Vec<Formal>,
    pub ret_type: Type,
    pub body: Expr
}

pub struct Attribute {
    pub name: Symbol,
    pub type_: Type,
    pub body: Option<Expr>
}

pub struct Class {
    pub name: Symbol,
    pub builtin: bool,
    pub inherits: Option<Idx<Class>>,
    pub children: Vec<Idx<Class>>,
    pub methods: Vec<Method>,
    pub attributes: Vec<Attribute>
}

impl Class {
    pub fn new(name: Symbol, builtin: bool) -> Self {
	Class {
	    name,
	    builtin,
	    inherits: None,
	    children: Vec::new(),
	    methods: Vec::new(),
	    attributes: Vec::new()
	}
    }
}

pub struct Program {
    pub object: Idx<Class>,
    pub class_arena: Arena<Class>,
    pub symbol_table: SymbolTable
}

impl Program {
    pub fn new() -> Self {
	let mut st = SymbolTable::new();
	let mut cl: Arena<Class> = Arena::new();


	// Create base program
	let object_sym = st.insert_ref("Object");
	let object = cl.alloc(Class::new(object_sym, true));

	let mut p = Program {
	    object,
	    class_arena: cl,
	    symbol_table: st,
	};

	// Create default hierarchy
	let io_sym = p.to_sym("IO");
	p.add_class(Some(object_sym), Class::new(io_sym, true));

	let string_sym = p.to_sym("String");
	p.add_class(Some(object_sym), Class::new(string_sym, true));

	let bool_sym = p.to_sym("Bool");
	p.add_class(Some(object_sym), Class::new(bool_sym, true));

	let int_sym = p.to_sym("Int");
	p.add_class(Some(object_sym), Class::new(int_sym, true));

	p
    }

    pub fn to_sym(&mut self, name: &str) -> Symbol {
	self.symbol_table.to_sym(name)
    }

    pub fn from_sym(&self, sym: Symbol) -> &str {
	self.symbol_table.from_sym(sym)
    }

    /// DFS to find class by name
    pub fn lookup_class(&self, name: Symbol) -> Option<Idx<Class>> {
	let mut st = vec![self.object];
	while !st.is_empty() {
	    let p = st.pop().unwrap();
	    let c = &self.class_arena[p];

	    if c.name == name {
		return Some(p)
	    }
	    
	    st.extend_from_slice(&c.children);
	}
	None
    }

    pub fn get_class(&self, class: Idx<Class>) -> &Class {
	&self.class_arena[class]
    }

    pub fn get_class_mut(&mut self, class: Idx<Class>) -> &mut Class {
	&mut self.class_arena[class]
    }

    pub fn add_class(&mut self, parent: Option<Symbol>, c: Class) -> Idx<Class> {
	let obj = self.symbol_table.lookup("Object").unwrap();
	let true_parent = parent.unwrap_or(obj);
	let true_parent_idx = self.lookup_class(true_parent).unwrap();
	let new_class_idx = self.class_arena.alloc(c);
	self.class_arena[new_class_idx].inherits = Some(true_parent_idx);
	let parent_class = &mut self.class_arena[true_parent_idx];
	parent_class.children.push(new_class_idx);
	new_class_idx
    }
}

