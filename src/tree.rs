use std::rc::Rc;
use std::cell::RefCell;
use la_arena::{Arena, Idx};
use crate::symbol::*;
use crate::environment::Environment;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    SelfType,
    Void,
    Concrete(ClassSymbol)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Formal {
    pub name: ObjectSymbol,
    pub type_: Type
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CaseArm {
    pub name: ObjectSymbol,
    pub type_: Type,
    pub body: Box<Expr>
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
    Int(u32),
    Bool(bool),
    Assignment {
        to: ObjectSymbol,
        val: Box<Expr>
    },
    Dispatch {
        on: Option<Box<Expr>>,
        at: Option<Type>,
	name: MethodSymbol,
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
    Block(Vec<Expr>),
    Let {
        binding: ObjectSymbol,
        type_: Type,
	initializer: Box<Expr>,
        body: Box<Expr>
    },
    Case {
        on: Box<Expr>,
        branches: Vec<CaseArm>
    },
    New(Type),
    Isvoid(Box<Expr>),
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
    Variable(ObjectSymbol),
    Hole(Type)
}

pub struct Method {
    pub name: MethodSymbol,
    pub formals: Vec<Formal>,
    pub ret_type: Type,
    pub body: Expr
}

pub struct Attribute {
    pub name: ObjectSymbol,
    pub type_: Type,
    pub body: Option<Expr>
}

pub struct Class {
    pub name: ClassSymbol,
    pub builtin: bool,
    pub inherits: Option<Idx<Class>>,
    pub children: Vec<Idx<Class>>,
    pub methods: Vec<Method>,
    pub attributes: Vec<Attribute>
}

impl Class {
    pub fn new(name: ClassSymbol, builtin: bool) -> Self {
	Class {
	    name,
	    builtin,
	    inherits: None,
	    children: Vec::new(),
	    methods: Vec::new(),
	    attributes: Vec::new()
	}
    }

    pub fn get_attr(&self, name: ObjectSymbol) -> Option<&Attribute> {
	self.attributes.iter().find(|a| a.name == name)
    }

    pub fn get_attr_mut(&mut self, name: ObjectSymbol) -> Option<&mut Attribute> {
	self.attributes.iter_mut().find(|a| a.name == name)
    }

    pub fn get_meth(&self, name: MethodSymbol) -> Option<&Method> {
	self.methods.iter().find(|a| a.name == name)
    }

    pub fn get_meth_mut(&mut self, name: MethodSymbol) -> Option<&mut Method> {
	self.methods.iter_mut().find(|a| a.name == name)
    }
}

pub struct Program {
    pub object: Idx<Class>,
    pub class_arena: Arena<Class>,
}

impl Program {
    pub fn new(st: &mut SymbolTable) -> Self {
	let mut cl: Arena<Class> = Arena::new();


	// Create base program
	let object_sym = st.insert_ref("Object").into();
	let object = cl.alloc(Class::new(object_sym, true));

	let mut p = Program {
	    object,
	    class_arena: cl,
	};

	// Create default hierarchy
	let io_sym = st.to_sym("IO");
	p.add_class(st, Some(object_sym), Class::new(io_sym.into(), true));

	let string_sym = st.to_sym("String");
	p.add_class(st, Some(object_sym), Class::new(string_sym.into(), true));

	let bool_sym = st.to_sym("Bool");
	p.add_class(st, Some(object_sym), Class::new(bool_sym.into(), true));

	let int_sym = st.to_sym("Int");
	p.add_class(st, Some(object_sym), Class::new(int_sym.into(), true));

	p
    }

    /// DFS to find class by name
    pub fn lookup_class(&self, name: ClassSymbol) -> Option<Idx<Class>> {
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

    pub fn add_class(&mut self, st: &mut SymbolTable, parent: Option<ClassSymbol>, c: Class) -> Idx<Class> {
	let obj = st.lookup("Object").unwrap();
	let true_parent = parent.unwrap_or(obj.into());
	let true_parent_idx = self.lookup_class(true_parent).unwrap();
	let new_class_idx = self.class_arena.alloc(c);
	self.class_arena[new_class_idx].inherits = Some(true_parent_idx);
	let parent_class = &mut self.class_arena[true_parent_idx];
	parent_class.children.push(new_class_idx);
	new_class_idx
    }

    pub fn debug_print_hierarchy(&self, st: &mut SymbolTable) {
        println!("⎯⎯⎯ Current Class Hierarchy ⎯⎯⎯");
        for (idx, class) in self.class_arena.iter() {
            println!(
                "  [{:?}] name={:?}   inherits→ [{:?}]   children→{:?}",
                idx,
                st.from_sym(class.name.into()),
                class.inherits,
                class.children,
            );
        }
        println!("⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯");
    }
}

