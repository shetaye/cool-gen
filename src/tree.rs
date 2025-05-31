use std::rc::Rc;
use std::cell::RefCell;
use crate::symbol::{Symbol, SymbolTable};

#[derive(Clone, Debug)]
pub enum Type {
    SelfType,
    Concrete(Symbol)
}

pub struct Formal {
    name: Symbol,
    type_: Type
}

pub struct CaseArm {
    name: Symbol,
    type_: Type
}

pub enum ArithmeticOp {
    Plus,
    Minus,
    Divide,
    Multiply,
}

pub enum ComparisonOp {
    LessThan,
    LessThanEqual,
    Equal
}

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
}

pub struct Method {
    name: Symbol,
    formals: Vec<Formal>,
    body: Expr
}

pub struct Attribute {
    name: Symbol,
    body: Option<Expr>
}

pub struct Class {
    name: Symbol,
    inherits: Option<Rc<RefCell<Class>>>,
    children: Vec<Rc<RefCell<Class>>>,
    methods: Vec<Method>,
    attributes: Vec<Attribute>
}

impl Class {
    fn new(s: Symbol) -> Self {
	Class {
	    name: s,
	    inherits: None,
	    children: Vec::new(),
	    methods: Vec::new(),
	    attributes: Vec::new()
	}
    }

    fn new_shared_ref(s: Symbol) -> Rc<RefCell<Self>> {
	Rc::new(RefCell::new(Self::new(s)))
    }

    fn add_child(&mut self, c: Self) {
	let rc = Rc::new(RefCell::new(c));
	self.children.push(rc);
    }
}

pub struct Program {
    class_hierarchy: Rc<RefCell<Class>>,
    symbol_table: SymbolTable
}

impl Program {
    fn new() -> Self {
        // Create symbol table
        let mut symbol_table = SymbolTable::new();

	    // Create default hierarchy
	    let obj_sym = symbol_table.insert_ref("Object");
	    let obj_class = Class::new_shared_ref(obj_sym);

	    // TODO
	    
	    Self {
	        class_hierarchy: obj_class,
	        symbol_table
	    }
    }
}

