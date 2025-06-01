use la_arena::Arena;

use crate::tree::*;
use crate::symbol::SymbolTable;
use crate::environment::Environment;

pub struct Emitter {
    indent: usize,
    output: String
}

impl Emitter {
    pub fn new() -> Self {
	Self {
	    indent: 0,
	    output: String::new()
	}
    }

    pub fn emit(self) -> String {
	self.output
    }

    fn emit_inline(&mut self, s: &str) {
	self.output.push_str(s);
    }

    fn enter_line(&mut self) {
	for _ in 0..self.indent {
	    self.output.push_str("  ");
	}
    }

    fn exit_line(&mut self) {
	self.output.push('\n');
    }

    fn enter_block(&mut self) {
	self.indent += 1;
	//self.output.push('\n');
    }

    fn exit_block(&mut self) {
	self.indent -= 1
    }
}

pub trait Emittable<'a> {
    type Context;
    
    fn emit(&self, e: &mut Emitter, c: Self::Context);
}

impl<'a> Emittable<'a> for Expr {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	match self {
	    Expr::Hole(t) => {
		e.emit_inline("[");
		t.emit(e, c);
		e.emit_inline("]");
	    },
	    _ => e.emit_inline("[Unhandled Expr]")
	}
    }
}

impl<'a> Emittable<'a> for Type {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	match self {
	    Type::SelfType => e.emit_inline("SELF_TYPE"),
	    Type::Concrete(sym) => e.emit_inline(c.from_sym(*sym))
	}
    }
}

impl<'a> Emittable<'a> for Attribute {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.enter_line();
	e.emit_inline(c.from_sym(self.name));
	e.emit_inline(": ");
	self.type_.emit(e, c);
	if let Some(body) = &self.body {
	    e.emit_inline(" <- ");
	    body.emit(e, c);
	    e.emit_inline(";");
	}
	e.exit_line();
    }
}

impl<'a> Emittable<'a> for Formal {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.emit_inline(c.from_sym(self.name));
	e.emit_inline(": ");
	self.type_.emit(e,c);
    }
}

impl<'a> Emittable<'a> for Method {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.enter_line();
	e.emit_inline(c.from_sym(self.name));
	e.emit_inline("(");
	for (i,f) in self.formals.iter().enumerate() {
	    if i > 0 {
		e.emit_inline(", ");
	    }
	    f.emit(e, c);
	}
	e.emit_inline("): ");
	self.ret_type.emit(e, c);
	e.emit_inline(" {");
	e.exit_line();
	e.enter_block();
	e.enter_line();
	self.body.emit(e, c);
	e.exit_line();
	e.exit_block();
	e.enter_line();
	e.emit_inline("};");
	e.exit_line();
    }
}

impl<'a> Emittable<'a> for Class {
    type Context = (&'a Arena<Class>, &'a SymbolTable);

    fn emit(&self, e: &mut Emitter, c: (&'a Arena<Class>, &'a SymbolTable)) {
	let (a, s) = c;
	e.enter_line();
	e.emit_inline("class ");
	e.emit_inline(s.from_sym(self.name));
	if let Some(p) = self.inherits {
	    if a[p].name != s.lookup("Object").unwrap() {
		e.emit_inline(" inherits ");
		e.emit_inline(s.from_sym(a[p].name));
	    }
	}
	e.emit_inline(" {");
	e.exit_line();
	e.enter_block();

	for attr in self.attributes.iter() {
	    attr.emit(e, s);
	}

	for meth in self.methods.iter() {
	    meth.emit(e, s);
	}

	e.exit_block();
	e.emit_inline("};");
	e.exit_line();
    }
}

impl<'a> Emittable<'a> for Program {
    type Context = ();
    
    fn emit(&self, e: &mut Emitter, c: ()) {
	for (idx, c) in self.class_arena.iter().filter(|(_, c)| !c.builtin) {
	    c.emit(e, (&self.class_arena, &self.symbol_table));
	}
    }
}
