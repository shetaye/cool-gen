use la_arena::Arena;

use crate::tree::*;
use crate::symbol::*;
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

impl<'a> Emittable<'a> for ObjectSymbol {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.emit_inline(c.from_sym(Symbol::from(*self)))
    }
}

impl<'a> Emittable<'a> for MethodSymbol {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.emit_inline(c.from_sym(Symbol::from(*self)))
    }
}

impl<'a> Emittable<'a> for ClassSymbol {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	e.emit_inline(c.from_sym(Symbol::from(*self)))
    }
}

impl<'a> Emittable<'a> for CaseArm {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
        //   name : Type => body ;
        self.name.emit(e, c);
        e.emit_inline(" : ");
        self.type_.emit(e, c);
        e.emit_inline(" => ");
        self.body.emit(e, c);
        e.emit_inline(";");
    }
}

impl<'a> Emittable<'a> for Expr {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
        match self {
            Expr::Int(i) => {
                e.emit_inline(i.to_string().as_str());
            }

            Expr::Bool(b) => {
                e.emit_inline(if *b { "true" } else { "false" });
            }

            Expr::String(s) => {
                e.emit_inline("(\"");
                e.emit_inline(s);
                e.emit_inline("\")");
            }

            Expr::Assignment { to, val } => {
                // x <- y
                to.emit(e, c);
                e.emit_inline(" <- ");
                val.emit(e, c);
            }

            Expr::Dispatch { on, at, name, formals } => {
                //
                // Format:
                //   [ (on_expr) ] [@StaticType].method(
                //     arg1,
                //     arg2,
                //     ...
                //   )
                //

                // 1) Receiver, if any
                if let Some(on_expr) = on {
                    e.emit_inline("(");
                    on_expr.emit(e, c);
                    e.emit_inline(")");
                }

                // 2) Static dispatch, if any
                if let Some(at_type) = at {
                    e.emit_inline("@");
                    at_type.emit(e, c);
                }

                // 3) Dot + method name (only if we saw on or at)
                if on.is_some() || at.is_some() {
                    e.emit_inline(".");
                }
                name.emit(e, c);
                e.emit_inline("(");

                // 4) Arguments: one per line, indented
                if !formals.is_empty() {
                    e.exit_line();
                    e.enter_block();
                    for (i, f) in formals.iter().enumerate() {
                        e.enter_line();
                        f.emit(e, c);
                        if i < formals.len() - 1 {
                            e.emit_inline(",");
                        }
                        e.exit_line();
                    }
                    e.exit_block();

                    // closing “)” at the same indent as “method(”
                    e.enter_line();
                }
                e.emit_inline(")");
            }

            Expr::If { condition, then, else_ } => {
                //
                // if ( condition )
                //   then_branch
                // else
                //   else_branch
                // fi
                //

                // “if (cond)”
                e.emit_inline("if (");
                condition.emit(e, c);
                e.emit_inline(")");
                e.exit_line();

                // then‐branch
                e.enter_block();
                e.enter_line();
                then.emit(e, c);
                e.exit_line();
                e.exit_block();

                // “else”
                e.enter_line();
                e.emit_inline("else");
                e.exit_line();

                // else‐branch
                e.enter_block();
                e.enter_line();
                else_.emit(e, c);
                e.exit_line();
                e.exit_block();

                // “fi”
                e.enter_line();
                e.emit_inline("fi");
            }

            Expr::Loop { condition, body } => {
                //
                // while ( condition )
                //   body
                // pool
                //

                // “while (cond)”
                e.emit_inline("while (");
                condition.emit(e, c);
                e.emit_inline(")");
                e.exit_line();

                // body
                e.enter_block();
                e.enter_line();
                body.emit(e, c);
                e.exit_line();
                e.exit_block();

                // “pool”
                e.enter_line();
                e.emit_inline("pool");
            }

            Expr::Block(exprs) => {
                //
                // block: print each sub‐expr on its own line
                //
                
		// Opening brace
		e.emit_inline("{");
		e.exit_line();

		// Increase indent for inner expressions
		e.enter_block();
		for expr in exprs {
		    e.enter_line();
		    expr.emit(e, c);
		    e.emit_inline(";");
		    e.exit_line();
		}
		e.exit_block();

		// Closing brace
		e.enter_line();
		e.emit_inline("}");
            }

            Expr::Let { binding, type_, initializer, body } => {
                //
                // let x : T <- ( initializer )
                //   body
                //
                e.emit_inline("(let ");
                binding.emit(e, c);
                e.emit_inline(": ");
                type_.emit(e, c);

                // initializer in its own “( … )” block
                e.emit_inline(" <- (");
                e.exit_line();
                e.enter_block();
                e.enter_line();
                initializer.emit(e, c);
                e.exit_line();
                e.exit_block();
                e.enter_line();
                e.emit_inline(") in");

                // body on next line
                e.exit_line();
                e.enter_block();
                e.enter_line();
                body.emit(e, c);
		e.emit_inline(")");
                e.exit_line();
                e.exit_block();
            }

	    Expr::Variable(name) => {
                // Simply print the variable’s name
                name.emit(e, c);
            }


            Expr::Case(arms) => {
                //
                // case
                //   name1 : Type1 => expr1;
                //   name2 : Type2 => expr2;
                // esac
                //
                e.emit_inline("case");
                e.exit_line();

                e.enter_block();
                for arm in arms {
                    e.enter_line();
                    arm.emit(e, c);
                    e.exit_line();
                }
                e.exit_block();

                e.enter_line();
                e.emit_inline("esac");
            }

            Expr::New(t) => {
                e.emit_inline("(new ");
                t.emit(e, c);
                e.emit_inline(")");
            }

            Expr::Isvoid(inner) => {
                e.emit_inline("isvoid ");
                inner.emit(e, c);
            }

            Expr::Arithmetic { op, lhs, rhs } => {
                lhs.emit(e, c);
                match op {
                    ArithmeticOp::Plus     => e.emit_inline(" + "),
                    ArithmeticOp::Minus    => e.emit_inline(" - "),
                    ArithmeticOp::Multiply => e.emit_inline(" * "),
                    ArithmeticOp::Divide   => e.emit_inline(" / "),
                }
                rhs.emit(e, c);
            }

            Expr::Comparison { op, lhs, rhs } => {
                lhs.emit(e, c);
                match op {
                    ComparisonOp::LessThan      => e.emit_inline(" < "),
                    ComparisonOp::LessThanEqual => e.emit_inline(" <= "),
                    ComparisonOp::Equal         => e.emit_inline(" = "),
                }
                rhs.emit(e, c);
            }

            Expr::Complement(inner) => {
                e.emit_inline("~");
                inner.emit(e, c);
            }

            Expr::Not(inner) => {
                e.emit_inline("not ");
                inner.emit(e, c);
            }

            Expr::Hole(t) => {
                e.emit_inline("[");
                t.emit(e, c);
                e.emit_inline("]");
            }
        }
    }
}

impl<'a> Emittable<'a> for Type {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	match self {
	    Type::SelfType => e.emit_inline("SELF_TYPE"),
	    Type::Concrete(sym) => sym.emit(e,c)
	}
    }
}

impl<'a> Emittable<'a> for Attribute {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
        // “name : Type <- ( … );”
        e.enter_line();
        self.name.emit(e, c);
        e.emit_inline(": ");
        self.type_.emit(e, c);

        if let Some(body) = &self.body {
            e.emit_inline(" <- (");
            e.exit_line();

            e.enter_block();
            e.enter_line();
            body.emit(e, c);
            e.exit_line();
            e.exit_block();

            // close this attribute on its own line
            e.enter_line();
            e.emit_inline(");");
            e.exit_line();
        } else {
            // no initializer: just terminate the line
            e.exit_line();
        }
    }
}

impl<'a> Emittable<'a> for Method {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
        // “foo(a:T, b:U): R {”
        e.enter_line();
        self.name.emit(e, c);
        e.emit_inline("(");
        for (i, f) in self.formals.iter().enumerate() {
            if i > 0 {
                e.emit_inline(", ");
            }
            f.emit(e, c);
        }
        e.emit_inline("): ");
        self.ret_type.emit(e, c);
        e.emit_inline(" {");
        e.exit_line();

        // method body indented
        e.enter_block();
        e.enter_line();
        self.body.emit(e, c);
        e.exit_line();
        e.exit_block();

        // close method
        e.enter_line();
        e.emit_inline("};");
        e.exit_line();
    }
}

impl<'a> Emittable<'a> for Formal {
    type Context = &'a SymbolTable;

    fn emit(&self, e: &mut Emitter, c: &'a SymbolTable) {
	self.name.emit(e,c);
	e.emit_inline(": ");
	self.type_.emit(e,c);
    }
}

impl<'a> Emittable<'a> for Class {
    type Context = (&'a Arena<Class>, &'a SymbolTable);

    fn emit(&self, e: &mut Emitter, c: (&'a Arena<Class>, &'a SymbolTable)) {
	let (a, s) = c;
	e.enter_line();
	e.emit_inline("class ");
	self.name.emit(e, s);
	if let Some(p) = self.inherits {
	    if a[p].name != s.lookup("Object").unwrap().into() {
		e.emit_inline(" inherits ");
		e.emit_inline(s.from_sym(a[p].name.into()));
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
    type Context = &'a SymbolTable;
    
    fn emit(&self, e: &mut Emitter, st: &'a SymbolTable) {
	for (idx, c) in self.class_arena.iter().filter(|(_, c)| !c.builtin) {
	    c.emit(e, (&self.class_arena, st));
	}
    }
}
