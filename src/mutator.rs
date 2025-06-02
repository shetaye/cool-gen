use std::collections::VecDeque;

use la_arena::Idx;

use crate::symbol::{ClassSymbol, MethodSymbol, ObjectSymbol, SymbolTable};
use crate::tree::*;
use crate::generator::*;
use crate::environment::Environment;

use rand::Rng;

type RNG = rand::rngs::ThreadRng;

/// A Mutator is like a Generator except it operates in-place
pub trait Mutator<T, C> {
    fn mutate(&mut self, obj: &mut T, context: C);
}

#[derive(Debug,Copy,Clone)]
pub struct ProgramMutationConfig {
    n_rounds: usize,
    n_classes: usize,
    min_children: usize,
    max_children: usize,
    min_attributes: usize,
    max_attributes: usize,
    min_methods: usize,
    max_methods: usize,
    min_formals: usize,
    max_formals: usize,
    min_block: usize,
    max_block: usize,
    min_case: usize,
    max_case: usize,
    p_void: f64,
    p_method_override: f64,
    p_formal_shadow: f64,
    p_dispatch_self: f64,
    p_dispatch_static: f64,
    p_let_shadow: f64,
    p_case_shadow: f64
}

impl ProgramMutationConfig {
    fn new(
	n_rounds: usize,
	n_classes: usize,
	min_children: usize,
	max_children: usize,
	min_attributes: usize,
	max_attributes: usize,
	min_methods: usize,
	max_methods: usize,
	min_formals: usize,
	max_formals: usize,
	min_block: usize,
	max_block: usize,
	min_case: usize,
	max_case: usize,
	p_void: f64,
	p_method_override: f64,
	p_formal_shadow: f64,
	p_dispatch_self: f64,
	p_dispatch_static: f64,
	p_let_shadow: f64,
	p_case_shadow: f64
    ) -> Self {
	Self {
	    n_rounds,
	    n_classes,
	    min_children,
	    max_children,
	    min_attributes,
	    max_attributes,
	    min_methods,
	    max_methods,
	    min_formals,
	    max_formals,
	    min_block,
	    max_block,
	    min_case,
	    max_case,
	    p_void,
	    p_method_override,
	    p_formal_shadow,
	    p_dispatch_self,
	    p_dispatch_static,
	    p_let_shadow,
	    p_case_shadow
	}
    }
}

impl Default for ProgramMutationConfig {
    fn default() -> Self {
	Self::new(
	    4,
	    10,
	    1,
	    2,
	    0,
	    5,
	    0,
	    5,
	    0,
	    3,
	    2,
	    4,
	    2,
	    4,
	    0.0,
	    0.1,
	    0.1,
		0.1,
	    0.1,
	    0.1,
	    0.1
	)
    }
}

pub struct ProgramMutator {
    rng: RNG,
    config: ProgramMutationConfig,
    class_name_generator: ClassNameGenerator,
    attribute_generator: ShallowAttributeGenerator<FreshObjectIDGenerator, SubtypeGenerator>,
    method_generator: ShallowMethodGenerator<ShadowingMethodIDGenerator, SubtypeGenerator>,
    expression_generator: ExpressionGenerator
}

impl ProgramMutator {
    pub fn new(config: ProgramMutationConfig) -> Self {
	let p_void = config.p_void;
	let p_method_override = config.p_method_override;
	let p_formal_shadow = config.p_formal_shadow;
	let min_formals = config.min_formals;
	let max_formals = config.max_formals;
	Self {
	    rng: rand::rng(),
	    config,
	    class_name_generator: ClassNameGenerator::new(),
	    attribute_generator: ShallowAttributeGenerator::new(
		FreshObjectIDGenerator::new(),
		SubtypeGenerator::new(),
		p_void
	    ),
	    method_generator: ShallowMethodGenerator::new(
		ShadowingMethodIDGenerator::new(p_method_override),
		SubtypeGenerator::new(),
		min_formals,
		max_formals,
		p_formal_shadow
	    ),
	    expression_generator: ExpressionGenerator::new(
		config.p_dispatch_self,
		config.p_dispatch_static,
		config.p_let_shadow,
		config.p_case_shadow,
		config.min_block,
		config.max_block,
		config.min_case,
		config.max_case
	    )
	}
    }
}

impl Mutator<Program, &mut SymbolTable> for ProgramMutator {
    fn mutate(&mut self, prog: &mut Program, st: &mut SymbolTable) {
	// Generate hierarchy
	let mut q: VecDeque<Idx<Class>> = VecDeque::new();
	let obj_sym = st.to_sym("Object").into();
	let obj_idx = prog.lookup_class(obj_sym).unwrap();
	q.push_back(obj_idx);
	for _ in 0..self.config.n_classes {
	    // Pop parent off queue
	    if let Some(p) = q.pop_front() {
		// Create some children
		let parent_sym = prog.get_class(p).name;
		let n_children: usize = self.rng.random_range(self.config.min_children..self.config.max_children);
		for _ in 0..n_children {
		    // Attempt to generate a name
		    let generation_attempt = self.class_name_generator.generate((), &mut Environment::new(None, prog, st));
		    if let Some(name) = generation_attempt {
			let child = prog.add_class(st, Some(parent_sym), Class::new(name, false));
			q.push_back(child);
		    }
		}
	    }
	}

	// Shallow generation pass to generate attributes & methods
	let mut dfs_stack: Vec<Idx<Class>> = vec![obj_idx];
	while !dfs_stack.is_empty() {
	    let next_idx = dfs_stack.pop().unwrap();

	    if !prog.get_class(next_idx).builtin {
		// Generate attributes
		let n_attributes: usize = self.rng.random_range(self.config.min_attributes..self.config.max_attributes);
		for i in 0..n_attributes {
		    let mut e = Environment::new(Some(next_idx), prog, st);
		    let generation_attempt = self.attribute_generator.generate((), &mut e);
		    if let Some(attr) = generation_attempt {
			prog.get_class_mut(next_idx).attributes.push(attr);
		    }
		}

		// Generate methods
		let n_methods: usize = self.rng.random_range(self.config.min_methods..self.config.max_methods);
		for _ in 0..n_methods {
		    let generation_attempt = self.method_generator.generate((), &mut Environment::new(Some(next_idx), prog, st));
		    if let Some(meth) = generation_attempt {
			prog.get_class_mut(next_idx).methods.push(meth);
		    }
		}
	    }

	    for c in prog.get_class(next_idx).children.iter() {
		dfs_stack.push(*c);
	    }
	}

	// Deep generation pass to fill all holes. Defer all actual mutation by collecting
	let new_attr = prog.class_arena
	    .iter()
	    .map(|(idx, class)| {
		let mut env = Environment::new(Some(idx), prog, st);
		let mut attributes: Vec<(ObjectSymbol, Expr)> = vec![];
		for a in class.attributes.iter() {
		    if let Some(b) = &a.body {
			env.push_scope();

			let mut body = self.expression_generator.generate(a.type_, &mut env).unwrap();
			let mut n_fill = 1;
			let mut n_rounds = 0;
			while n_fill > 0 && n_rounds < self.config.n_rounds {
			    (n_fill, body) = fill(body, &mut env, &mut self.expression_generator, false);
			    n_rounds += 1;
			}
			if n_fill > 0 {
			    (_, body) = fill(body, &mut env, &mut self.expression_generator, true);
			}
			attributes.push((a.name, body));

			env.pop_scope();
		    }
		}
		(idx, attributes)
	    })
	    .collect::<Vec<(Idx<Class>, Vec<(ObjectSymbol, Expr)>)>>();

	let new_method = prog.class_arena
	    .iter()
	    .map(|(idx, class)| {
		let mut env = Environment::new(Some(idx), prog, st);
		let mut methods: Vec<(MethodSymbol, Expr)> = vec![];
		for m in class.methods.iter() {
		    env.push_scope();

		    // Put formals in scope
		    for f in m.formals.iter() {
			env.bind(&f.name, f.type_);
		    }


		    // Body scope
		    env.push_scope();
		    let mut body = self.expression_generator.generate(m.ret_type, &mut env).unwrap();
		    let mut n_fill = 1;
		    let mut n_rounds = 0;
		    while n_fill > 0 && n_rounds < self.config.n_rounds {
			(n_fill, body) = fill(body, &mut env, &mut self.expression_generator, false);
			n_rounds += 1;
		    }
		    if n_fill > 0 {
			(_, body) = fill(body, &mut env, &mut self.expression_generator, true);
		    }
		    methods.push((m.name, body));
		    env.pop_scope();
		    

		    env.pop_scope();
		}
		(idx, methods)
	    })
	    .collect::<Vec<(Idx<Class>, Vec<(MethodSymbol, Expr)>)>>();

	for (idx, attrs) in new_attr {
	    let class = prog.get_class_mut(idx);
	    for (sym, body) in attrs {
		class.get_attr_mut(sym).unwrap().body = Some(body);
	    }
	}
	for (idx, methods) in new_method {
	    let class = prog.get_class_mut(idx);
	    for (sym, body) in methods {
		class.get_meth_mut(sym).unwrap().body = body;
	    }
	}
    }
}

fn fill(e: Expr, env: &mut Environment, gen: &mut ExpressionGenerator, f: bool) -> (usize, Expr) {
    match e {
	Expr::String(s) => (0, Expr::String(s)),
	Expr::Int(i) => (0, Expr::Int(i)),
	Expr::Bool(b) => (0, Expr::Bool(b)),
	Expr::Assignment{to, val} => {
	    let (n, new_val) = fill(*val, env, gen, f);
	    (n, Expr::Assignment{to, val: Box::new(new_val)})
	},
	Expr::Dispatch { on, at, name, formals } => {
            let mut total = 0;

            // Recurse on the receiver if present
            let new_on = if let Some(boxed_e) = on {
                let (n, new_e) = fill(*boxed_e, env, gen, f);
                total += n;
                Some(Box::new(new_e))
            } else {
                None
            };

            // Recurse on each actual argument
            let mut new_formals = Vec::with_capacity(formals.len());
            for arg in formals {
                let (n, new_arg) = fill(arg, env, gen, f);
                total += n;
                new_formals.push(new_arg);
            }

            (
                total,
                Expr::Dispatch {
                    on: new_on,
                    at,
                    name,
                    formals: new_formals,
                },
            )
        },

        Expr::If { condition, then, else_ } => {
            let (n1, new_cond) = fill(*condition, env, gen, f);
            let (n2, new_then) = fill(*then, env, gen, f);
            let (n3, new_else) = fill(*else_, env, gen, f);
            (n1 + n2 + n3, Expr::If {
                condition: Box::new(new_cond),
                then: Box::new(new_then),
                else_: Box::new(new_else),
            })
        },

        Expr::Loop { condition, body } => {
            let (n1, new_cond) = fill(*condition, env, gen, f);
            let (n2, new_body) = fill(*body, env, gen, f);
            (n1 + n2, Expr::Loop {
                condition: Box::new(new_cond),
                body: Box::new(new_body),
            })
        },

        Expr::Block(exprs) => {
            let mut total = 0;
            let mut new_vec = Vec::with_capacity(exprs.len());
            for sub in exprs {
                let (n, new_sub) = fill(sub, env, gen, f);
                total += n;
                new_vec.push(new_sub);
            }
            (total, Expr::Block(new_vec))
        },

        Expr::Let { binding, type_, initializer, body } => {
            // No initializer field; just recurse on the body
	    let (n_initializer, new_initializer) = fill(*initializer, env, gen, f);
	    env.push_scope();
	    env.bind(&binding, type_);
            let (n_body, new_body) = fill(*body, env, gen, f);
	    env.pop_scope();
            (n_initializer+n_body, Expr::Let {
                binding,
                type_,
		initializer: Box::new(new_initializer),
                body: Box::new(new_body),
            })
        },

        Expr::Case(branches) => {
            let mut total = 0;
            let mut new_branches = Vec::with_capacity(branches.len());
            for br in branches {
		env.push_scope();
		env.bind(&br.name, br.type_);
                let (n, new_br) = fill(*br.body, env, gen, f);
		env.pop_scope();
                total += n;
                new_branches.push(CaseArm {
		    name: br.name,
		    type_: br.type_,
		    body: Box::new(new_br)
		});
            }
            (total, Expr::Case(new_branches))
        },

        Expr::New(t) => {
            // No subexpressions to fill
            (0, Expr::New(t))
        },

        Expr::Isvoid(e_inner) => {
            let (n, new_e) = fill(*e_inner, env, gen, f);
            (n, Expr::Isvoid(Box::new(new_e)))
        },

        Expr::Arithmetic { op, lhs, rhs } => {
            let (n1, new_lhs) = fill(*lhs, env, gen, f);
            let (n2, new_rhs) = fill(*rhs, env, gen, f);
            (n1 + n2, Expr::Arithmetic {
                op,
                lhs: Box::new(new_lhs),
                rhs: Box::new(new_rhs),
            })
        },

        Expr::Comparison { op, lhs, rhs } => {
            let (n1, new_lhs) = fill(*lhs, env, gen, f);
            let (n2, new_rhs) = fill(*rhs, env, gen, f);
            (n1 + n2, Expr::Comparison {
                op,
                lhs: Box::new(new_lhs),
                rhs: Box::new(new_rhs),
            })
        },

        Expr::Complement(e_inner) => {
            let (n, new_e) = fill(*e_inner, env, gen, f);
            (n, Expr::Complement(Box::new(new_e)))
        },

        Expr::Not(e_inner) => {
            let (n, new_e) = fill(*e_inner, env, gen, f);
            (n, Expr::Not(Box::new(new_e)))
        },
	Expr::Hole(t) => {
	    (1, if f {
		gen.fallback().generate(t, env).unwrap()
	    } else {
		gen.generate(t, env).unwrap()
	    })
	},
	Expr::Variable(s) => (0, Expr::Variable(s))
    }
}
