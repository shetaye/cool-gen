use std::collections::VecDeque;

use la_arena::Idx;

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
    n_classes: usize,
    min_children: usize,
    max_children: usize,
    min_attributes: usize,
    max_attributes: usize,
    min_methods: usize,
    max_methods: usize,
    min_formals: usize,
    max_formals: usize,
    p_void: f64,
    p_method_override: f64,
    p_formal_shadow: f64
}

impl ProgramMutationConfig {
    fn new(
	n_classes: usize,
	min_children: usize,
	max_children: usize,
	min_attributes: usize,
	max_attributes: usize,
	min_methods: usize,
	max_methods: usize,
	min_formals: usize,
	max_formals: usize,
	p_void: f64,
	p_method_override: f64,
	p_formal_shadow: f64
    ) -> Self {
	Self {
	    n_classes,
	    min_children,
	    max_children,
	    min_attributes,
	    max_attributes,
	    min_methods,
	    max_methods,
	    min_formals,
	    max_formals,
	    p_void,
	    p_method_override,
	    p_formal_shadow
	}
    }
}

impl Default for ProgramMutationConfig {
    fn default() -> Self {
	Self::new(
	    10,
	    1,
	    2,
	    0,
	    5,
	    0,
	    5,
	    0,
	    3,
	    0.0,
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
    method_generator: ShallowMethodGenerator<ShadowingMethodIDGenerator, SubtypeGenerator>
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
	    )
	}
    }
}

impl Mutator<Program, ()> for ProgramMutator {
    fn mutate(&mut self, prog: &mut Program, _: ()) {
	// Generate hierarchy
	let mut q: VecDeque<Idx<Class>> = VecDeque::new();
	let obj_sym = prog.to_sym("Object");
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
		    let generation_attempt = self.class_name_generator.generate((), &mut Environment::new(None, prog));
		    if let Some(name) = generation_attempt {
			let child = prog.add_class(Some(parent_sym), Class::new(name, false));
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
		for _ in 0..n_attributes {
		    let generation_attempt = self.attribute_generator.generate((), &mut Environment::new(Some(next_idx), prog));
		    if let Some(attr) = generation_attempt {
			prog.get_class_mut(next_idx).attributes.push(attr);
		    }
		}

		// Generate methods
		let n_methods: usize = self.rng.random_range(self.config.min_methods..self.config.max_methods);
		for _ in 0..n_methods {
		    let generation_attempt = self.method_generator.generate((), &mut Environment::new(Some(next_idx), prog));
		    if let Some(meth) = generation_attempt {
			prog.get_class_mut(next_idx).methods.push(meth);
		    }
		}
	    }

	    for c in prog.get_class(next_idx).children.iter() {
		dfs_stack.push(*c);
	    }
	}
    }
}
