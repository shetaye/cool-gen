use std::marker::PhantomData;
use std::str::FromStr;
use std::collections::HashSet;

use crate::environment::Environment;
use crate::tree::*;
use crate::symbol::Symbol;
use rand::prelude::*;

type RNG = rand::rngs::ThreadRng;

pub trait Generator<T, C> {
    fn generate(&mut self, constraint: C, environment: &mut Environment) -> Option<T>;
}

/* ~~~ actual generators below this line ~~~ */

// Symbols

const METHOD_DICT: &'static [&'static str] = &[
    "act", "berate", "cut", "dent", "exit", "flunk", "gut", "happen",
    "include", "jit", "kill", "lie", "make", "nuke", "oowoomp", "prep",
    "quarry", "romp", "skip", "touch", "upskill", "vindictae",
    "whine", "xerox", "yank", "zip"
];

pub struct ShadowingMethodIDGenerator {
    rng: RNG,
    p_shadow: f64
}

impl ShadowingMethodIDGenerator {
    pub fn new(p_shadow: f64) -> Self {
        Self {
            p_shadow,
            rng: rand::rng(),
        }
    }
}

impl Generator<Symbol, ()> for ShadowingMethodIDGenerator {
    fn generate(&mut self, constraint: (), environment: &mut Environment) -> Option<Symbol> {
	let shadow = self.rng.random_bool(self.p_shadow);

	let mut valid: Vec<Symbol>;
	if shadow {
	    // Only select from in-scope items that are not in local scope
	    valid = environment.enumerate_methods()
		.iter()
		.map(|(sym, _)| sym.clone())
		.collect();
	    // Have to do this seperately bc environment is used twice
	    valid = valid
		.iter()
		.filter(|sym| !environment.peek_method(sym))
		.map(|sym| sym.clone())
		.collect();
	} else {
	    // Only select from dictionary items not used in-scope
	    valid = METHOD_DICT 
		.iter()
		.map(|s| environment.to_sym(s))
		.collect();
	    // Have to do this seperately bc environment is used twice
	    valid = valid
		.iter()
		.filter(|sym| environment.lookup_method(sym).is_none())
		.map(|sym| sym.clone())
		.collect();
	    
	}
	match valid.len() {
	    0 => None,
	    l => Some(valid[self.rng.random_range(0..l)])
	}
    }
}


const OBJECTID_DICT: &'static [&'static str] = &[
    "alpha", "bravo", "charlie", "delta", "echo", "foxtrot",
    "golf", "hotel", "india", "juliett", "kilo", "lima",
    "mike", "november", "oscar", "papa", "quebec", "romeo",
    "sierra", "tango", "uniform", "victor", "whiskey",
    "xray", "yankee", "zulu"
];


pub struct FreshObjectIDGenerator {
    rng: RNG
}

impl FreshObjectIDGenerator {
    pub fn new() -> Self { Self { rng: rand::rng() } }
}

impl Generator<Symbol, ()> for FreshObjectIDGenerator {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Symbol> {
	// Only select from dictionary items not used in-scope
	let mut valid: Vec<Symbol> = OBJECTID_DICT
	    .iter()
	    .map(|s| environment.to_sym(s))
	    .collect();
	// Have to do this seperately bc environment is used twice
	valid = valid
	    .iter()
	    .filter(|sym| environment.lookup_binding(sym).is_none())
	    .map(|sym| sym.clone())
	    .collect();
	match valid.len() {
	    0 => None,
	    l => Some(valid[self.rng.random_range(0..l)])
	}
    }
}

pub struct ShadowingObjectIDGenerator {
    rng: RNG,
    p_shadow: f64
}

impl ShadowingObjectIDGenerator {
    pub fn new(p_shadow: f64) -> Self {
        Self {
            p_shadow,
            rng: rand::rng(),
        }
    }
}

impl Generator<Symbol, ()> for ShadowingObjectIDGenerator {
    fn generate(&mut self, constraint: (), environment: &mut Environment) -> Option<Symbol> {
	let shadow = self.rng.random_bool(self.p_shadow);

	let mut valid: Vec<Symbol>;
	if shadow {
	    // Only select from in-scope items that are not in local scope
	    valid = environment.enumerate_bindings()
		.iter()
		.map(|(sym, _)| sym.clone())
		.collect();
	    // Have to do this seperately bc environment is used twice
	    valid = valid
		.iter()
		.filter(|sym| environment.peek_binding(sym).is_none())
		.map(|sym| sym.clone())
		.collect();
	} else {
	    // Only select from dictionary items not used in-scope
	    valid = OBJECTID_DICT
		.iter()
		.map(|s| environment.to_sym(s))
		.collect();
	    // Have to do this seperately bc environment is used twice
	    valid = valid
		.iter()
		.filter(|sym| environment.lookup_binding(sym).is_none())
		.map(|sym| sym.clone())
		.collect();
	    
	}
	match valid.len() {
	    0 => None,
	    l => Some(valid[self.rng.random_range(0..l)])
	}
    }
}

const CLASS_DICT: &'static [&'static str] = &[
    "Apple", "Banana", "Coconut", "Dog", "Epsilon",
    "Fish", "Giraffe", "Hospital", "Igloo", "Jemoka",
    "Kangaroo", "Lemon", "Mango", "Nut", "OptionsPricingModel",
    "Pricing", "Quant", "Rust", "Stanford", "Triangle",
    "Umbrella", "VTuber", "Wisconsin", "Xylophone", "Yogurt",
    "Zepto"
];

/// Generates a fresh class name
pub struct ClassNameGenerator {
    rng: RNG,
}

impl ClassNameGenerator {
    pub fn new() -> Self {
	Self {
	    rng: rand::rng(),
	}
    }
}

impl Generator<Symbol, ()> for ClassNameGenerator {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Symbol> {
	let dictionary: HashSet<Symbol> = HashSet::from_iter(
	    CLASS_DICT
	    .iter()
	    .map(|s| environment.to_sym(s)));
	
	let existing = HashSet::from_iter(
	    environment.classes()
		.iter()
		.map(|(s, idx)| s.clone()));

	let valid: Vec<&Symbol> = dictionary.difference(&existing).collect();
	match valid.len() {
	    0 => None,
	    l => Some(valid[self.rng.random_range(..l)].clone())
	}
    }
}

pub struct ShallowAttributeGenerator<N: Generator<Symbol, ()>, T: Generator<Type, Type>> {
    rng: RNG,
    name_generator: N,
    type_generator: T,
    p_void: f64
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> ShallowAttributeGenerator<N,T> {
    pub fn new(name_generator: N, type_generator: T, p_void: f64) -> Self {
	Self { rng: rand::rng(), name_generator, type_generator, p_void }
    }
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> Generator<Attribute, ()> for ShallowAttributeGenerator<N, T> {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Attribute> {
	let name = self.name_generator.generate((), environment)?;

	// Can be any type
	let obj_sym = environment.to_sym("Object");
	let type_ = self.type_generator.generate(Type::Concrete(obj_sym), environment)?;

	// Whether we generate a void
	let void = self.rng.random_bool(self.p_void);

	let body = if void { None } else { Some(Expr::Hole(type_)) };

	Some(Attribute {
	    name,
	    type_,
	    body
	})
    }
}

pub struct FormalsGenerator<N: Generator<Symbol, ()>, T: Generator<Type, Type>> {
    name_generator: N,
    type_generator: T
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> FormalsGenerator<N,T> {
    pub fn new(name_generator: N, type_generator: T) -> Self {
	Self { name_generator, type_generator }
    }
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> Generator<Formal, ()> for FormalsGenerator<N,T> {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Formal> {
	// Assume that the only things in our scope are other formals
	let name = self.name_generator.generate((), environment)?;

	// Can be any type
	let obj_sym = environment.to_sym("Object");
	let type_ = self.type_generator.generate(Type::Concrete(obj_sym), environment)?;

	Some(Formal { name, type_ })
    }
}

pub struct ShallowMethodGenerator<N: Generator<Symbol, ()>, T: Generator<Type, Type>> {
    rng: RNG,
    name_generator: N,
    type_generator: T,
    min_formals: usize,
    max_formals: usize,
    formal_generator: FormalsGenerator<ShadowingObjectIDGenerator, SubtypeGenerator>
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> ShallowMethodGenerator<N,T> {
    pub fn new(name_generator: N, type_generator: T, min_formals: usize, max_formals: usize, p_shadow: f64) -> Self {
	Self {
	    rng: rand::rng(),
	    name_generator,
	    type_generator,
	    min_formals,
	    max_formals,
	    formal_generator: FormalsGenerator::new(
		ShadowingObjectIDGenerator::new(p_shadow),
		SubtypeGenerator::new()
	    )
	}
    }
}

impl<N: Generator<Symbol, ()>, T: Generator<Type, Type>> Generator<Method, ()> for ShallowMethodGenerator<N,T> {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Method> {
	

	// Generate name & type
	let name = self.name_generator.generate((), environment)?;

	// Respect override rules
	if let Some(declaring_class_sym) = environment.lookup_method(&name) {
	    let declaring_class = environment.get_class(environment.lookup_class(declaring_class_sym).unwrap());
	    let overriden_method = declaring_class.methods.iter().find(|a| a.name == name).unwrap();
	    Some(Method {
		name,
		ret_type: overriden_method.ret_type,
		formals: overriden_method.formals.iter().map(|f| *f).collect(),
		body: Expr::Hole(overriden_method.ret_type)
	    })
	} else {
	    // Generate formals in their own scope
	    environment.push_scope();
	    let mut formals: Vec<Formal> = vec![];
	    let n_formals = self.rng.random_range(self.min_formals..self.max_formals);
	    for _ in 0..n_formals {
		if let Some(f) = self.formal_generator.generate((), environment) {
		    formals.push(f);
		}
	    }
	    environment.pop_scope();

	    // Can be any type
	    let obj_sym = environment.to_sym("Object");
	    let type_ = self.type_generator.generate(Type::Concrete(obj_sym), environment)?;

	    Some(Method {
		name,
		ret_type: type_,
		formals,
		body: Expr::Hole(type_)
	    })
	}
    }
}

pub struct SubtypeGenerator {
    rng: RNG
}

impl SubtypeGenerator {
    pub fn new() -> Self { Self { rng: rand::rng() } }
}

impl Generator<Type, Type> for SubtypeGenerator {
    fn generate(&mut self, supertype: Type, environment: &mut Environment) -> Option<Type> {
	let subtypes = environment.subtypes_of(supertype);
	match subtypes.len() {
	    0 => None,
	    l => Some(subtypes[self.rng.random_range(0..l)])
	}
    }
}

// String

pub struct StringGenerator<'a> {
    rng: &'a mut RNG
}

impl Generator<Expr, Type> for StringGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        Some(Expr::String(String::from_str(
            vec![
                "The quick brown fox jumps over the lazy dog.",
                "Apple.",
                "Five corpulent porpoises"
            ].choose(self.rng)?
        ).ok()?))
    }
}

// Int

pub struct IntGenerator<'a> {
    rng: &'a mut RNG
}

impl Generator<Expr, Type> for IntGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        Some(Expr::Int(self.rng.random::<i64>()))
    }
}

// Bool

pub struct BoolGenerator<'a> {
    rng: &'a mut RNG
}

impl Generator<Expr, Type> for BoolGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        Some(Expr::Bool(self.rng.random::<bool>()))
    }
}

// Assign

pub struct AssignGenerator<'a, T: Generator<Symbol, Option<Vec<Symbol>>>, E: Generator<Expr, Type>> {
    symbol_generator: T,
    expression_generator: E,
    rng: &'a mut RNG
}

impl<T: Generator<Symbol, Option<Vec<Symbol>>>, E: Generator<Expr, Type>> Generator<Expr, Type> for AssignGenerator<'_, T, E> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        let (n, t) = environment
                      .enumerate_bindings()
                      .iter()
                      .filter(|x| {x.1 == goal_type})
                      .choose(self.rng)?;

        Some(Expr::Assignment {
            to: self.symbol_generator.generate(None, environment)?,
            val: Box::new(
                self.expression_generator.generate(goal_type, environment)?
            )
        })
    }
}



// Dispatch

// pub struct DispatchGenerator<T: SymbolGenerator, E: ExpressionGenerator> {
//     symbol_generator: T,
//     expression_generator: E
// }

// impl<T: SymbolGenerator, E: ExpressionGenerator> ExpressionGenerator for AssignGenerator<'_, T, E> {
//     fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
//         Some(Expr::Assignment {
//             to: self.symbol_generator.generate(environment),
//             val: Box::new(
//                 self.expression_generator.generate(goal_type, environment)?
//             )
//         })
//     }
// }
