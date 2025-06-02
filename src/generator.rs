use std::marker::PhantomData;
use std::str::FromStr;
use std::collections::HashSet;

use crate::environment::Environment;
use crate::tree::*;
use crate::symbol::{MethodSymbol, ObjectSymbol, ClassSymbol};
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

impl Generator<MethodSymbol, ()> for ShadowingMethodIDGenerator {
    fn generate(&mut self, constraint: (), environment: &mut Environment) -> Option<MethodSymbol> {
	let shadow = self.rng.random_bool(self.p_shadow);

	let mut valid: Vec<MethodSymbol>;
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
		.map(|s| environment.to_sym(s).into())
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

impl Generator<ObjectSymbol, ()> for FreshObjectIDGenerator {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<ObjectSymbol> {
	// Only select from dictionary items not used in-scope
	let mut valid: Vec<ObjectSymbol> = OBJECTID_DICT
	    .iter()
	    .map(|s| environment.to_sym(s).into())
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

impl Generator<ObjectSymbol, ()> for ShadowingObjectIDGenerator {
    fn generate(&mut self, constraint: (), environment: &mut Environment) -> Option<ObjectSymbol> {
	let shadow = self.rng.random_bool(self.p_shadow);

	let mut valid: Vec<ObjectSymbol>;
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
		.map(|s| environment.to_sym(s).into())
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

impl Generator<ClassSymbol, ()> for ClassNameGenerator {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<ClassSymbol> {
	let dictionary: HashSet<ClassSymbol> = HashSet::from_iter(
	    CLASS_DICT
	    .iter()
	    .map(|s| environment.to_sym(s).into()));
	
	let existing = HashSet::from_iter(
	    environment.classes()
		.iter()
		.map(|(s, idx)| s.clone()));

	let valid: Vec<&ClassSymbol> = dictionary.difference(&existing).collect();
	match valid.len() {
	    0 => None,
	    l => Some(valid[self.rng.random_range(..l)].clone())
	}
    }
}

pub struct ShallowAttributeGenerator<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> {
    rng: RNG,
    name_generator: N,
    type_generator: T,
    p_void: f64
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> ShallowAttributeGenerator<N,T> {
    pub fn new(name_generator: N, type_generator: T, p_void: f64) -> Self {
	Self { rng: rand::rng(), name_generator, type_generator, p_void }
    }
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> Generator<Attribute, ()> for ShallowAttributeGenerator<N, T> {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Attribute> {
	let name = self.name_generator.generate((), environment)?;

	// Can be any type
	let obj_sym = environment.to_sym("Object");
	let type_ = self.type_generator.generate(Type::Concrete(obj_sym.into()), environment)?;

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

pub struct FormalsGenerator<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> {
    name_generator: N,
    type_generator: T
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> FormalsGenerator<N,T> {
    pub fn new(name_generator: N, type_generator: T) -> Self {
	Self { name_generator, type_generator }
    }
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> Generator<Formal, ()> for FormalsGenerator<N,T> {
    fn generate(&mut self, _: (), environment: &mut Environment) -> Option<Formal> {
	// Assume that the only things in our scope are other formals
	let name = self.name_generator.generate((), environment)?;

	// Can be any type
	let obj_sym = environment.to_sym("Object");
	let type_ = self.type_generator.generate(Type::Concrete(obj_sym.into()), environment)?;

	Some(Formal { name, type_ })
    }
}

pub struct ShallowMethodGenerator<N: Generator<MethodSymbol, ()>, T: Generator<Type, Type>> {
    rng: RNG,
    name_generator: N,
    type_generator: T,
    min_formals: usize,
    max_formals: usize,
    formal_generator: FormalsGenerator<ShadowingObjectIDGenerator, SubtypeGenerator>
}

impl<N: Generator<MethodSymbol, ()>, T: Generator<Type, Type>> ShallowMethodGenerator<N,T> {
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

impl<N: Generator<MethodSymbol, ()>, T: Generator<Type, Type>> Generator<Method, ()> for ShallowMethodGenerator<N,T> {
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
	    let type_ = self.type_generator.generate(Type::Concrete(obj_sym.into()), environment)?;

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
	let subtypes = environment.subtypes_of(supertype, true);
	match subtypes.len() {
	    0 => None,
	    l => Some(subtypes[self.rng.random_range(0..l)])
	}
    }
}

// Assign
pub struct AssignGenerator {
    rng: RNG
}

impl AssignGenerator {
    fn new() -> Self {
	Self {
	    rng: rand::rng()
	}
    }
}

impl Generator<Expr, Type> for AssignGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        let (n, t) = environment
                      .enumerate_bindings()
                      .into_iter()
                      .filter(|x| {x.1 == goal_type})
                      .choose(&mut self.rng)?;

        Some(Expr::Assignment {
            to: n,
            val: Box::new(
                Expr::Hole(goal_type)
            )
        })
    }
}

pub struct DispatchGenerator<T: Generator<Type, Type>> {
    rng: RNG,
    type_generator: T,
    p_self: f64,
    p_static: f64
}

impl<T: Generator<Type, Type>> DispatchGenerator<T> {
    fn new(type_generator: T, p_self: f64, p_static: f64) -> Self {
	Self {
	    type_generator,
	    rng: rand::rng(),
	    p_self,
	    p_static
	}
    }
}

impl<T: Generator<Type, Type>> Generator<Expr, Type> for DispatchGenerator<T> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
	let gen_self = self.rng.random_bool(self.p_self);
	if gen_self {
	    let method = environment.materialize_methods()
		.into_iter()
		.map(|(m,c)| {
		    let defining_class_idx = environment.lookup_class(c).unwrap();
		    let defining_class = environment.get_class(defining_class_idx);
		    defining_class.get_meth(m).unwrap()
		})
		.filter(|m| m.ret_type == goal_type)
		.choose(&mut self.rng)?;

	    let formals = method.formals
		.iter()
		.map(|f| Expr::Hole(f.type_))
		.collect();

	    Some(Expr::Dispatch{
		on: None,
		at: None,
		name: method.name,
		formals
	    })
	} else {
	    let gen_static = self.rng.random_bool(self.p_static);

	    let (method, class) = environment.enumerate_methods()
		.into_iter()
		.map(|(m,c)| {
		    let defining_class_idx = environment.lookup_class(c).unwrap();
		    let defining_class = environment.get_class(defining_class_idx);
		    (defining_class.get_meth(m).unwrap(), c)
		})
		.filter(|(m,_)| m.ret_type == goal_type)
		.choose(&mut self.rng)?;

	    let formals = method.formals
		.iter()
		.map(|f| Expr::Hole(f.type_))
		.collect();

	    let dispatch_on = if gen_static {
		environment.subtypes_of(Type::Concrete(class), false).choose(&mut self.rng)?.clone()
	    } else {
		Type::Concrete(class)
	    };

	    let at = if gen_static { Some(dispatch_on) } else { None };

	    Some(Expr::Dispatch{
		on: Some(Box::new(Expr::Hole(dispatch_on))),
		at,
		name: method.name,
		formals
	    })
	}
    }
}


pub struct IfGenerator {
    rng: RNG,
}

impl IfGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for IfGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
        let bool_type = Type::Concrete(bool_sym);

	let subtypes = environment.subtypes_of(goal_type, true);
        let cond_hole = Expr::Hole(bool_type);
        let then_hole = Expr::Hole(*subtypes.choose(&mut self.rng)?);
        let else_hole = Expr::Hole(*subtypes.choose(&mut self.rng)?);

        // 3) Construct the If‐expression
        Some(Expr::If {
            condition: Box::new(cond_hole),
            then:      Box::new(then_hole),
            else_:     Box::new(else_hole),
        })
    }
}

pub struct ConstantGenerator {
    rng: RNG,
    // A small set of example strings to choose from
    example_strings: Vec<&'static str>,
}

impl ConstantGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
            example_strings: vec![
                "The quick brown fox jumps over the lazy dog.",
                "Apple.",
                "Five corpulent porpoises",
                "Hello, World!",
                "Cool language!"],
        }
    }
}

impl Generator<Expr, Type> for ConstantGenerator {
    fn generate(&mut self, goal_type: Type, _environment: &mut Environment) -> Option<Expr> {
        match goal_type {
            // If the goal is exactly String, produce a random string literal
            Type::Concrete(sym) if sym == _environment.to_sym("String").into() => {
                let choice = self
                    .example_strings
                    .choose(&mut self.rng)?
                    .to_string();
                Some(Expr::String(choice))
            }

            // If the goal is exactly Int, produce a random i32 literal
            Type::Concrete(sym) if sym == _environment.to_sym("Int").into() => {
                let value = self.rng.gen::<i32>();
                Some(Expr::Int(value))
            }

            // If the goal is exactly Bool, produce a random boolean literal
            Type::Concrete(sym) if sym == _environment.to_sym("Bool").into() => {
                let value = self.rng.gen::<bool>();
                Some(Expr::Bool(value))
            }

            // For SelfType or any other class type, emit `new <type>`
            _ => Some(Expr::New(goal_type)),
        }
    }
}

pub struct LoopGenerator {
    rng: RNG,
}

impl LoopGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for LoopGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // 1) Build a “Bool” Type
        let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
        let bool_type = Type::Concrete(bool_sym);

        // 2) Make the condition hole of type Bool, and the body hole of goal_type
        let cond_hole = Expr::Hole(bool_type);
        let body_hole = Expr::Hole(goal_type);

        // 3) Construct the Loop‐expression
        Some(Expr::Loop {
            condition: Box::new(cond_hole),
            body:      Box::new(body_hole),
        })
    }
}

pub struct BlockGenerator {
    rng: RNG,
    min_len: usize,
    max_len: usize,
}

impl BlockGenerator {
    /// `min_len` and `max_len` control how many expressions appear in the block.
    /// They must satisfy `min_len >= 1` and `max_len >= min_len`.
    pub fn new(min_len: usize, max_len: usize) -> Self {
        assert!(min_len >= 1 && max_len >= min_len);
        Self {
            rng: rand::rng(),
            min_len,
            max_len,
        }
    }
}

impl Generator<Expr, Type> for BlockGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Decide how many sub‐expressions to put in this Block
        let len = self.rng.random_range(self.min_len..=self.max_len);

        // Gather all concrete subtypes of Object so that earlier holes can be any of them.
        let object_sym: ClassSymbol = environment.to_sym("Object").into();
        let all_subs = environment.subtypes_of(Type::Concrete(object_sym), true);

        // If there are no subtypes of Object, we’ll just do a 1‐element block.
        if all_subs.is_empty() {
            return Some(Expr::Block(vec![Expr::Hole(goal_type)]));
        }

        let mut exprs = Vec::with_capacity(len);
        // For the first len−1 expressions, pick a random subtype and make a hole.
        for _ in 0..(len - 1) {
            let t = all_subs.choose(&mut self.rng).unwrap().clone();
            exprs.push(Expr::Hole(t));
        }
        // Last expression must have type = goal_type
        exprs.push(Expr::Hole(goal_type));

        Some(Expr::Block(exprs))
    }
}

pub struct LetGenerator<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> {
    rng: RNG,
    name_generator: N,
    type_generator: T,
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> LetGenerator<N, T> {
    pub fn new(name_generator: N, type_generator: T) -> Self {
        Self {
            rng: rand::rng(),
            name_generator,
            type_generator,
        }
    }
}

impl<N: Generator<ObjectSymbol, ()>, T: Generator<Type, Type>> Generator<Expr, Type>
    for LetGenerator<N, T>
{
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        let name: ObjectSymbol = self.name_generator.generate((), environment)?;

        let binding_type: Type = self.type_generator.generate(Type::Concrete(environment.to_sym("Object").into()), environment)?;

        let initializer = Expr::Hole(binding_type);

        let body = Expr::Hole(goal_type);

        environment.pop_scope();

        Some(Expr::Let {
            binding: name,
            type_: binding_type,
            initializer: Box::new(initializer),
            body: Box::new(body),
        })
    }
}

pub struct CaseGenerator<N: Generator<ObjectSymbol, ()>> {
    rng: RNG,
    name_generator: N,
    min_arms: usize,
    max_arms: usize,
}

impl<N: Generator<ObjectSymbol, ()>> CaseGenerator<N> {
    /// `min_arms` ≥ 1, `max_arms` ≥ `min_arms`.
    pub fn new(name_generator: N, min_arms: usize, max_arms: usize) -> Self {
        assert!(min_arms >= 1 && max_arms >= min_arms);
        Self {
            rng: rand::rng(),
            name_generator,
            min_arms,
            max_arms,
        }
    }
}

impl<N: Generator<ObjectSymbol, ()>> Generator<Expr, Type> for CaseGenerator<N> {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // 1) Collect all concrete subtypes of `goal_type` (including `goal_type` itself).
        let mut candidates = environment.subtypes_of(goal_type, true);
        if candidates.is_empty() {
            // If there are no subtypes to choose from, we can't build any arms.
            return None;
        }

        // 2) Decide how many arms to make
        let mut num_arms = self.rng.random_range(self.min_arms..=self.max_arms);
        if num_arms > candidates.len() {
            num_arms = candidates.len();
        }

        // 3) Randomly pick `num_arms` distinct types from `candidates`
        candidates.shuffle(&mut self.rng);
        let chosen_types: Vec<Type> = candidates.into_iter().take(num_arms).collect();

        // 4) For each chosen type, build a CaseArm:
        //    - generate a fresh name
        //    - assign that type
        //    - body is a Hole of that type
        let mut arms = Vec::with_capacity(num_arms);
        for arm_type in chosen_types {
            // 4a) Generate a fresh binding name
            let name: ObjectSymbol = self.name_generator.generate((), environment)?;
            // 4b) Body is just a hole of `arm_type`
            let body_hole = Expr::Hole(arm_type);
            // 4c) Construct the CaseArm
            arms.push(CaseArm {
                name,
                type_: arm_type,
                body: Box::new(body_hole),
            });
        }

        // 5) Return the Case expression
        Some(Expr::Case(arms))
    }
}

pub struct ArithmeticGenerator {
    rng: RNG,
}

impl ArithmeticGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for ArithmeticGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Only produce an arithmetic expression if the goal type is exactly Int
        let int_sym: ClassSymbol = environment.to_sym("Int").into();
        if goal_type != Type::Concrete(int_sym) {
            return None;
        }

        // Randomly pick one of the four arithmetic ops
        let op = match self.rng.random_range(0..4) {
            0 => ArithmeticOp::Plus,
            1 => ArithmeticOp::Minus,
            2 => ArithmeticOp::Multiply,
            _ => ArithmeticOp::Divide,
        };

        // Both operands are holes of type Int
        let lhs = Expr::Hole(Type::Concrete(int_sym));
        let rhs = Expr::Hole(Type::Concrete(int_sym));

        Some(Expr::Arithmetic {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }
}

pub struct ComplementGenerator {}

impl ComplementGenerator {
    pub fn new() -> Self { Self {} }
}

impl Generator<Expr, Type> for ComplementGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Only produce a complement expression if the goal type is exactly Int
        let int_sym: ClassSymbol = environment.to_sym("Int").into();
        if goal_type != Type::Concrete(int_sym) {
            return None;
        }

        // Subexpression is a hole of type Int
        let inner = Expr::Hole(Type::Concrete(int_sym));

        Some(Expr::Complement(Box::new(inner)))
    }
}

/// Generates an `Expr::Not` (logical negation) node of type Bool:
///   `not <subexpr>`
/// where `<subexpr>` is a hole of type Bool.
pub struct NotGenerator { }

impl NotGenerator {
    pub fn new() -> Self { Self {} }
}

impl Generator<Expr, Type> for NotGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Only produce a `not` expression if the goal type is exactly Bool
        let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
        if goal_type != Type::Concrete(bool_sym) {
            return None;
        }

        // Subexpression is a hole of type Bool
        let inner = Expr::Hole(Type::Concrete(bool_sym));

        Some(Expr::Not(Box::new(inner)))
    }
}

pub struct IsvoidGenerator {
    rng: RNG,
}

impl IsvoidGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for IsvoidGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Only generate isvoid if the goal type is Bool
        let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
        if goal_type != Type::Concrete(bool_sym) {
            return None;
        }

        // Pick a random subtype of Object (including Object itself) for the inner expression
        let object_sym: ClassSymbol = environment.to_sym("Object").into();
        let mut candidates = environment.subtypes_of(Type::Concrete(object_sym), true);
        if candidates.is_empty() {
            // If no subtypes, default to Object
            candidates.push(Type::Concrete(object_sym));
        }
        let chosen = candidates.choose(&mut self.rng).unwrap().clone();

        // Build `isvoid (Hole(chosen))`
        let inner = Expr::Hole(chosen);
        Some(Expr::Isvoid(Box::new(inner)))
    }
}

/// Generates an `Expr::Comparison` node of type Bool:
///   `<lhs> op <rhs>`
/// where `op` is one of:
///   - `<` or `<=`: both operands are holes of type Int
///   - `=`: operands are holes of a randomly chosen comparable type (Int, Bool, or String)
pub struct ComparisonGenerator {
    rng: RNG,
}

impl ComparisonGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for ComparisonGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Only generate comparison if the goal type is Bool
        let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
        if goal_type != Type::Concrete(bool_sym) {
            return None;
        }

        // Prepare the three comparison operations
        let ops = [ComparisonOp::LessThan, ComparisonOp::LessThanEqual, ComparisonOp::Equal];
        // Choose one at random
        let op = *ops.choose(&mut self.rng).unwrap();

        match op {
            ComparisonOp::LessThan | ComparisonOp::LessThanEqual => {
                // Both sides must be Int
                let int_sym: ClassSymbol = environment.to_sym("Int").into();
                let int_t = Type::Concrete(int_sym);
                let lhs = Expr::Hole(int_t);
                let rhs = Expr::Hole(int_t);
                Some(Expr::Comparison {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            ComparisonOp::Equal => {
                // Pick a random comparable type among Int, Bool, String
                let int_sym: ClassSymbol = environment.to_sym("Int").into();
                let bool_sym: ClassSymbol = environment.to_sym("Bool").into();
                let str_sym: ClassSymbol = environment.to_sym("String").into();
                let choices = [
                    Type::Concrete(int_sym),
                    Type::Concrete(bool_sym),
                    Type::Concrete(str_sym),
                ];
                let chosen_t = *choices.choose(&mut self.rng).unwrap();
                let lhs = Expr::Hole(chosen_t);
                let rhs = Expr::Hole(chosen_t);
                Some(Expr::Comparison {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
        }
    }
}


pub struct VariableGenerator {
    rng: RNG,
}

impl VariableGenerator {
    pub fn new() -> Self {
        Self {
            rng: rand::rng(),
        }
    }
}

impl Generator<Expr, Type> for VariableGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
        // Collect all bindings whose type == goal_type
        let mut candidates: Vec<ObjectSymbol> = environment
            .enumerate_bindings()
            .into_iter()
            .filter_map(|(sym, ty)| if ty == goal_type { Some(sym) } else { None })
            .collect();

        if candidates.is_empty() {
            None
        } else {
            let choice = candidates[self.rng.random_range(0..candidates.len())];
            Some(Expr::Variable(choice))
        }
    }
}

pub struct ExpressionGenerator {
    rng: RNG,
    generators: Vec<Box<dyn Generator<Expr, Type>>>,
    fallback: Box<dyn Generator<Expr, Type>>
}

impl ExpressionGenerator {
    pub fn new(
	p_dispatch_self: f64,
	p_dispatch_static: f64,
	p_let_shadow: f64,
	p_case_shadow: f64,
	min_block: usize,
	max_block: usize,
	min_case: usize,
	max_case: usize) -> Self {
	Self {
	    rng: rand::rng(),
	    generators: vec![
		Box::new(AssignGenerator::new()),
		Box::new(DispatchGenerator::new(SubtypeGenerator::new(), p_dispatch_self, p_dispatch_static)),
		Box::new(IfGenerator::new()),
		Box::new(LoopGenerator::new()),
		Box::new(BlockGenerator::new(min_block, max_block)),
		Box::new(LetGenerator::new(ShadowingObjectIDGenerator::new(p_let_shadow), SubtypeGenerator::new())),
		Box::new(CaseGenerator::new(ShadowingObjectIDGenerator::new(p_case_shadow), min_case, max_case)),
		Box::new(ArithmeticGenerator::new()),
		Box::new(ComplementGenerator::new()),
		Box::new(NotGenerator::new()),
		Box::new(IsvoidGenerator::new()),
		Box::new(ComparisonGenerator::new()),
		Box::new(VariableGenerator::new())
	    ],
	    fallback: Box::new(ConstantGenerator::new())
	}
    }

    pub fn fallback(&mut self) -> &mut Box<dyn Generator<Expr, Type>> {
	&mut self.fallback
    }
}



impl Generator<Expr, Type> for ExpressionGenerator {
    fn generate(&mut self, goal_type: Type, environment: &mut Environment) -> Option<Expr> {
	// Generate a random attempt order
	self.generators.shuffle(&mut self.rng);

	// Try each
	for g in self.generators.iter_mut() {
	    if let Some(e) = g.generate(goal_type, environment) {
		return Some(e);
	    }
	}

	self.fallback.generate(goal_type, environment)
    }
}
