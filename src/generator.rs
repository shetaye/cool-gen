use std::marker::PhantomData;
use std::str::FromStr;

use crate::environment::Environment;
use crate::tree::{Type, Expr};
use crate::symbol::Symbol;
use rand::prelude::*;

type RNG = rand::rngs::ThreadRng;

pub struct GeneratorState {}

pub trait SymbolGenerator {
    fn generate(&mut self, environment: &Environment) -> Symbol;
    fn generate_many(&mut self, n: usize, environment: &Environment) -> Vec<Symbol>;
}

pub trait TypeGenerator {
    fn generate(&mut self, subtypes: Type, environment: &Environment) -> Type;
}

pub trait ExpressionGenerator {
    /// Generate a new expression that would typecheck to goal_type, otherwise return None
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr>;
}


/* ~~~ actual generators below this line ~~~ */ 

// String

pub struct StringGenerator<'a> {
    rng: &'a mut RNG
}

impl ExpressionGenerator for StringGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
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

impl ExpressionGenerator for IntGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
        Some(Expr::Int(self.rng.random::<i64>()))
    }
}

// Bool

pub struct BoolGenerator<'a> {
    rng: &'a mut RNG
}

impl ExpressionGenerator for BoolGenerator<'_> {
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
        Some(Expr::Bool(self.rng.random::<bool>()))
    }
}

// Assign

pub struct AssignGenerator<'a, T: SymbolGenerator, E: ExpressionGenerator> {
    symbol_generator: T,
    expression_generator: E,
    rng: &'a mut RNG
}

impl<T: SymbolGenerator, E: ExpressionGenerator> ExpressionGenerator for AssignGenerator<'_, T, E> {
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
        let (n, t) = (environment
                      .enumerate_bindings()
                      .iter()
                      .filter(|x| {x.1 == goal_type})
                      .choose(self.rng)?);

        Some(Expr::Assignment {
            to: self.symbol_generator.generate(environment),
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
