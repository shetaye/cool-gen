use std::marker::PhantomData;

use crate::environment::Environment;
use crate::tree::{Type, Expr};
use crate::symbol::Symbol;
use rand::prelude::*;

pub struct GeneratorState {}


pub trait Generator {
    fn get_generator(&mut self) -> 
}

pub trait SymbolGenerator: Generator {
    fn generate(&mut self, environment: &Environment) -> Symbol;
    fn generate_many(&mut self, n: usize, environment: &Environment) -> Vec<Symbol>;
}

pub trait TypeGenerator: Generator {
    fn generate(&mut self, subtypes: Type, environment: &Environment) -> Type;
}

pub trait ExpressionGenerator: Generator {
    /// Generate a new expression that would typecheck to goal_type, otherwise return None
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr>;
}


/* ~~~ actual generators below this line ~~~ */ 

pub struct AssignGenerator<T: SymbolGenerator, E: ExpressionGenerator> {
    symbol_generator: T,
    expression_generator: E
}

impl<T: SymbolGenerator, E: ExpressionGenerator> ExpressionGenerator for AssignGenerator<T, E> {
    fn generate(&mut self, goal_type: Type, environment: &Environment) -> Option<Expr> {
        Some(Expr::Assignment {
            to: self.symbol_generator.generate(environment),
            val: Box::new(
                self.expression_generator.generate(goal_type, environment)?
            )
        })
    }
}


