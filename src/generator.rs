use std::marker::PhantomData;

use crate::environment::Environment;
use crate::tree::{Type, Expr};
use crate::symbol::Symbol;

pub struct GeneratorState {}

pub trait SymbolGenerator {
    fn generate(environment: &Environment, state: &mut GeneratorState) -> Symbol;
    fn generate_many(n: usize, environment: &Environment, state: &mut GeneratorState) -> Vec<Symbol>;
}

pub trait TypeGenerator {
    fn generate(subtypes: Type, environment: &Environment, state: &mut GeneratorState) -> Type;
}

pub trait ExpressionGenerator {
    /// Generate a new expression that would typecheck to goal_type, otherwise return None
    fn generate(goal_type: Type, environment: &Environment, state: &mut GeneratorState) -> Option<Expr>;
}

pub struct AssignGenerator<S: SymbolGenerator> {
    symbol_generator: PhantomData<S>
}

impl<S: SymbolGenerator> ExpressionGenerator for AssignGenerator<S> {
    fn generate(goal_type: Type, environment: &Environment, state: &mut GeneratorState) -> Option<Expr> {
	S::generate(environment, state);
	Some(Expr::Hole(goal_type))
    }
}
