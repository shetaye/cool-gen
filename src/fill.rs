use crate::environment::Environment;
use crate::tree::{Type, Expr, Class, Program};
use crate::symbol::Symbol;
use crate::generator::GeneratorState;

pub trait Fillable {
    /// Return a new, filled version of yourself, consuming yourself in the process
    fn fill(self, environment: &mut Environment, state: &mut GeneratorState) -> Self;
}

impl Fillable for Program {
    fn fill(self, environment: &mut Environment, state: &mut GeneratorState) -> Program {
	// TODO: Implement me
	self
    }
}

impl Fillable for Class {
    fn fill(self, environment: &mut Environment, state: &mut GeneratorState) -> Class {
	// TODO: Implement me
	self
    }
}

impl Fillable for Expr {
    fn fill(self, environment: &mut Environment, state: &mut GeneratorState) -> Expr {
	    
	Expr::Hole(Type::SelfType)
    }
}
