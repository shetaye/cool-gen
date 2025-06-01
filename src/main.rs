mod symbol;
mod tree;
mod generator;
mod environment;
mod emit;
mod mutator;

use rand::prelude::*;

use crate::tree::Program;
use crate::emit::{Emitter, Emittable};
use crate::mutator::{Mutator, ProgramMutator, ProgramMutationConfig};

fn main() {
    let mut c = Program::new();
    let mut m = ProgramMutator::new(ProgramMutationConfig::default());
    m.mutate(&mut c, ());
    let mut e = Emitter::new();
    c.emit(&mut e, ());
    println!("{}", e.emit());
}
