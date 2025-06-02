mod symbol;
mod tree;
mod generator;
mod environment;
mod emit;
mod mutator;

use rand::prelude::*;

use crate::symbol::SymbolTable;
use crate::tree::Program;
use crate::emit::{Emitter, Emittable};
use crate::mutator::{Mutator, ProgramMutator, ProgramMutationConfig};

fn main() {
    let mut st = SymbolTable::new();
    let mut c = Program::new(&mut st);
    let mut m = ProgramMutator::new(ProgramMutationConfig::default());
    m.mutate(&mut c, &mut st);
    let mut e = Emitter::new();
    c.emit(&mut e, &st);
    println!("{}", e.emit());
}
