mod symbol;
mod tree;
mod generator;
mod environment;
mod emit;
mod mutator;

pub use crate::symbol::SymbolTable;
pub use crate::tree::Program;
pub use crate::mutator::{Mutator, ProgramMutator, ProgramMutationConfig};
pub use crate::emit::Emit;
pub use crate::environment::Environment;

fn main() {
    let mut st = SymbolTable::new();
    let mut c = Program::new(&mut st);
    let mut m = ProgramMutator::new(ProgramMutationConfig::default());
    m.mutate(&mut c, &mut st);
    println!("{}", c.emit(&st, &c));
}
