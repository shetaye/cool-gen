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

pub fn generate_with_config(config: ProgramMutationConfig) -> String {
    let mut st = SymbolTable::new();
    let mut c = Program::new(&mut st);
    let mut m = ProgramMutator::new(config);
    m.mutate(&mut c, &mut st);
    c.emit(&st, &c)
}

pub fn generate() -> String {
    generate_with_config(ProgramMutationConfig::default())
}


