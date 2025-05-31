use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(usize);

pub struct SymbolTable {
    symbols: Vec<String>,
    string_to_symbol: HashMap<String, Symbol>
}

impl SymbolTable {
    pub fn new() -> Self {
	Self {
	    symbols: Vec::new(),
	    string_to_symbol: HashMap::new()
	}
    }

    pub fn insert(&mut self, s: String) -> Symbol {
	let i = self.symbols.len();
	self.symbols.push(s);
	Symbol(i)
    }

    pub fn insert_ref(&mut self, s: &str) -> Symbol {
	self.insert(String::from(s))
    }

    pub fn lookup(&self, s: &str) -> Option<Symbol> {
	self.string_to_symbol.get(s).cloned()
    }

    pub fn get(&self, sym: Symbol) -> &str {
	&self.symbols[sym.0]
    }
}
