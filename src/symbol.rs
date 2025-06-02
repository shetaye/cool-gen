use std::collections::HashMap;
use std::convert::From;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(usize);

impl From<ClassSymbol> for Symbol {
    fn from(item: ClassSymbol) -> Self { item.0 }
}
impl From<ObjectSymbol> for Symbol {
    fn from(item: ObjectSymbol) -> Self { item.0 }
}
impl From<MethodSymbol> for Symbol {
    fn from(item: MethodSymbol) -> Self { item.0 }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassSymbol(Symbol);

impl From<Symbol> for ClassSymbol {
    fn from(item: Symbol) -> Self { Self(item) }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectSymbol(Symbol);

impl From<Symbol> for ObjectSymbol {
    fn from(item: Symbol) -> Self { Self(item) }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodSymbol(Symbol);

impl From<Symbol> for MethodSymbol {
    fn from(item: Symbol) -> Self { Self(item) }
}



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
	self.string_to_symbol.insert(s.clone(), Symbol(i));
	self.symbols.push(s);
	Symbol(i)
    }

    pub fn to_sym(&mut self, name: &str) -> Symbol {
	match self.lookup(name) {
	    Some(s) => s,
	    None => self.insert_ref(name)
	}
    }

    pub fn from_sym(&self, sym: Symbol) -> &str {
	&self.symbols[sym.0]
    }

    pub fn insert_ref(&mut self, s: &str) -> Symbol {
	self.insert(String::from(s))
    }

    pub fn lookup(&self, s: &str) -> Option<Symbol> {
	self.string_to_symbol.get(s).copied()
    }
    
}
