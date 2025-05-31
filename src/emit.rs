struct Emitter {
    indent: usize,
    output: String
}

impl Emitter {
    fn new() -> Self {
	Self {
	    indent: 0,
	    output: String::new()
	}
    }

    fn emit_inline(&mut self, s: &str) {
	self.output.push_str(s);
    }

    fn emit_block(&mut self, s: &str) {
	self.output.push('\n');
	for _ in 0..self.indent {
	    self.output.push_str("  ");
	}
	self.output.push_str(s);
    }

    fn enter_block(&mut self) {
	self.indent += 1
    }

    fn exit_block(&mut self) {
	self.indent -= 1
    }
}

trait Emittable {
    fn emit(&self, e: &mut Emitter);
}
