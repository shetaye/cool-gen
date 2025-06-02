use crate::symbol::*;
use crate::tree::*;

/// A small trait that every AST node implements.
/// We now pass in `&Program` (in addition to `&SymbolTable`) whenever we emit.
/// That extra `&Program` is only needed when emitting a `Class`, so that
/// we can look up a class’s parent in the arena.
pub trait Emit {
    fn emit(&self, st: &SymbolTable, prog: &Program) -> String;
}

////////////////////////////////////////////////////////////////////////////////
// 1) Helpers: emit for Type, ArithmeticOp, ComparisonOp, Formal, CaseArm
//    (none of these actually need to inspect `prog`, so they simply ignore it)
////////////////////////////////////////////////////////////////////////////////

impl Emit for Type {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        match self {
            Type::Void => "[void]".into(),
            Type::SelfType => "SELF_TYPE".into(),
            Type::Concrete(class_sym) => st.from_sym((*class_sym).into()).to_string(),
        }
    }
}

impl Emit for ArithmeticOp {
    fn emit(&self, _st: &SymbolTable, _prog: &Program) -> String {
        match self {
            ArithmeticOp::Plus     => "+".into(),
            ArithmeticOp::Minus    => "-".into(),
            ArithmeticOp::Multiply => "*".into(),
            ArithmeticOp::Divide   => "/".into(),
        }
    }
}

impl Emit for ComparisonOp {
    fn emit(&self, _st: &SymbolTable, _prog: &Program) -> String {
        match self {
            ComparisonOp::LessThan       => "<".into(),
            ComparisonOp::LessThanEqual  => "<=".into(),
            ComparisonOp::Equal          => "=".into(),
        }
    }
}

impl Emit for Formal {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        let name = st.from_sym(self.name.into());
        let ty   = self.type_.emit(st, _prog);
        format!("{}: {}", name, ty)
    }
}

impl Emit for CaseArm {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        // e.g.  x: Type => body
        let var_name = st.from_sym(self.name.into());
        let ty       = self.type_.emit(st, _prog);
        let body_str = self.body.emit(st, _prog);
        format!("{}: {} => {}", var_name, ty, body_str)
    }
}

////////////////////////////////////////////////////////////////////////////////
// 2) Emit for expressions (Expr), fully parenthesized + indented.
//    Expr never needs to know about `prog`—only about `st`—so we ignore `prog`.
////////////////////////////////////////////////////////////////////////////////

impl Expr {
    /// Internal helper: emit with a given indent level (0 = no indent).
    /// Each indent level = four spaces.
    fn emit_with_indent(&self, st: &SymbolTable, _prog: &Program, indent: usize) -> String {
        // Helper to produce `indent` × 4 spaces
        fn indent_str(level: usize) -> String {
            "    ".repeat(level)
        }

        let cur_indent   = indent_str(indent);
        let child_indent = indent_str(indent + 1);

        match self {
            Expr::String(s) => {
                // (
                //     "hello"
                // )
                let escaped = s.replace('"', "\\\"");
                format!(
                    "{}(\n{}\"{}\"\n{})",
                    cur_indent,
                    child_indent,
                    escaped,
                    cur_indent
                )
            }

            Expr::Int(i) => {
                // (
                //     42
                // )
                format!(
                    "{}(\n{}{}\n{})",
                    cur_indent,
                    child_indent,
                    i,
                    cur_indent
                )
            }

            Expr::Bool(b) => {
                let bool_str = if *b { "true" } else { "false" };
                format!(
                    "{}(\n{}{}\n{})",
                    cur_indent,
                    child_indent,
                    bool_str,
                    cur_indent
                )
            }

            Expr::Variable(obj_sym) => {
                let var_name = st.from_sym((*obj_sym).into());
                format!(
                    "{}(\n{}{}\n{})",
                    cur_indent,
                    child_indent,
                    var_name,
                    cur_indent
                )
            }

            Expr::Hole(ty) => {
                let ty_str = ty.emit(st, _prog);
                format!(
                    "{}(\n{}_<{}>_\n{})",
                    cur_indent,
                    child_indent,
                    ty_str,
                    cur_indent
                )
            }

            Expr::Assignment { to, val } => {
                // (
                //     x <-
                //     ( <val> )
                // )
                let var_name = st.from_sym((*to).into());
                let val_blk  = val.emit_with_indent(st, _prog, indent + 1);
                format!(
                    "{}(\n{}{} <-\n{}\n{})",
                    cur_indent,
                    child_indent,
                    var_name,
                    val_blk,
                    cur_indent
                )
            }
            Expr::Dispatch { on, at, name, formals } => {
                /*
                We want:

                1) No target, no @Type:
                <indent>method(

                2) Has target, no @Type:
                <indent>(
                <on-expr>
            )
                <indent>.method(

                3) Has target and @Type:
                <indent>(
                <on-expr>
            )
                <indent>@Type.method(

                4) (Optional) No target but @Type → treat as “self”:
                <indent>(
                self
            )
                <indent>@Type.method(
                 */

                // 1) “on” side: if Some(expr_on), emit that; if None but at=Some(_), treat as “self”; else skip.
                let on_block = if let Some(expr_on) = on {
                    // We have a real “on” expression.
                    expr_on.emit_with_indent(st, _prog, indent + 1)
                } else if at.is_some() {
                    // No explicit “on”, but we do have @Type.  So we dispatch on “self@Type.”
                    // Wrap “self” in a one‐line block.
                    format!(
                        "{}(\n{}self\n{})",
                        child_indent,
                        indent_str(indent + 2),
                        child_indent,
                    )
                } else {
                    // No on, no @Type → direct dispatch on self without even printing a “( self )” block.
                    String::new()
                };

                // 2) Decide prefix for static‐type (if any)
                //    If at = Some(ty), we will emit “@Type.” before the method name.
                let at_prefix = if let Some(static_ty) = at {
                    format!("@{}", static_ty.emit(st, _prog))
                } else {
                    "".to_string()
                };

                // 3) Compute each argument’s block
                let mut args_blocks = Vec::new();
                for arg in formals {
                    args_blocks.push(arg.emit_with_indent(st, _prog, indent + 1));
                }

                // 4) Now assemble the entire Dispatch
                let mut sb = String::new();
                sb.push_str(&format!("{}(\n", cur_indent));

                // If we actually printed an on_block (cases 2 & 3, or case 4), insert it
                if !on_block.is_empty() {
                    sb.push_str(&on_block);
                    sb.push('\n');
                }

                // 5) Emit the method‐line itself
                let method_name = st.from_sym((*name).into());
                if at_prefix.is_empty() {
                    // Cases 1 or 2: no static dispatch
                    if on.is_some() {
                        // Case 2: we do have a target, so prefix with “<indent> .method(”
                        sb.push_str(&format!("{} .{}(\n", child_indent, method_name));
                    } else {
                        // Case 1: no target, no @Type → just “<indent>method(”
                        sb.push_str(&format!("{}{}\n", child_indent, method_name));
                        sb.push_str(&format!("{}(\n", child_indent));
                    }
                } else {
                    // Case 3 (or 4 if on was None but at is Some): we have @Type
                    // → “<indent>@Type.method(”
                    sb.push_str(&format!("{}{}.{}(\n", child_indent, at_prefix, method_name));
                }

                // 6) Emit arguments, comma‐separated
                for (i, arg_blk) in args_blocks.into_iter().enumerate() {
                    sb.push_str(&arg_blk);
                    if i + 1 < formals.len() {
                        sb.push_str(",\n");
                    } else {
                        sb.push('\n');
                    }
                }

                // 7) Close “method( … )”
                sb.push_str(&format!("{}  )\n", child_indent));
                // 8) Close the entire dispatch
                sb.push_str(&format!("{})", cur_indent));
                sb
            }


            Expr::If { condition, then, else_ } => {
                /*
                (
                    if
                        ( <cond> )
                    then
                        ( <then> )
                    else
                        ( <else> )
                    fi
                )
                */
                let cond_blk = condition.emit_with_indent(st, _prog, indent + 1);
                let then_blk = then.emit_with_indent(st, _prog, indent + 1);
                let else_blk = else_.emit_with_indent(st, _prog, indent + 1);

                format!(
                    "{}(\n\
                     {}if\n\
                     {}\n\
                     {}then\n\
                     {}\n\
                     {}else\n\
                     {}\n\
                     {}fi\n\
                     {})",
                    cur_indent,
                    child_indent,
                    cond_blk,
                    child_indent,
                    then_blk,
                    child_indent,
                    else_blk,
                    child_indent,
                    cur_indent
                )
            }

            Expr::Loop { condition, body } => {
                /*
                (
                    while
                        ( <cond> )
                    loop
                        ( <body> )
                    pool
                )
                */
                let cond_blk = condition.emit_with_indent(st, _prog, indent + 1);
                let body_blk = body.emit_with_indent(st, _prog, indent + 1);

                format!(
                    "{}(\n\
                     {}while\n\
                     {}\n\
                     {}loop\n\
                     {}\n\
                     {}pool\n\
                     {})",
                    cur_indent,
                    child_indent,
                    cond_blk,
                    child_indent,
                    body_blk,
                    child_indent,
                    cur_indent
                )
            }

            Expr::Block(exprs) => {
                /*
                (
                    {
                        ( <e1> );
                        ( <e2> );
                        …
                    }
                )
                */
                let mut sb = String::new();
                sb.push_str(&format!("{}(\n", cur_indent));
                sb.push_str(&format!("{}{{\n", child_indent));

                for subexpr in exprs.iter() {
                    let sub_blk = subexpr.emit_with_indent(st, _prog, indent + 1);
                    sb.push_str(&sub_blk);
                    sb.push_str(";\n");
                }

                sb.push_str(&format!("{}  }}\n", child_indent));
                sb.push_str(&format!("{})", cur_indent));
                sb
            }

            Expr::Let { binding, type_, initializer, body } => {
                /*
                (
                    let x: T <-
                        ( <init> )
                    in
                        ( <body> )
                )
                */
                let var_name = st.from_sym((*binding).into());
                let ty_str   = type_.emit(st, _prog);
                let init_blk = initializer.emit_with_indent(st, _prog, indent + 1);
                let body_blk = body.emit_with_indent(st, _prog, indent + 1);

                format!(
                    "{}(\n\
                     {}let {}: {} <-\n\
                     {}\n\
                     {}in\n\
                     {}\n\
                     {})",
                    cur_indent,
                    child_indent,
                    var_name,
                    ty_str,
                    init_blk,
                    child_indent,
                    body_blk,
                    cur_indent
                )
            }

            Expr::Case { on, branches } => {
                /*
                (
                    case
                        ( <on_expr> )
                    of
                        <arm1>;
                        <arm2>;
                        …
                    end
                )
                */
                let on_blk = on.emit_with_indent(st, _prog, indent + 1);

                let mut sb = String::new();
                sb.push_str(&format!("{}(\n", cur_indent));
                sb.push_str(&format!("{}case\n", child_indent));
                sb.push_str(&format!("{}\n", on_blk));
                sb.push_str(&format!("{}of\n", child_indent));

                for arm in branches.iter() {
                    // Each CaseArm is a single line (e.g. "x: T => <body>"), so indent by child_indent
                    sb.push_str(&format!("{}{}", child_indent, arm.emit(st, _prog)));
                    sb.push_str(";\n");
                }

                sb.push_str(&format!("{}esac\n", child_indent));
                sb.push_str(&format!("{})", cur_indent));
                sb
            }

            Expr::New(ty) => {
                /*
                (
                    new T
                )
                */
                let ty_str = ty.emit(st, _prog);
                format!(
                    "{}(\n{}new {}\n{})",
                    cur_indent,
                    child_indent,
                    ty_str,
                    cur_indent
                )
            }

            Expr::Isvoid(x) => {
                /*
                (
                    isvoid
                    ( <x> )
                )
                */
                let x_blk = x.emit_with_indent(st, _prog, indent + 1);
                format!(
                    "{}(\n\
                     {}isvoid\n\
                     {}\n\
                     {})",
                    cur_indent,
                    child_indent,
                    x_blk,
                    cur_indent
                )
            }

            Expr::Arithmetic { op, lhs, rhs } => {
                /*
                (
                    ( <lhs> )
                    +
                    ( <rhs> )
                )
                */
                let op_str  = op.emit(st, _prog);
                let lhs_blk = lhs.emit_with_indent(st, _prog, indent + 1);
                let rhs_blk = rhs.emit_with_indent(st, _prog, indent + 1);

                format!(
                    "{}(\n\
                     {}\n\
                     {}{}\n\
                     {}\n\
                     {})",
                    cur_indent,
                    lhs_blk,
                    child_indent,
                    op_str,
                    rhs_blk,
                    cur_indent
                )
            }

            Expr::Comparison { op, lhs, rhs } => {
                /*
                (
                    ( <lhs> )
                    =
                    ( <rhs> )
                )
                */
                let op_str  = op.emit(st, _prog);
                let lhs_blk = lhs.emit_with_indent(st, _prog, indent + 1);
                let rhs_blk = rhs.emit_with_indent(st, _prog, indent + 1);

                format!(
                    "{}(\n\
                     {}\n\
                     {}{}\n\
                     {}\n\
                     {})",
                    cur_indent,
                    lhs_blk,
                    child_indent,
                    op_str,
                    rhs_blk,
                    cur_indent
                )
            }

            Expr::Complement(x) => {
                /*
                (
                    ~
                    ( <x> )
                )
                */
                let x_blk = x.emit_with_indent(st, _prog, indent + 1);
                format!(
                    "{}(\n\
                     {}~\n\
                     {}\n\
                     {})",
                    cur_indent,
                    child_indent,
                    x_blk,
                    cur_indent
                )
            }

            Expr::Not(x) => {
                /*
                (
                    not
                    ( <x> )
                )
                */
                let x_blk = x.emit_with_indent(st, _prog, indent + 1);
                format!(
                    "{}(\n\
                     {}not\n\
                     {}\n\
                     {})",
                    cur_indent,
                    child_indent,
                    x_blk,
                    cur_indent
                )
            }
        }
    }
}

impl Emit for Expr {
    fn emit(&self, st: &SymbolTable, prog: &Program) -> String {
        // Public entry point: start at indent level 0
        self.emit_with_indent(st, prog, 0)
    }
}

////////////////////////////////////////////////////////////////////////////////
// 3) Emit for Method and Attribute  (unchanged except we now accept `prog`)
////////////////////////////////////////////////////////////////////////////////

impl Emit for Method {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        // name(formals): RetType { body }
        let name     = st.from_sym(self.name.into());
        let formals  = self.formals
                           .iter()
                           .map(|f| f.emit(st, _prog))
                           .collect::<Vec<_>>()
                           .join(", ");
        let ret_ty   = self.ret_type.emit(st, _prog);
        // If you wanted the body fully parenthesized+indented, you could call
        // `self.body.emit_with_indent(st, _prog, 1)` here instead of `self.body.emit(st,prog)`
        let body_str = self.body.emit(st, _prog);
        format!(
            "{}({}): {} {{ {} }}",
            name, formals, ret_ty, body_str
        )
    }
}

impl Emit for Attribute {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        let name = st.from_sym(self.name.into());
        let ty   = self.type_.emit(st, _prog);
        if let Some(init_expr) = &self.body {
            let init_str = init_expr.emit(st, _prog);
            format!("{}: {} <- {}", name, ty, init_str)
        } else {
            format!("{}: {}", name, ty)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// 4) Emit for Class
//
//    Here we really need `&Program` so that we can look up the parent’s name
//    in the arena.  We use that to build the “inherits X” clause correctly.
//    We no longer call any broken helper; instead we fetch the parent’s
//    `ClassSymbol` directly from `prog.class_arena`.
//
////////////////////////////////////////////////////////////////////////////////

impl Emit for Class {
    fn emit(&self, st: &SymbolTable, prog: &Program) -> String {
        let class_name = st.from_sym(self.name.into());

        // Build the “inherits Y” clause, if any
        let inherits_clause = if let Some(parent_idx) = self.inherits {
            // Look up the parent Class in the arena, then get its symbol name:
            let parent = prog.get_class(parent_idx);
            if parent.name == self.name {
                // If this ever panics, you truly have “class Foo inherits Foo.”
                panic!(
                    "❌ BUG: class {:?} inherits from itself!",
                    st.from_sym(self.name.into())
                );
            }
            let parent_name = st.from_sym(parent.name.into());
            format!(" inherits {}", parent_name)
        } else {
            "".into()
        };

        // Collect attributes and methods
        let mut features: Vec<String> = Vec::new();
        for attr in &self.attributes {
            features.push(attr.emit(st, prog));
        }
        for method in &self.methods {
            features.push(method.emit(st, prog));
        }

        let body = if features.is_empty() {
            "{};".into()
        } else {
            format!(
                " {{\n    {};\n}};",
                features.join(";\n    ")
            )
        };

        format!("class {}{}{}", class_name, inherits_clause, body)
    }
}

////////////////////////////////////////////////////////////////////////////////
// 5) Emit for Program
//
//    When emitting a `Program`, we simply call `.emit(...)` on each `Class`.
//    Note that for a `Program` we also ignore the extra `prog` parameter,
//    since `&self` _is_ the program already.
//
////////////////////////////////////////////////////////////////////////////////

impl Emit for Program {
    fn emit(&self, st: &SymbolTable, _prog: &Program) -> String {
        let mut clauses = Vec::new();
        for (_idx, class_node) in self.class_arena.iter() {
            if !class_node.builtin {
                clauses.push(class_node.emit(st, self));
            }
        }
        clauses.join("\n\n")
    }
}
