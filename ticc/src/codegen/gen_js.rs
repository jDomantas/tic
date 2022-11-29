mod gen_ir;
mod ir;

use std::fmt::Write;
use ticc_core::ir as cir;

pub(crate) fn generate_js(program: cir::Program) -> String {
    let ir = gen_ir::gen_ir(&program);
    let mut generator = Generator {
        output: String::new(),
        indent: 0,
        debug: &program.names,
    };
    for def in ir.stmts {
        generator.emit_stmt(def);
    }
    generator.emit_exports(&ir.exports);
    if let Some(msg) = &ir.trap {
        generator.emit_trap(msg);
    }
    generator.output
}

struct Generator<'a> {
    output: String,
    indent: u32,
    debug: &'a cir::NameGenerator<'a>,
}

impl Generator<'_> {
    fn emit_stmt(&mut self, stmt: ir::Stmt) {
        match stmt {
            ir::Stmt::If(name, a, b, c) => {
                self.emit_indent();
                self.output.push_str("let ");
                self.emit_name(name);
                self.output.push_str(";\n");
                let end = ir::BlockEnd::If(a, b, c);
                self.emit_block_end(end, Some(name));
            }
            ir::Stmt::Match(name, a, b) => {
                self.emit_indent();
                self.output.push_str("let ");
                self.emit_name(name);
                self.output.push_str(";\n");
                let end = ir::BlockEnd::Match(a, b);
                self.emit_block_end(end, Some(name));
            }
            ir::Stmt::ValueDef(name, value) => {
                self.emit_indent();
                self.output.push_str("const ");
                self.emit_name(name);
                self.output.push_str(" = ");
                self.emit_expr(&value, Prec::Min);
                self.output.push_str(";\n");
            }
            ir::Stmt::Def(def) => {
                self.emit_indent();
                self.output.push_str("function ");
                self.emit_name(def.name);
                self.output.push_str("(");
                for (i, &param) in def.params.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_name(param);
                }
                self.output.push_str(") {\n");
                self.indent += 1;
                self.emit_stmts(def.body.stmts);
                self.emit_block_end(*def.body.value, None);
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
        }
    }

    fn emit_stmts(&mut self, stmts: Vec<ir::Stmt>) {
        for stmt in stmts {
            self.emit_stmt(stmt);
        }
    }

    fn emit_block_end(&mut self, end: ir::BlockEnd, into: Option<ir::Name>) {
        self.emit_indent();
        match end {
            ir::BlockEnd::If(a, b, c) => {
                self.output.push_str("if (");
                self.emit_expr(&a, Prec::Min);
                self.output.push_str(") {\n");
                self.indent += 1;
                self.emit_stmts(b.stmts);
                self.emit_block_end(*b.value, into);
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("} else {\n");
                self.indent += 1;
                self.emit_stmts(c.stmts);
                self.emit_block_end(*c.value, into);
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ir::BlockEnd::Match(a, b) => {
                self.output.push_str("switch (");
                self.emit_name(a);
                self.output.push_str("[0]) {\n");
                self.indent += 1;
                for b in b {
                    self.emit_indent();
                    self.output.push_str("case '");
                    let debug = self.debug.debug_info(b.ctor);
                    self.output.push_str(debug);
                    self.output.push_str("': {\n");
                    self.indent += 1;
                    for (i, bind) in b.bindings.into_iter().enumerate() {
                        self.emit_indent();
                        self.output.push_str("const ");
                        self.emit_cir_name(bind);
                        self.output.push_str(" = ");
                        self.emit_name(a);
                        self.output.push('[');
                        self.emit_num(i as u64 + 1);
                        self.output.push_str("];\n");
                    }
                    self.emit_stmts(b.body.stmts);
                    self.emit_block_end(*b.body.value, into);
                    if into.is_some() {
                        self.emit_indent();
                        self.output.push_str("break;\n");
                    }
                    self.indent -= 1;
                    self.emit_indent();
                    self.output.push_str("}\n");
                }
                self.indent -= 1;
                self.emit_indent();
                self.output.push_str("}\n");
            }
            ir::BlockEnd::Value(expr) => {
                match into {
                    Some(name) => {
                        self.emit_name(name);
                        self.output.push_str(" = ");
                    }
                    None => self.output.push_str("return "),
                }
                self.emit_expr(&expr, Prec::Min);
                self.output.push_str(";\n");
            }
            ir::BlockEnd::Trap(msg) => {
                self.emit_trap(&msg);
            }
        }
    }

    fn emit_expr(&mut self, expr: &ir::Expr, prec: Prec) {
        let expr_prec = expr_prec(expr);
        if expr_prec < prec {
            self.output.push('(');
        }
        match expr {
            ir::Expr::Bool(true) => self.output.push_str("true"),
            ir::Expr::Bool(false) => self.output.push_str("false"),
            ir::Expr::Int(x) => self.emit_num(*x),
            ir::Expr::String(x) => self.output.push_str(&format!("{x:?}")),
            ir::Expr::Name(n) => self.emit_name(*n),
            ir::Expr::Call(a, b) => {
                self.emit_expr(a, Prec::Call);
                self.output.push('(');
                for (i, arg) in b.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.emit_expr(arg, Prec::Min);
                }
                self.output.push(')');
            }
            ir::Expr::Op(a, op, b) => {
                self.emit_expr(a, expr_prec);
                self.output.push_str(match op {
                    cir::Op::Add => " + ",
                    cir::Op::Subtract => " - ",
                    cir::Op::Multiply => " * ",
                    cir::Op::Less => " < ",
                    cir::Op::LessEq => " <= ",
                    cir::Op::Greater => " > ",
                    cir::Op::GreaterEq => " >= ",
                    cir::Op::Equal => " == ",
                    cir::Op::NotEqual => " != ",
                });
                self.emit_expr(b, expr_prec.next());
            }
            ir::Expr::Construct(ctor, a) => {
                self.output.push_str("['");
                let name = self.debug.debug_info(*ctor);
                self.output.push_str(name);
                self.output.push('\'');
                for e in a {
                    self.output.push_str(", ");
                    self.emit_expr(e, Prec::Min);
                }
                self.output.push(']');
            }
            ir::Expr::Lambda(names, body) => {
                self.output.push_str("(");
                match names.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.emit_name(*first);
                        for &n in rest {
                            self.output.push_str(", ");
                            self.emit_name(n);
                        }
                    }
                }
                self.output.push_str(")");
                self.output.push_str(" => ");
                self.emit_expr(body, Prec::Lambda);
            }
        }
        if expr_prec < prec {
            self.output.push(')');
        }
    }

    fn emit_name(&mut self, n: ir::Name) {
        match n {
            ir::Name::Cir(n) => self.emit_cir_name(n),
            ir::Name::Temp(id) => {
                self.output.push_str("$$");
                self.emit_num(id.into());
            }
        }
    }

    fn emit_cir_name(&mut self, n: cir::Name) {
        self.output.push_str(self.debug.debug_info(n));
        self.output.push('_');
        self.emit_num(n.idx.into());
        if n.copy != 0 {
            self.output.push('_');
            self.emit_num(n.copy.into());
        }
    }

    fn emit_num(&mut self, x: u64) {
        if x >= 10 {
            self.emit_num(x / 10);
        }
        self.output.push(char::from(b'0' + (x % 10) as u8));
    }

    fn emit_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    fn emit_exports(&mut self, exports: &[ir::Export]) {
        self.output.push_str("const $export_list = [");
        for (i, export) in exports.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            write!(&mut self.output, "'{}'", export.public_name).unwrap();
        }
        self.output.push_str("];\n");
        self.output.push_str("const $exports = {\n");
        for export in exports {
            self.output.push_str("    ");
            write!(&mut self.output, "'{}': ", export.public_name).unwrap();
            self.emit_name(export.name);
            self.output.push_str(",\n");
        }
        self.output.push_str("};\n");
    }

    fn emit_trap(&mut self, trap: &str) {
        self.output.push_str("throw new Error('");
        for c in trap.chars() {
            if c == '\'' {
                self.output.push('\\');
            }
            self.output.push(c);
        }
        self.output.push_str("');\n");
    }
}

fn expr_prec(expr: &ir::Expr) -> Prec {
    match expr {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) |
        ir::Expr::Construct(_, _) => Prec::Atom,
        ir::Expr::Call(_, _) => Prec::Call,
        ir::Expr::Op(_, ir::Op::Add, _) |
        ir::Expr::Op(_, ir::Op::Subtract, _) => Prec::Sum,
        ir::Expr::Op(_, ir::Op::Multiply, _) => Prec::Product,
        ir::Expr::Op(_, ir::Op::Less, _) |
        ir::Expr::Op(_, ir::Op::LessEq, _) |
        ir::Expr::Op(_, ir::Op::Greater, _) |
        ir::Expr::Op(_, ir::Op::GreaterEq, _) => Prec::Compare,
        ir::Expr::Op(_, ir::Op::Equal, _) |
        ir::Expr::Op(_, ir::Op::NotEqual, _) => Prec::Equality,
        ir::Expr::Lambda(_, _) => Prec::Lambda,
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum Prec {
    Min,
    Lambda,
    Equality,
    Compare,
    Sum,
    Product,
    Call,
    Atom,
}

impl Prec {
    fn next(self) -> Prec {
        match self {
            Prec::Min => Prec::Lambda,
            Prec::Lambda => Prec::Equality,
            Prec::Equality => Prec::Compare,
            Prec::Compare => Prec::Sum,
            Prec::Sum => Prec::Product,
            Prec::Product => Prec::Call,
            Prec::Call |
            Prec::Atom => Prec::Atom,
        }
    }
}
