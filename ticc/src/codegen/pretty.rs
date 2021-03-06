use std::collections::HashMap;
use formatter::{FormatBuilder, Node, Sticky};
use crate::codegen::cir;

pub(crate) fn pretty_print_cir(program: &cir::Program<'_>) -> String {
    let mut result = String::new();
    let mut exports = HashMap::new();
    for export in &program.exports {
        exports.insert(export.name, export.public_name.as_str());
    }
    for name in &program.order {
        let mut format = Format {
            builder: FormatBuilder::new(),
            exports: &exports,
            debug: &program.debug_info,
        };
        format.write_def(name, &program.values[name]);
        if result.len() > 0 {
            result.push('\n');
        }
        result.push_str(&format.builder.finish());
    }

    result
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Prec {
    Min,
    Compare,
    Apply,
    Add,
    Multiply,
    Atom,
}

impl Prec {
    fn next(self) -> Prec {
        match self {
            Prec::Min => Prec::Compare,
            Prec::Compare => Prec::Apply,
            Prec::Apply => Prec::Add,
            Prec::Add => Prec::Multiply,
            Prec::Multiply |
            Prec::Atom => Prec::Atom,
        }
    }
}

struct Format<'a> {
    builder: FormatBuilder,
    exports: &'a HashMap<cir::Name, &'a str>,
    debug: &'a HashMap<cir::Name, &'a str>,
}

impl Format<'_> {
    fn write_def(&mut self, name: &cir::Name, value: &cir::Expr) {
        self.builder.start_node(Node { large: true, indents: true });
        if let Some(export_name) = self.exports.get(name) {
            self.builder.add_sticky_token("export(", Sticky::Next);
            self.builder.add_token(export_name);
            self.builder.add_sticky_token(")", Sticky::Prev);
        }
        self.builder.add_token("let");
        self.write_name(name);
        self.builder.add_token("=");
        self.write_expr(value, Prec::Min);
        self.builder.add_sticky_token(";", Sticky::Prev);
        self.builder.add_newline();
        self.builder.finish_node();
    }

    fn write_name(&mut self, name: &cir::Name) {
        let debug = &self.debug[name];
        self.builder.add_token(debug);
        self.builder.add_sticky_token("_", Sticky::Both);
        self.write_number(u64::from(name.id));
    }

    fn write_ctor(&mut self, ctor: &cir::Ctor) {
        self.builder.add_sticky_token("@", Sticky::Next);
        self.write_name(&ctor.name);
    }

    fn write_number(&mut self, x: u64) {
        self.builder.add_token("");
        fn glue_number(builder: &mut FormatBuilder, x: u64) {
            let c = char::from(b'0' + (x % 10) as u8);
            if x >= 10 {
                glue_number(builder, x / 10);
            }
            builder.add_sticky_token(c.encode_utf8(&mut [0; 4]), Sticky::Prev);
        }
        glue_number(&mut self.builder, x);
    }

    fn write_expr(&mut self, expr: &cir::Expr, prec: Prec) {
        let node = expr_node(expr);
        self.builder.start_node(node);
        let expr_prec = expr_prec(expr);
        if matches!(expr, cir::Expr::Let(..) | cir::Expr::LetRec(..)) {
            self.builder.add_newline();
        }
        if expr_prec < prec {
            self.builder.add_sticky_token("(", Sticky::Next);
        }
        match expr {
            cir::Expr::Bool(true) => self.builder.add_token("true"),
            cir::Expr::Bool(false) => self.builder.add_token("false"),
            cir::Expr::Int(x) => self.write_number(*x),
            cir::Expr::Name(n) => self.write_name(n),
            cir::Expr::Call(a, b) => {
                self.write_expr(a, Prec::Apply);
                self.write_expr(b, Prec::Atom);
            }
            cir::Expr::If(_, _, _) => {}
            cir::Expr::Op(a, op, b) => {
                self.write_expr(a, expr_prec.next());
                self.builder.add_token(match op {
                    cir::Op::Add => "+",
                    cir::Op::Subtract => "-",
                    cir::Op::Multiply => "*",
                    cir::Op::Less => "<",
                    cir::Op::LessEq => "<=",
                    cir::Op::Greater => ">",
                    cir::Op::GreaterEq => ">=",
                    cir::Op::Equal => "==",
                    cir::Op::NotEqual => "!=",
                });
                self.write_expr(b, expr_prec.next());
            }
            cir::Expr::Lambda(x, e) => {
                self.builder.add_sticky_token("\\", Sticky::Next);
                self.write_name(x);
                self.builder.add_token("->");
                if !matches!(**e, cir::Expr::Lambda(_, _)) {
                    self.builder.add_large_newline();
                }
                self.write_expr(e, Prec::Min);
            }
            cir::Expr::Match(d, bs) => {
                self.builder.add_token("match");
                self.write_expr(d, Prec::Apply);
                self.builder.add_token("with");
                for b in bs {
                    self.builder.start_node(Node { large: false, indents: true });
                    self.builder.add_newline();
                    self.builder.add_token("|");
                    self.write_ctor(&b.ctor);
                    for bind in &b.bindings {
                        self.write_name(bind);
                    }
                    self.builder.add_token("->");
                    self.write_expr(&b.value, Prec::Min);
                    self.builder.finish_node();
                }
                self.builder.add_newline();
                self.builder.add_token("end");
            }
            cir::Expr::Construct(ctor, xs) => {
                self.write_ctor(ctor);
                for x in xs {
                    self.write_expr(x, Prec::Atom);
                }
            }
            cir::Expr::Let(x, v, e) => {
                self.builder.add_token("let");
                self.write_name(x);
                self.builder.start_node(Node { large: false, indents: true });
                self.builder.add_token("=");
                self.write_expr(v, Prec::Min);
                self.builder.add_sticky_token(";", Sticky::Prev);
                self.builder.finish_node();
                self.builder.add_newline();
                self.write_expr(e, Prec::Min);
            }
            cir::Expr::LetRec(x, v, e) => {
                self.builder.add_token("let");
                self.builder.add_token("rec");
                self.write_name(x);
                self.builder.add_token("=");
                self.builder.start_node(Node { large: false, indents: true });
                self.write_expr(v, Prec::Min);
                self.builder.add_sticky_token(";", Sticky::Prev);
                self.builder.finish_node();
                self.builder.add_newline();
                self.write_expr(e, Prec::Min);
            }
            cir::Expr::Trap(_) => self.builder.add_token("$trap"),
        }
        if expr_prec < prec {
            self.builder.add_sticky_token(")", Sticky::Prev);
        }
        self.builder.finish_node();
    }
}

fn expr_prec(e: &cir::Expr) -> Prec {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Name(_) => Prec::Atom,
        cir::Expr::Call(_, _) |
        cir::Expr::Construct(_, _) => Prec::Apply,
        cir::Expr::Op(_, cir::Op::Add, _) |
        cir::Expr::Op(_, cir::Op::Subtract, _) => Prec::Add,
        cir::Expr::Op(_, cir::Op::Multiply, _) => Prec::Add,
        cir::Expr::Op(_, cir::Op::Less, _) |
        cir::Expr::Op(_, cir::Op::LessEq, _) |
        cir::Expr::Op(_, cir::Op::Greater, _) |
        cir::Expr::Op(_, cir::Op::GreaterEq, _) |
        cir::Expr::Op(_, cir::Op::Equal, _) |
        cir::Expr::Op(_, cir::Op::NotEqual, _) => Prec::Compare,
        cir::Expr::If(_, _, _) |
        cir::Expr::Lambda(_, _) |
        cir::Expr::Match(_, _) |
        cir::Expr::Let(_, _, _) |
        cir::Expr::LetRec(_, _, _) => Prec::Min,
        cir::Expr::Trap(_) => Prec::Atom,
    }
}

fn expr_node(e: &cir::Expr) -> Node {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Name(_) |
        cir::Expr::Call(_, _) |
        cir::Expr::Construct(_, _) |
        cir::Expr::Op(_, _, _) |
        cir::Expr::Lambda(_, _) |
        cir::Expr::Trap(_) => Node { large: false, indents: true },
        cir::Expr::If(_, _, _) |
        cir::Expr::Match(_, _) => Node { large: true, indents: false },
        cir::Expr::Let(_, _, _) |
        cir::Expr::LetRec(_, _, _) => Node { large: true, indents: false },
    }
}
