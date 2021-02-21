use std::collections::HashMap;
use crate::codegen::cir;

pub(crate) fn emit(program: &cir::Program<'_>) -> String {
    let mut codegen = Codegen::new(&program.debug_info);
    for name in &program.order {
        let value = &program.values[name];
        codegen.emit_def(name, value);
    }
    for export in &program.exports {
        codegen.emit_export(export);
    }
    codegen.code
}

struct Codegen<'a> {
    code: String,
    indent: u32,
    debug_info: &'a HashMap<cir::Name, &'a str>,
}

impl<'a> Codegen<'a> {
    fn new(debug_info: &'a HashMap<cir::Name, &'a str>) -> Codegen {
        Codegen {
            code: INTERNALS.to_owned(),
            indent: 0,
            debug_info,
        }
    }

    fn emit_def(&mut self, name: &cir::Name, value: &cir::Expr) {
        self.indent = 0;
        self.code.push_str("\nconst ");
        self.emit_name(name);
        self.code.push_str(" = ");
        self.emit_expr(value);
        self.code.push_str(";\n");
    }

    fn internal_name(&self, name: &cir::Name) -> String {
        let debug = &self.debug_info[name];
        format!("{}_{}", debug, name.id)
    }

    fn emit_name(&mut self, name: &cir::Name) {
        self.code.push_str(&self.internal_name(name));
    }

    fn emit_names(&mut self, names: &[cir::Name]) {
        match names {
            [] => {},
            [one] => self.emit_name(one),
            [first, rest @ ..] => {
                self.emit_name(first);
                for name in rest {
                    self.code.push_str(", ");
                    self.emit_name(name);
                }
            }
        }
    }

    fn emit_branch(&mut self, branch: &cir::Branch, x: impl FnOnce(&mut Codegen)) {
        if branch.bindings.len() > 0 {
            self.code.push_str("((");
            self.emit_names(&branch.bindings);
            self.code.push_str(") => ");
            self.emit_expr(&branch.value);
            self.code.push_str(").apply(null, ");
            x(self);
            self.code.push(')');
        } else {
            self.emit_expr(&branch.value);
        }
    }

    fn emit_expr(&mut self, expr: &cir::Expr) {
        match expr {
            cir::Expr::Bool(true) => self.code.push_str("true"),
            cir::Expr::Bool(false) => self.code.push_str("false"),
            cir::Expr::Int(x) => self.code.push_str(&format!("{}", x)),
            cir::Expr::Name(name) => self.emit_name(name),
            cir::Expr::Call(f, x) => {
                self.emit_expr(f);
                self.code.push('(');
                self.emit_expr(x);
                self.code.push(')');
            }
            cir::Expr::If(c, t, e) => {
                self.code.push('(');
                self.emit_expr(c);
                self.code.push('\n');
                self.indent += 1;
                self.emit_indent();
                self.code.push_str("? ");
                self.emit_expr(t);
                self.code.push('\n');
                self.emit_indent();
                self.code.push_str(": ");
                self.emit_expr(e);
                self.code.push(')');
                self.indent -= 1;
            }
            cir::Expr::Op(lhs, op, rhs) => {
                let op = match op {
                    cir::Op::Add => "+",
                    cir::Op::Subtract => "-",
                    cir::Op::Multiply => "*",
                    cir::Op::Less => "<",
                    cir::Op::LessEq => "<=",
                    cir::Op::Greater => ">",
                    cir::Op::GreaterEq => ">=",
                    cir::Op::Equal => "===",
                    cir::Op::NotEqual => "!==",
                };
                self.code.push('(');
                self.emit_expr(lhs);
                self.code.push(' ');
                self.code.push_str(op);
                self.code.push(' ');
                self.emit_expr(rhs);
                self.code.push(')');
            }
            cir::Expr::Lambda(x, b) => {
                self.code.push('(');
                self.emit_name(x);
                self.code.push_str(" =>\n");
                self.indent += 1;
                self.emit_indent();
                self.emit_expr(b);
                self.code.push(')');
                self.indent -= 1;
            }
            cir::Expr::Match(expr, branches) => {
                match &branches[..] {
                    [] => panic!("must have branches"),
                    [one] => {
                        self.emit_branch(one, |this| {
                            this.emit_expr(expr);
                            this.code.push_str(".slice(1)");
                        });
                    }
                    [init @ .., last] => {
                        self.code.push_str("bind(\n");
                        self.indent += 1;
                        self.emit_indent();
                        self.emit_expr(expr);
                        self.code.push_str(",\n");
                        self.emit_indent();
                        self.code.push_str("x =>\n");
                        self.indent += 1;
                        for branch in init {
                            self.emit_indent();
                            self.code.push_str("x[0] === '");
                            self.emit_name(&branch.ctor.name);
                            self.code.push_str("' ? ");
                            self.emit_branch(
                                branch,
                                |this| this.code.push_str("x.slice(1)"),
                            );
                            self.code.push_str(" :\n");
                        }
                        self.emit_indent();
                        self.emit_branch(
                            last,
                            |this| this.code.push_str("x.slice(1)"),
                        );
                        self.code.push(')');
                        self.indent -= 2;
                    }
                }
            }
            cir::Expr::Construct(ctor, args) => {
                self.code.push_str("[\n");
                self.indent += 1;
                self.emit_indent();
                self.code.push('\'');
                self.emit_name(&ctor.name);
                self.code.push('\'');
                for arg in args {
                    self.code.push_str(",\n");
                    self.emit_indent();
                    self.emit_expr(arg);
                }
                self.indent -= 1;
                self.code.push(']');
            }
            cir::Expr::Let(x, val, rest) => {
                self.code.push_str("bind(\n");
                self.indent += 1;
                self.emit_indent();
                self.emit_expr(val);
                self.code.push_str(",\n");
                self.emit_indent();
                self.emit_name(x);
                self.code.push_str(" => ");
                self.emit_expr(rest);
                self.code.push(')');
                self.indent -= 1;
            },
            cir::Expr::Trap(msg) => {
                self.code.push_str("fail(\"");
                self.code.push_str(msg);
                self.code.push_str("\")");
            }
        }
    }

    fn emit_indent(&mut self) {
        for _ in 0..self.indent {
            self.code.push_str("  ");
        }
    }

    fn emit_export(&mut self, export: &cir::Export) {
        let internal_name = self.internal_name(&export.name);
        self.code.push_str("\nexport const ");
        self.code.push_str(&export.public_name);
        self.code.push_str(" = ");
        self.code.push_str(&internal_name);
        self.code.push_str(";\n");
    }
}

const INTERNALS: &str = "\
\"use strict\";
const bind = (x, f) => f(x);
const fail(msg) => { throw new Error(msg); };
";
