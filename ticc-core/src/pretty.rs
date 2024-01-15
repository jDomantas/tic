use formatter::{FormatBuilder, Node, Sticky};
use crate::ir;

pub fn pretty_print(program: &ir::Program<'_>) -> String {
    let mut result = String::new();
    for def in &program.values {
        let mut format = Format {
            builder: FormatBuilder::new(),
            names: &program.names,
        };
        format.write_def(def);
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
    names: &'a ir::NameGenerator<'a>,
}

impl Format<'_> {
    fn write_def(&mut self, def: &ir::ValueDef) {
        self.builder.start_node(Node { large: true, indents: true });
        if let Some(export_name) = &def.export_name {
            self.builder.add_sticky_token("export(", Sticky::Next);
            self.builder.add_token(&format!("{:?}", export_name));
            self.builder.add_sticky_token(")", Sticky::Prev);
        }
        self.builder.add_token("let");
        self.write_name(&def.name);
        self.builder.add_token(":");
        self.write_ty(&def.ty, Prec::Min);
        self.builder.add_token("=");
        self.write_expr(&def.value, Prec::Min);
        self.builder.add_sticky_token(";", Sticky::Prev);
        self.builder.add_newline();
        self.builder.finish_node();
    }

    fn write_name(&mut self, name: &ir::Name) {
        self.builder.add_token(&self.names.show_debug(*name).to_string());
    }

    fn write_ctor(&mut self, ctor: &ir::Name) {
        self.builder.add_sticky_token("@", Sticky::Next);
        self.write_name(ctor);
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

    fn write_string(&mut self, x: &ir::ByteString) {
        self.builder.add_token(&format!("{x:?}"));
    }

    fn write_expr(&mut self, expr: &ir::Expr, prec: Prec) {
        let node = expr_node(expr);
        self.builder.start_node(node);
        let expr_prec = expr_prec(expr);
        if matches!(expr, ir::Expr::Let(..) | ir::Expr::LetRec(..)) {
            self.builder.add_newline();
        }
        if expr_prec < prec {
            self.builder.add_sticky_token("(", Sticky::Next);
        }
        match expr {
            ir::Expr::Bool(true) => self.builder.add_token("true"),
            ir::Expr::Bool(false) => self.builder.add_token("false"),
            ir::Expr::Int(x) => self.write_number(*x),
            ir::Expr::String(x) => self.write_string(x),
            ir::Expr::Name(n) => self.write_name(n),
            ir::Expr::Call(f, args) => {
                self.write_expr(f, Prec::Apply);
                self.builder.add_sticky_token("(", Sticky::Both);
                match args.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.write_expr(first, Prec::Min);
                        for e in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_expr(e, Prec::Min);
                        }
                    }
                }
                self.builder.add_sticky_token(")", Sticky::Prev);
            }
            ir::Expr::Intrinsic(i, args) => {
                self.builder.add_token(match i {
                    ir::Intrinsic::StringLen => "%stringLen",
                    ir::Intrinsic::StringConcat => "%stringConcat",
                    ir::Intrinsic::StringCharAt => "%stringCharAt",
                    ir::Intrinsic::StringSubstring => "%stringSubstring",
                    ir::Intrinsic::StringFromChar => "%stringFromChar",
                    ir::Intrinsic::IntToString => "%intToString",
                });
                self.builder.add_sticky_token("(", Sticky::Both);
                match args.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.write_expr(first, Prec::Min);
                        for e in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_expr(e, Prec::Min);
                        }
                    }
                }
                self.builder.add_sticky_token(")", Sticky::Prev);
            }
            ir::Expr::If(a, b, c) => {
                self.builder.add_token("if");
                self.write_expr(a, Prec::Apply);
                self.builder.add_token("then");
                self.builder.add_large_newline();
                self.write_expr(b, Prec::Min);
                self.builder.add_large_newline();
                self.builder.add_token("else");
                self.builder.add_large_newline();
                self.write_expr(c, Prec::Min);
            }
            ir::Expr::Op(a, op, b) => {
                self.write_expr(a, expr_prec.next());
                self.builder.add_token(match op {
                    ir::Op::Add => "+",
                    ir::Op::Subtract => "-",
                    ir::Op::Multiply => "*",
                    ir::Op::Divide => "/",
                    ir::Op::Modulo => "*",
                    ir::Op::Less => "<",
                    ir::Op::LessEq => "<=",
                    ir::Op::Greater => ">",
                    ir::Op::GreaterEq => ">=",
                    ir::Op::Equal => "==",
                    ir::Op::NotEqual => "!=",
                });
                self.write_expr(b, expr_prec.next());
            }
            ir::Expr::Lambda(params, e) => {
                self.builder.add_sticky_token("\\(", Sticky::Next);
                match params.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.write_lambda_param(first);
                        for param in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_lambda_param(param);
                        }
                    }
                }
                self.builder.add_sticky_token(")", Sticky::Prev);
                self.builder.add_token("->");
                if !matches!(**e, ir::Expr::Lambda(_, _)) {
                    self.builder.add_large_newline();
                }
                self.write_expr(e, Prec::Min);
            }
            ir::Expr::Match(d, bs) => {
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
            ir::Expr::Construct(ctor, tys, xs) => {
                self.write_ctor(ctor);
                match tys.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.builder.add_sticky_token("[", Sticky::Both);
                        self.write_ty(first, Prec::Min);
                        for t in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_ty(t, Prec::Min);
                        }
                        self.builder.add_sticky_token("]", Sticky::Prev);
                    }
                }
                self.builder.add_sticky_token("(", Sticky::Both);
                match xs.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.write_expr(first, Prec::Min);
                        for e in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_expr(e, Prec::Min);
                        }
                    }
                }
                self.builder.add_sticky_token(")", Sticky::Prev);
            }
            ir::Expr::Let(x, v, e) => {
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
            ir::Expr::LetRec(x, ty, v, e) => {
                self.builder.add_token("let");
                self.builder.add_token("rec");
                self.write_name(x);
                self.builder.add_token(":");
                self.write_ty(ty, Prec::Min);
                self.builder.start_node(Node { large: false, indents: true });
                self.builder.add_token("=");
                self.write_expr(v, Prec::Min);
                self.builder.add_sticky_token(";", Sticky::Prev);
                self.builder.finish_node();
                self.builder.add_newline();
                self.write_expr(e, Prec::Min);
            }
            ir::Expr::Trap(msg, ty) => {
                self.builder.add_sticky_token("#trap(", Sticky::Next);
                self.builder.add_token(&format!("{:?}", msg));
                self.builder.add_sticky_token(",", Sticky::Prev);
                self.write_ty(ty, Prec::Min);
                self.builder.add_sticky_token(")", Sticky::Prev);
            }
            ir::Expr::Pi(var, expr) => {
                self.builder.add_token("Pi");
                self.write_ty(&ir::Ty::Var(*var), Prec::Min);
                self.builder.add_token("->");
                self.write_expr(expr, Prec::Min);
            }
            ir::Expr::PiApply(expr, ty) => {
                self.write_expr(expr, Prec::Apply);
                self.builder.add_sticky_token("[", Sticky::Both);
                self.write_ty(ty, Prec::Min);
                self.builder.add_sticky_token("]", Sticky::Prev);
            }
        }
        if expr_prec < prec {
            self.builder.add_sticky_token(")", Sticky::Prev);
        }
        self.builder.finish_node();
    }

    fn write_lambda_param(&mut self, param: &ir::LambdaParam) {
        self.write_name(&param.name);
        self.builder.add_token(":");
        self.write_ty(&param.ty, Prec::Min);
    }

    fn write_ty(&mut self, ty: &ir::Ty, prec: Prec) {
        let ty_prec = ty_prec(ty);
        if ty_prec < prec {
            self.builder.add_sticky_token("(", Sticky::Next);
        }
        match ty {
            ir::Ty::Bool => {
                self.builder.add_token("bool");
            }
            ir::Ty::Int => {
                self.builder.add_token("int");
            }
            ir::Ty::String => {
                self.builder.add_token("string");
            }
            ir::Ty::Var(v) => {
                self.builder.add_sticky_token("t", Sticky::Next);
                self.write_number(v.0);
            }
            ir::Ty::Named(n, args) => {
                self.write_name(n);
                for t in args {
                    self.write_ty(t, Prec::Atom);
                }
            }
            ir::Ty::Fn(params, out) => {
                self.builder.add_sticky_token("(", Sticky::Next);
                match params.as_slice() {
                    [] => {}
                    [first, rest @ ..] => {
                        self.write_ty(first, Prec::Min);
                        for t in rest {
                            self.builder.add_sticky_token(",", Sticky::Prev);
                            self.write_ty(t, Prec::Min);
                        }
                    }
                }
                self.builder.add_sticky_token(")", Sticky::Prev);
                self.builder.add_token("->");
                self.write_ty(out, Prec::Min);
            }
            ir::Ty::Abs(var, ty) => {
                self.builder.add_token("for");
                self.write_ty(&ir::Ty::Var(*var), Prec::Min);
                self.builder.add_token(".");
                self.write_ty(ty, Prec::Min);
            }
        }
        if ty_prec < prec {
            self.builder.add_sticky_token(")", Sticky::Prev);
        }
    }
}

fn expr_prec(e: &ir::Expr) -> Prec {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) => Prec::Atom,
        ir::Expr::Call(_, _) |
        ir::Expr::Intrinsic(_, _) |
        ir::Expr::Construct(_, _, _) => Prec::Atom,
        ir::Expr::Op(_, ir::Op::Add, _) |
        ir::Expr::Op(_, ir::Op::Subtract, _) => Prec::Add,
        ir::Expr::Op(_, ir::Op::Multiply, _) |
        ir::Expr::Op(_, ir::Op::Divide, _) |
        ir::Expr::Op(_, ir::Op::Modulo, _) => Prec::Multiply,
        ir::Expr::Op(_, ir::Op::Less, _) |
        ir::Expr::Op(_, ir::Op::LessEq, _) |
        ir::Expr::Op(_, ir::Op::Greater, _) |
        ir::Expr::Op(_, ir::Op::GreaterEq, _) |
        ir::Expr::Op(_, ir::Op::Equal, _) |
        ir::Expr::Op(_, ir::Op::NotEqual, _) => Prec::Compare,
        ir::Expr::If(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Match(_, _) |
        ir::Expr::Let(_, _, _) |
        ir::Expr::LetRec(_, _, _, _) => Prec::Min,
        ir::Expr::Trap(_, _) => Prec::Atom,
        ir::Expr::Pi(_, _) => Prec::Min,
        ir::Expr::PiApply(_, _) => Prec::Atom,
    }
}

fn expr_node(e: &ir::Expr) -> Node {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) |
        ir::Expr::Call(_, _) |
        ir::Expr::Intrinsic(_, _) |
        ir::Expr::Construct(_, _, _) |
        ir::Expr::Op(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Trap(_, _) |
        ir::Expr::Pi(_, _) |
        ir::Expr::PiApply(_, _) => Node { large: false, indents: true },
        ir::Expr::Match(_, _) => Node { large: true, indents: false },
        ir::Expr::Let(_, _, _) |
        ir::Expr::LetRec(_, _, _, _) => Node { large: true, indents: false },
        ir::Expr::If(_, t, e) => {
            Node {
                large: !is_atom(t) || !is_atom(e),
                indents: true,
            }
        }
    }
}

fn is_atom(e: &ir::Expr) -> bool {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) |
        ir::Expr::Call(_, _) |
        ir::Expr::Intrinsic(_, _) |
        ir::Expr::Trap(_, _) |
        ir::Expr::PiApply(_, _) => true,
        ir::Expr::If(_, _, _) |
        ir::Expr::Op(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Match(_, _) |
        ir::Expr::Construct(_, _, _) |
        ir::Expr::Let(_, _, _) |
        ir::Expr::LetRec(_, _, _, _) |
        ir::Expr::Pi(_, _) => false,
    }
}

fn ty_prec(ty: &ir::Ty) -> Prec {
    match ty {
        ir::Ty::Bool |
        ir::Ty::Int |
        ir::Ty::String |
        ir::Ty::Var(_) => Prec::Atom,
        ir::Ty::Named(_, _) |
        ir::Ty::Fn(_, _) |
        ir::Ty::Abs(_, _) => Prec::Apply,
    }
}
