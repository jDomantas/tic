use std::collections::{HashMap, HashSet};

use ticc_core::ir::{self, ByteString};

#[derive(Debug, Clone)]
pub(crate) enum Op {
    PushInt(u64),
    PushBool(bool),
    PushString(ByteString),
    Pick(usize),
    SwapPop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
    Jump(Label),
    JumpIfFalse(Label),
    Return { params: u32, captures: u32 },
    Call,
    Construct { ctor: ir::Name, fields: u32 },
    ConstructClosure { addr: Label, fields: u32 },
    Branch(Box<[Branch]>),
    Trap(Box<str>),
    StringLen,
    StringConcat,
    StringCharAt,
    StringSubstring,
    StringFromChar,
    IntToString,
    Label(Label),
    Exit,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub(crate) struct Label(pub(crate) u32);

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub(crate) ctor: ir::Name,
    pub(crate) dst: Label,
}

pub(crate) fn compile(program: &ir::Program<'_>, exprs: &[ir::Expr]) -> Vec<Op> {
    let mut compiler = Compiler {
        functions: Vec::new(),
        next_label: Label(0),
    };
    let mut fn_compiler = FunctionCompiler {
        compiler: &mut compiler,
        stack_size: 0,
        ops: Vec::new(),
        entry: Label(0),
        local_depths: HashMap::new(),
    };
    for def in &program.defs {
        fn_compiler.compile_expr(&def.value);
        fn_compiler.define_local(def.name);
    }
    for e in exprs {
        fn_compiler.compile_expr(e);
    }
    fn_compiler.op(Op::Exit, 0);
    assert_eq!(fn_compiler.stack_size, program.defs.len() + exprs.len());
    let mut ops = Vec::new();
    ops.extend(fn_compiler.ops);
    ops.extend(compiler.functions.into_iter().flatten());
    ops
}

struct Compiler {
    functions: Vec<Vec<Op>>,
    next_label: Label,
}

impl Compiler {
    fn fresh_label(&mut self) -> Label {
        let l = self.next_label;
        self.next_label.0 += 1;
        l
    }
}

struct FunctionCompiler<'a> {
    compiler: &'a mut Compiler,
    stack_size: usize,
    ops: Vec<Op>,
    entry: Label,
    local_depths: HashMap<ir::Name, usize>,
}

impl FunctionCompiler<'_> {
    fn op(&mut self, op: Op, stack_change: isize) {
        self.ops.push(op);
        if stack_change >= 0 {
            self.stack_size += stack_change as usize;
        } else {
            self.stack_size -= (-stack_change) as usize;
        }
    }

    fn compile_expr(&mut self, expr: &ir::Expr) {
        match expr {
            ir::Expr::Bool(b) => self.op(Op::PushBool(*b), 1),
            ir::Expr::Int(i) => self.op(Op::PushInt(*i), 1),
            ir::Expr::String(s) => self.op(Op::PushString(s.clone()), 1),
            ir::Expr::Name(n) => {
                self.pick_var(*n);
            }
            ir::Expr::Call(f, args) => {
                for arg in args {
                    self.compile_expr(arg);
                }
                self.compile_expr(f);
                // args+fn get replaced with result
                self.op(Op::Call, -(args.len() as isize));
            }
            ir::Expr::If(c, t, e) => {
                self.compile_expr(c);
                let else_label = self.fresh_label();
                let end_label = self.fresh_label();
                self.op(Op::JumpIfFalse(else_label), -1);
                self.compile_expr(t);
                self.op(Op::Jump(end_label), 0);
                self.op(Op::Label(else_label), 0);
                self.stack_size -= 1;
                self.compile_expr(e);
                self.op(Op::Label(end_label), 0);
            }
            ir::Expr::Op(a, op, b) => {
                self.compile_expr(a);
                self.compile_expr(b);
                self.op(match op {
                    ir::Op::Add => Op::Add,
                    ir::Op::Subtract => Op::Sub,
                    ir::Op::Multiply => Op::Mul,
                    ir::Op::Divide => Op::Div,
                    ir::Op::Modulo => Op::Mod,
                    ir::Op::Less => Op::Less,
                    ir::Op::LessEq => Op::LessEq,
                    ir::Op::Greater => Op::Greater,
                    ir::Op::GreaterEq => Op::GreaterEq,
                    ir::Op::Equal => Op::Eq,
                    ir::Op::NotEqual => Op::NotEq,
                }, -1);
            }
            ir::Expr::Intrinsic(i, args) => {
                for arg in args {
                    self.compile_expr(arg);
                }
                match i {
                    ir::Intrinsic::StringLen => self.op(Op::StringLen, 0),
                    ir::Intrinsic::StringConcat => self.op(Op::StringConcat, -1),
                    ir::Intrinsic::StringCharAt => self.op(Op::StringCharAt, -1),
                    ir::Intrinsic::StringSubstring => self.op(Op::StringSubstring, -2),
                    ir::Intrinsic::StringFromChar => self.op(Op::StringFromChar, 0),
                    ir::Intrinsic::IntToString => self.op(Op::IntToString, 0),
                }
            }
            ir::Expr::Lambda(p, e) => {
                let captures = free_variables(expr);
                let entry = self.fresh_label();
                let lambda_compiler = FunctionCompiler {
                    compiler: self.compiler,
                    stack_size: 0,
                    ops: Vec::new(),
                    entry,
                    local_depths: HashMap::new(),
                };
                lambda_compiler.compile_function(p, None, e, &captures);
                for &c in &captures {
                    self.pick_var(c);
                }
                self.op(Op::ConstructClosure { addr: entry, fields: captures.len() as u32 }, -(captures.len() as isize) + 1);
            }
            ir::Expr::Match(discr, branches) => {
                self.compile_expr(discr);
                let mut table = Vec::with_capacity(branches.len());
                let end_label = self.fresh_label();
                let branch_labels = branches.iter().map(|_| self.fresh_label()).collect::<Vec<_>>();
                for (lbl, br) in std::iter::zip(&branch_labels, branches) {
                    table.push(Branch {
                        ctor: br.ctor,
                        dst: *lbl,
                    })
                }
                self.op(Op::Branch(table.into()), -1);
                for (br, lbl) in std::iter::zip(branches, branch_labels) {
                    self.op(Op::Label(lbl), 0);
                    for &b in &br.bindings {
                        self.stack_size += 1;
                        self.define_local(b);
                    }
                    self.compile_expr(&br.value);
                    for _ in &br.bindings {
                        self.op(Op::SwapPop, -1);
                    }
                    self.op(Op::Jump(end_label), 0);
                    self.stack_size -= 1;
                }
                self.op(Op::Label(end_label), 0);
                self.stack_size += 1;
            }
            ir::Expr::Construct(ctor, _, fields) => {
                for field in fields {
                    self.compile_expr(field);
                }
                self.op(Op::Construct { ctor: *ctor, fields: fields.len() as u32 }, -(fields.len() as isize) + 1);
            }
            ir::Expr::Let(n, v, e) => {
                self.compile_expr(v);
                self.define_local(*n);
                self.compile_expr(e);
                self.op(Op::SwapPop, -1);
            }
            ir::Expr::LetRec(n, _, v, e) => {
                let v = unwrap_pis(v);
                let ir::Expr::Lambda(p, v) = v else {
                    panic!("let rec value is not lambda");
                };
                // compile lambda value
                let mut captures = free_variables(expr);
                captures.retain(|c| c != n);
                let entry = self.fresh_label();
                let lambda_compiler = FunctionCompiler {
                    compiler: self.compiler,
                    stack_size: 0,
                    ops: Vec::new(),
                    entry,
                    local_depths: HashMap::new(),
                };
                lambda_compiler.compile_function(p, Some(*n), v, &captures);
                for &c in &captures {
                    self.pick_var(c);
                }
                self.op(Op::ConstructClosure { addr: entry, fields: captures.len() as u32 }, -(captures.len() as isize) + 1);
                // compile let
                self.define_local(*n);
                self.compile_expr(e);
                self.op(Op::SwapPop, -1);
            }
            ir::Expr::Pi(_, e) => self.compile_expr(e),
            ir::Expr::PiApply(e, _) => self.compile_expr(e),
            ir::Expr::Trap(msg, _) => {
                self.op(Op::Trap(msg.as_str().into()), 1);
            }
        }
    }

    fn pick_var(&mut self, name: ir::Name) {
        let at = self.local_depths[&name];
        let depth = self.stack_size - at - 1;
        self.op(Op::Pick(depth), 1);
    }

    fn fresh_label(&mut self) -> Label {
        self.compiler.fresh_label()
    }

    fn define_local(&mut self, name: ir::Name) {
        let at = self.stack_size - 1;
        self.local_depths.insert(name, at);
    }

    fn compile_function(mut self, p: &[ir::LambdaParam], rec_name: Option<ir::Name>, e: &ir::Expr, captures: &[ir::Name]) {
        self.op(Op::Label(self.entry), 0);
        self.compile_function_inner(p, rec_name, e, captures);
        self.op(Op::Return { captures: captures.len() as u32, params: p.len() as u32 }, -((p.len() + captures.len() + 2) as isize));
        assert_eq!(self.stack_size, 1); // return value
        self.compiler.functions.push(self.ops);
    }

    fn compile_function_inner(&mut self, p: &[ir::LambdaParam], rec_name: Option<ir::Name>, e: &ir::Expr, captures: &[ir::Name]) {
        if let Some((p, rest)) = p.split_first() {
            self.stack_size += 1;
            self.define_local(p.name);
            if rest.len() == 0 {
                // slot for the function value that was called
                self.stack_size += 1;
                if let Some(rec) = rec_name {
                    self.define_local(rec)
                }
                // slot for return address
                self.stack_size += 1;
            }
            self.compile_function_inner(rest, rec_name, e, captures);
        } else if let Some((c, rest)) = captures.split_first() {
            self.stack_size += 1;
            self.define_local(*c);
            self.compile_function_inner(p, rec_name, e, rest);
        } else {
            self.compile_expr(e);
        }
    }
}

fn free_variables(expr: &ir::Expr) -> Vec<ir::Name> {
    fn go(e: &ir::Expr, used: &mut Vec<ir::Name>, defined: &mut HashSet<ir::Name>) {
        match e {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) => {}
            ir::Expr::Name(n) => used.push(*n),
            ir::Expr::Call(a, bs) => {
                go(a, used, defined);
                for b in bs {
                    go(b, used, defined);
                }
            }
            ir::Expr::If(c, t, e) => {
                go(c, used, defined);
                go(t, used, defined);
                go(e, used, defined);
            }
            ir::Expr::Op(a, _, b) => {
                go(a, used, defined);
                go(b, used, defined);
            }
            ir::Expr::Intrinsic(_, args) => {
                for arg in args {
                    go(arg, used, defined);
                }
            }
            ir::Expr::Lambda(ps, e) => {
                for p in ps {
                    defined.insert(p.name);
                }
                go(e, used, defined);
            }
            ir::Expr::Match(d, branches) => {
                go(d, used, defined);
                for br in branches {
                    for &v in &br.bindings {
                        defined.insert(v);
                    }
                    go(&br.value, used, defined);
                }
            }
            ir::Expr::Construct(_, _, xs) => {
                for x in xs {
                    go(x, used, defined);
                }
            }
            ir::Expr::Let(x, v, e) |
            ir::Expr::LetRec(x, _, v, e) => {
                defined.insert(*x);
                go(v, used, defined);
                go(e, used, defined);
            }
            ir::Expr::Pi(_, e) |
            ir::Expr::PiApply(e, _) => go(e, used, defined),
            ir::Expr::Trap(_, _) => {}
        }
    }
    let mut used = Vec::new();
    let mut defined = HashSet::new();
    go(expr, &mut used, &mut defined);
    used.retain(|item| defined.insert(*item));
    used
}

fn unwrap_pis(mut expr: &ir::Expr) -> &ir::Expr {
    while let ir::Expr::Pi(_, e) = expr {
        expr = e;
    }
    expr
}
