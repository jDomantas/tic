use std::collections::{HashMap, HashSet};

use ticc_core::ir::{self, ByteString};

#[derive(Debug, Clone)]
pub(crate) enum Op {
    PushInt(u64),
    PushBool(bool),
    PushString(ByteString),
    Pick(usize),
    Set(usize),
    PopN(usize),
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
    EnterFunction {
        upvalues: u32,
        stack_usage: u32,
    },
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
    pub(crate) bindings: usize,
}

pub(crate) fn compile(program: &ir::Program<'_>, exprs: &[ir::Expr]) -> Vec<Op> {
    let mut compiler = Compiler {
        functions: Vec::new(),
        next_label: Label(0),
    };
    let mut fn_compiler = FunctionCompiler {
        compiler: &mut compiler,
        stack_size: 0,
        max_stack_size: 0,
        entry_stack_size: 0,
        ops: Vec::new(),
        entry: Label(0),
        tail_call_entry: Label(0),
        local_depths: HashMap::new(),
        params: &[],
        rec: None,
    };
    fn_compiler.op(Op::EnterFunction { upvalues: 0, stack_usage: 0 }, 0);
    for def in &program.defs {
        fn_compiler.compile_expr(&def.value, false);
        fn_compiler.define_local(def.name);
    }
    for e in exprs {
        fn_compiler.compile_expr(e, false);
    }
    fn_compiler.op(Op::Exit, 0);
    match &mut fn_compiler.ops[0] {
        Op::EnterFunction { stack_usage, .. } => *stack_usage = fn_compiler.max_stack_size as u32,
        _ => unreachable!(),
    }
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
    entry_stack_size: usize,
    max_stack_size: usize,
    ops: Vec<Op>,
    entry: Label,
    tail_call_entry: Label,
    local_depths: HashMap<ir::Name, usize>,
    params: &'a [ir::LambdaParam],
    rec: Option<ir::Name>,
}

impl FunctionCompiler<'_> {
    fn op(&mut self, op: Op, stack_change: isize) {
        self.ops.push(op);
        self.change_stack_size(stack_change);
    }

    fn compile_expr(&mut self, expr: &ir::Expr, is_tail_position: bool) {
        match expr {
            ir::Expr::Bool(b) => self.op(Op::PushBool(*b), 1),
            ir::Expr::Int(i) => self.op(Op::PushInt(*i), 1),
            ir::Expr::String(s) => self.op(Op::PushString(s.clone()), 1),
            ir::Expr::Name(n) => {
                self.pick_var(*n);
            }
            ir::Expr::Call(f, args) => {
                for arg in args {
                    self.compile_expr(arg, false);
                }
                'compile_call: {
                    if let ir::Expr::Name(n) = **f {
                        if is_tail_position && self.rec == Some(n) {
                            // println!("gonna compile rec call");
                            for p in self.params.iter().rev() {
                                self.set_var(p.name);
                            }
                            let to_pop = self.stack_size - self.entry_stack_size;
                            if to_pop > 0 {
                                // keep stack size as if this is a call that
                                // returned normally - and we already popped args
                                self.op(Op::PopN(to_pop), 0);
                            }
                            // +1 for return value
                            self.op(Op::Jump(self.tail_call_entry), 1);
                            break 'compile_call;
                        }
                    }
                    self.compile_expr(f, false);
                    // call will push return address to stack - add and remove it for max stack size tracking
                    self.change_stack_size(1);
                    self.change_stack_size(-1);
                    // args+fn get replaced with result
                    self.op(Op::Call, -(args.len() as isize));
                }
            }
            ir::Expr::If(c, t, e) => {
                self.compile_expr(c, false);
                let else_label = self.fresh_label();
                let end_label = self.fresh_label();
                self.op(Op::JumpIfFalse(else_label), -1);
                self.compile_expr(t, is_tail_position);
                self.op(Op::Jump(end_label), 0);
                self.op(Op::Label(else_label), 0);
                self.change_stack_size(-1);
                self.compile_expr(e, is_tail_position);
                self.op(Op::Label(end_label), 0);
            }
            ir::Expr::Op(a, op, b) => {
                self.compile_expr(a, false);
                self.compile_expr(b, false);
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
                    self.compile_expr(arg, false);
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
                    tail_call_entry: self.fresh_label(),
                    compiler: self.compiler,
                    stack_size: 0,
                    entry_stack_size: 0,
                    max_stack_size: 0,
                    ops: Vec::new(),
                    entry,
                    local_depths: HashMap::new(),
                    params: p,
                    rec: None,
                };
                lambda_compiler.compile_function(p, None, e, &captures);
                for &c in &captures {
                    self.pick_var(c);
                }
                self.op(Op::ConstructClosure { addr: entry, fields: captures.len() as u32 }, -(captures.len() as isize) + 1);
            }
            ir::Expr::Match(discr, branches) => {
                self.compile_expr(discr, false);
                let mut table = Vec::with_capacity(branches.len());
                let end_label = self.fresh_label();
                let branch_labels = branches.iter().map(|_| self.fresh_label()).collect::<Vec<_>>();
                for (lbl, br) in std::iter::zip(&branch_labels, branches) {
                    table.push(Branch {
                        ctor: br.ctor,
                        dst: *lbl,
                        bindings: br.bindings.len(),
                    })
                }
                self.op(Op::Branch(table.into()), -1);
                for (br, lbl) in std::iter::zip(branches, branch_labels) {
                    self.op(Op::Label(lbl), 0);
                    for &b in &br.bindings {
                        self.change_stack_size(1);
                        self.define_local(b);
                    }
                    self.compile_expr(&br.value, is_tail_position);
                    for _ in &br.bindings {
                        self.op(Op::SwapPop, -1);
                    }
                    self.op(Op::Jump(end_label), 0);
                    self.change_stack_size(-1);
                }
                self.op(Op::Label(end_label), 0);
                self.change_stack_size(1);
            }
            ir::Expr::Construct(ctor, _, fields) => {
                for field in fields {
                    self.compile_expr(field, false);
                }
                self.op(Op::Construct { ctor: *ctor, fields: fields.len() as u32 }, -(fields.len() as isize) + 1);
            }
            ir::Expr::Let(n, v, e) => {
                self.compile_expr(v, false);
                self.define_local(*n);
                self.compile_expr(e, is_tail_position);
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
                    tail_call_entry: self.fresh_label(),
                    compiler: self.compiler,
                    stack_size: 0,
                    entry_stack_size: 0,
                    max_stack_size: 0,
                    ops: Vec::new(),
                    entry,
                    local_depths: HashMap::new(),
                    params: p,
                    rec: Some(*n),
                };
                lambda_compiler.compile_function(p, Some(*n), v, &captures);
                for &c in &captures {
                    self.pick_var(c);
                }
                self.op(Op::ConstructClosure { addr: entry, fields: captures.len() as u32 }, -(captures.len() as isize) + 1);
                // compile let
                self.define_local(*n);
                self.compile_expr(e, is_tail_position);
                self.op(Op::SwapPop, -1);
            }
            ir::Expr::Pi(_, e) => self.compile_expr(e, is_tail_position),
            ir::Expr::PiApply(e, _) => self.compile_expr(e, is_tail_position),
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

    fn set_var(&mut self, name: ir::Name) {
        let at = self.local_depths[&name];
        // given depth is after popping value on top of the stack
        let depth = self.stack_size - at - 1 - 1;
        self.op(Op::Set(depth), -1);
    }

    fn fresh_label(&mut self) -> Label {
        self.compiler.fresh_label()
    }

    fn define_local(&mut self, name: ir::Name) {
        let at = self.stack_size - 1;
        self.local_depths.insert(name, at);
    }

    fn compile_function(mut self, params: &[ir::LambdaParam], rec_name: Option<ir::Name>, e: &ir::Expr, captures: &[ir::Name]) {
        self.op(Op::Label(self.entry), 0);
        for p in params {
            self.change_stack_size(1);
            self.define_local(p.name);
        }
        // slot for the function value that was called
        self.change_stack_size(1);
        if let Some(rec) = rec_name {
            self.define_local(rec);
        }
        // slot for return address
        self.change_stack_size(1);
        for &c in captures {
            self.change_stack_size(1);
            self.define_local(c);
        }
        let entry_op_index = self.ops.len();
        self.entry_stack_size = self.stack_size;
        self.op(Op::EnterFunction {
            upvalues: captures.len() as u32,
            stack_usage: 0,
        }, 0);
        self.op(Op::Label(self.tail_call_entry), 0);
        self.compile_expr(e, true);
        self.op(Op::Return { captures: captures.len() as u32, params: params.len() as u32 }, -((params.len() + captures.len() + 2) as isize));
        assert_eq!(self.stack_size, 1); // return value
        match &mut self.ops[entry_op_index] {
            Op::EnterFunction { stack_usage, .. } => *stack_usage = (self.max_stack_size - self.entry_stack_size + captures.len()) as u32,
            _ => unreachable!(),
        }
        self.compiler.functions.push(self.ops);
    }

    fn change_stack_size(&mut self, delta: isize) {
        if delta >= 0 {
            self.stack_size += delta as usize;
        } else {
            self.stack_size -= (-delta) as usize;
        }
        self.max_stack_size = self.max_stack_size.max(self.stack_size);
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
