mod tagged;

use std::{
    collections::{HashMap, HashSet},
    fmt,
    rc::{Rc, Weak},
};
use ticc_core::ir::{self, ByteString};
use crate::tagged::Tagged;

#[derive(Clone)]
pub enum Value {
    Int(u64),
    Bool(bool),
    String(ByteString),
    Composite(Tagged),
    Fn(Function),
}

type Callable = dyn Fn(&mut CompactEnv, &[Value]) -> Result<Value, Trap>;

#[derive(Clone)]
pub struct Function(Rc<Callable>);

impl Function {
    pub fn call(&self, args: &[Value]) -> Result<Value, Trap> {
        (self.0)(&mut CompactEnv::default(), args)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(x) => f.debug_tuple("Int").field(x).finish(),
            Value::Bool(x) => f.debug_tuple("Bool").field(x).finish(),
            Value::String(x) => f.debug_tuple("String").field(x).finish(),
            Value::Composite(v) => {
                let mut s = f.debug_tuple("Composite");
                s.field(&v.tag());
                for x in v.fields().iter() {
                    s.field(x);
                }
                s.finish()
            }
            Value::Fn(_) => f.debug_tuple("Fn").field(&std::ops::RangeFull).finish(),
        }
    }
}

impl Value {
    fn into_int(self) -> u64 {
        if let Value::Int(x) = self {
            x
        } else {
            panic!("value is not an int");
        }
    }

    fn into_bool(self) -> bool {
        if let Value::Bool(x) = self {
            x
        } else {
            panic!("value is not a bool");
        }
    }

    fn into_string(self) -> ByteString {
        if let Value::String(x) = self {
            x
        } else {
            panic!("value is not a string");
        }
    }

    fn into_composite(self) -> Tagged {
        if let Value::Composite(x) = self {
            x
        } else {
            panic!("value is not a composite");
        }
    }

    fn into_fn(self) -> Rc<Callable> {
        if let Value::Fn(x) = self {
            x.0
        } else {
            panic!("value is not an fn");
        }
    }
}

#[derive(Default, Clone)]
pub struct Env {
    values: HashMap<ir::Name, Value>,
}

impl Env {
    pub fn new() -> Env {
        Env::default()
    }

    pub fn add(&mut self, name: ir::Name, value: Value) {
        self.values.insert(name, value);
    }
}

#[derive(Debug)]
pub struct Trap {
    pub message: String,
}

#[derive(Clone, Copy)]
struct NameLookup(usize);

#[derive(Clone, Copy)]
struct FramePos(usize);

#[derive(Default)]
struct CompactEnv {
    names: Vec<Value>,
    frame_start: usize,
}

impl CompactEnv {
    fn open_frame(&mut self) -> FramePos {
        let pos = FramePos(self.frame_start);
        self.frame_start = self.names.len();
        pos
    }

    fn pop_frame(&mut self, pos: FramePos) {
        self.frame_start = pos.0;
    }

    fn lookup(&self, lookup: NameLookup) -> Value {
        self.names[self.frame_start + lookup.0].clone()
    }

    fn add(&mut self, value: Value) {
        self.names.push(value);
    }

    fn remove(&mut self) {
        self.names.pop();
    }
}

enum ResolvedName {
    Value(Value),
    Name(NameLookup),
}

struct EnvResolver {
    names: HashMap<ir::Name, usize>,
    stack: Vec<ir::Name>,
    known_values: Rc<HashMap<ir::Name, Value>>,
}

impl EnvResolver {
    fn from_env(env: &Env) -> EnvResolver {
        let mut known_values = HashMap::new();
        for (k, v) in &env.values {
            known_values.insert(k.clone(), v.clone());
        }
        EnvResolver {
            names: Default::default(),
            stack: Default::default(),
            known_values: Rc::new(known_values),
        }
    }

    fn from_other(r: &EnvResolver) -> EnvResolver {
        EnvResolver {
            names: Default::default(),
            stack: Default::default(),
            known_values: r.known_values.clone(),
        }
    }

    fn lookup(&mut self, name: ir::Name) -> ResolvedName {
        if let Some(v) = self.known_values.get(&name).cloned() {
            ResolvedName::Value(v)
        } else {
            ResolvedName::Name(NameLookup(self.names[&name]))
        }
    }

    fn add(&mut self, name: ir::Name) {
        self.names.insert(name, self.stack.len());
        self.stack.push(name);
    }

    fn remove(&mut self, name: ir::Name) {
        assert_eq!(self.stack.pop(), Some(name));
        self.names.remove(&name);
    }
}

pub fn eval(program: &ir::Program<'_>, exprs: &[ir::Expr]) -> Result<Vec<Value>, Trap> {
    let mut env = Env::default();
    for def in &program.defs {
        let v = eval_expr(env.clone(), &def.value)?;
        env.add(def.name, v);
    }
    let mut results = Vec::new();
    for e in exprs {
        let v = eval_expr(env.clone(), e)?;
        results.push(v);
    }
    Ok(results)
}

fn eval_expr(env: Env, expr: &ir::Expr) -> Result<Value, Trap> {
    let mut compact_env = CompactEnv::default();
    let mut resolver = EnvResolver::from_env(&env);
    let compiled = compile_expr(expr, &mut resolver);
    assert_eq!(resolver.stack.len(), 0);
    let res = compiled(&mut compact_env);
    assert_eq!(compact_env.names.len(), 0);
    res
}

fn compile_expr(expr: &ir::Expr, env: &mut EnvResolver) -> Box<dyn Fn(&mut CompactEnv) -> Result<Value, Trap>> {
    match expr {
        ir::Expr::Bool(b) => {
            let b = *b;
            Box::new(move |_| Ok(Value::Bool(b)))
        }
        ir::Expr::Int(i) => {
            let i = *i;
            Box::new(move |_| Ok(Value::Int(i)))
        }
        ir::Expr::String(s) => {
            let s = s.clone();
            Box::new(move |_| Ok(Value::String(s.clone())))
        }
        ir::Expr::Name(n) => {
            match env.lookup(*n) {
                ResolvedName::Value(v) => Box::new(move |_| Ok(v.clone())),
                ResolvedName::Name(lookup) => Box::new(move |env| Ok(env.lookup(lookup))),
            }
        }
        ir::Expr::Call(f, args) => {
            let f = compile_expr(f, env);
            match args.len() {
                0 => Box::new(move |env| f(env)?.into_fn()(env, &[])),
                1 => {
                    let arg = compile_expr(&args[0], env);
                    Box::new(move |env| {
                        let f = f(env)?.into_fn();
                        let arg = arg(env)?;
                        f(env, &[arg])
                    })
                }
                2 => {
                    let arg1 = compile_expr(&args[0], env);
                    let arg2 = compile_expr(&args[1], env);
                    Box::new(move |env| {
                        let f = f(env)?.into_fn();
                        let arg1 = arg1(env)?;
                        let arg2 = arg2(env)?;
                        f(env, &[arg1, arg2])
                    })
                }
                3 => {
                    let arg1 = compile_expr(&args[0], env);
                    let arg2 = compile_expr(&args[1], env);
                    let arg3 = compile_expr(&args[2], env);
                    Box::new(move |env| {
                        let f = f(env)?.into_fn();
                        let arg1 = arg1(env)?;
                        let arg2 = arg2(env)?;
                        let arg3 = arg3(env)?;
                        f(env, &[arg1, arg2, arg3])
                    })
                }
                _ => {
                    let args = args.iter().map(|e| compile_expr(e, env)).collect::<Vec<_>>();
                    Box::new(move |env| {
                        let f = f(env)?.into_fn();
                        let mut arg_values = Vec::with_capacity(args.len());
                        for a in &args {
                            arg_values.push(a(env)?);
                        }
                        f(env, &arg_values)
                    })
                }
            }
        }
        ir::Expr::If(c, t, e) => {
            let c = compile_expr(c, env);
            let t = compile_expr(t, env);
            let e = compile_expr(e, env);
            Box::new(move |env| {
                if c(env)?.into_bool() {
                    t(env)
                } else {
                    e(env)
                }
            })
        }
        ir::Expr::Op(a, op, b) => {
            let a = compile_expr(a, env);
            let b = compile_expr(b, env);
            match op {
                ir::Op::Add => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Int(a.wrapping_add(b)))
                }),
                ir::Op::Subtract => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Int(a.wrapping_sub(b)))
                }),
                ir::Op::Multiply => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Int(a.wrapping_mul(b)))
                }),
                ir::Op::Divide => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Int(a.checked_div(b).unwrap_or(0)))
                }),
                ir::Op::Modulo => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Int(a.checked_rem(b).unwrap_or(a)))
                }),
                ir::Op::Less => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a < b))
                }),
                ir::Op::LessEq => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a <= b))
                }),
                ir::Op::Greater => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a > b))
                }),
                ir::Op::GreaterEq => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a >= b))
                }),
                ir::Op::Equal => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a == b))
                }),
                ir::Op::NotEqual => Box::new(move |env| {
                    let a = a(env)?.into_int();
                    let b = b(env)?.into_int();
                    Ok(Value::Bool(a != b))
                }),
            }
        }
        ir::Expr::Intrinsic(i, args) => {
            match i {
                ir::Intrinsic::StringLen => {
                    let arg = compile_expr(&args[0], env);
                    Box::new(move |env| {
                        let arg = arg(env)?.into_string();
                        Ok(Value::Int(arg.len() as u64))
                    })
                }
                ir::Intrinsic::StringConcat => {
                    let s1 = compile_expr(&args[0], env);
                    let s2 = compile_expr(&args[1], env);
                    Box::new(move |env| {
                        let s1 = s1(env)?.into_string();
                        let s2 = s2(env)?.into_string();
                        let mut res = Vec::with_capacity(s1.len() + s2.len());
                        res.extend(&s1[..]);
                        res.extend(&s2[..]);
                        Ok(Value::String(res.into()))
                    })
                }
                ir::Intrinsic::StringCharAt => {
                    let idx = compile_expr(&args[0], env);
                    let s = compile_expr(&args[1], env);
                    Box::new(move |env| {
                        let idx = idx(env)?.into_int() as usize;
                        let s = s(env)?.into_string();
                        let ch = s.get(idx).copied().unwrap_or(0);
                        Ok(Value::Int(u64::from(ch)))
                    })
                }
                ir::Intrinsic::StringSubstring => {
                    let start = compile_expr(&args[0], env);
                    let len = compile_expr(&args[1], env);
                    let s = compile_expr(&args[2], env);
                    Box::new(move |env| {
                        let start = start(env)?.into_int() as usize;
                        let len = len(env)?.into_int() as usize;
                        let s = s(env)?.into_string();
                        let s = s.get(start..).unwrap_or(b"");
                        let s = s.get(..len).unwrap_or(s);
                        Ok(Value::String(s.into()))
                    })
                }
                ir::Intrinsic::StringFromChar => {
                    let ch = compile_expr(&args[0], env);
                    Box::new(move |env| {
                        let ch = ch(env)?.into_int() as u8;
                        Ok(Value::String(ByteString::from([ch].as_slice())))
                    })
                }
                ir::Intrinsic::IntToString => {
                    let arg = compile_expr(&args[0], env);
                    Box::new(move |env| {
                        let arg = arg(env)?.into_int();
                        Ok(Value::String(arg.to_string().as_bytes().into()))
                    })
                }
            }
        }
        ir::Expr::Lambda(params, body) => {
            let mut captured_names = HashSet::new();
            free_names(expr, &mut captured_names);
            let mut inner_env = EnvResolver::from_other(&env);
            let mut captures = Vec::with_capacity(captured_names.len());
            for &c in &captured_names {
                match env.lookup(c) {
                    ResolvedName::Value(_) => continue,
                    ResolvedName::Name(l) => {
                        captures.push(l);
                        inner_env.add(c);
                    }
                }
            }
            for param in params {
                inner_env.add(param.name);
            }
            let body: Rc<dyn Fn(&mut CompactEnv) -> Result<Value, Trap>> = compile_expr(body, &mut inner_env).into();
            for param in params.iter().rev() {
                inner_env.remove(param.name);
            }
            assert_eq!(inner_env.names.len(), captures.len());
            Box::new(move |env| {
                let captured_values: Rc<[Value]> = captures
                    .iter()
                    .map(|n| env.lookup(*n))
                    .collect::<Vec<_>>()
                    .into();
                let body = body.clone();
                Ok(Value::Fn(Function(Rc::new(move |env, args| {
                    let prev_frame = env.open_frame();
                    for v in captured_values.iter() {
                        env.add(v.clone());
                    }
                    for arg in args {
                        env.add(arg.clone());
                    }
                    let res = body(env);
                    for _ in args {
                        env.remove();
                    }
                    for _ in captured_values.iter() {
                        env.remove();
                    }
                    env.pop_frame(prev_frame);
                    res
                }))))
            })
        }
        ir::Expr::Match(x, branches) => {
            let x = compile_expr(x, env);
            let branches = branches
                .iter()
                .cloned()
                .map(|b| {
                    for &binding in &b.bindings {
                        env.add(binding)
                    }
                    let compiled = compile_expr(&b.value, env);
                    for &binding in b.bindings.iter().rev() {
                        env.remove(binding)
                    }
                    (b, compiled)
                })
                .collect::<Vec<_>>();
            Box::new(move |env| {
                let tagged = x(env)?.into_composite();
                let tag = tagged.tag();
                for (br, body) in &branches {
                    if br.ctor == tag {
                        for f in tagged.fields().iter() {
                            env.add(f.clone());
                        }
                        let res = body(env);
                        for _ in tagged.fields().iter() {
                            env.remove();
                        }
                        return res;
                    }
                }
                panic!("no branch matched")
            })
        }
        ir::Expr::Construct(n, _, fields) => {
            let n = *n;
            let fields = fields.iter().map(|f| compile_expr(f, env)).collect::<Vec<_>>();
            match fields.len() {
                0 => {
                    Box::new(move |_| Ok(Value::Composite(Tagged::from_array(n, []))))
                }
                1 => {
                    Box::new(move |env| {
                        let fields = [
                            fields[0](env)?,
                        ];
                        Ok(Value::Composite(Tagged::from_array(n, fields)))
                    })
                }
                2 => {
                    Box::new(move |env| {
                        let fields = [
                            fields[0](env)?,
                            fields[1](env)?,
                        ];
                        Ok(Value::Composite(Tagged::from_array(n, fields)))
                    })
                }
                3 => {
                    Box::new(move |env| {
                        let fields = [
                            fields[0](env)?,
                            fields[1](env)?,
                            fields[2](env)?,
                        ];
                        Ok(Value::Composite(Tagged::from_array(n, fields)))
                    })
                }
                4 => {
                    Box::new(move |env| {
                        let fields = [
                            fields[0](env)?,
                            fields[1](env)?,
                            fields[2](env)?,
                            fields[3](env)?,
                        ];
                        Ok(Value::Composite(Tagged::from_array(n, fields)))
                    })
                }
                _ => {
                    Box::new(move |env| {
                        let mut field_values = Vec::with_capacity(fields.len());
                        for f in &fields {
                            field_values.push(f(env)?);
                        }
                        Ok(Value::Composite(Tagged::from_vec(n, field_values)))
                    })
                }
            }
        }
        ir::Expr::Let(n, v, r) => {
            let n = *n;
            let v = compile_expr(v, env);
            env.add(n);
            let r = compile_expr(r, env);
            env.remove(n);
            Box::new(move |env| {
                let v = v(env)?;
                env.add(v);
                let res = r(env);
                env.remove();
                res
            })
        }
        ir::Expr::LetRec(n, _, v, r) => {
            let n = *n;
            let v = strip_pis(v);
            if let ir::Expr::Lambda(params, body) = v {
                // let params = Rc::new(params.clone());
                // let body = Rc::new(compile_expr(&body, env));
                // let r = compile_expr(r, env);
                // Box::new(move |env| {
                //     let original = env.clone();
                //     let env = env.clone();
                //     let params = params.clone();
                //     let body = body.clone();
                //     let f = Rc::new_cyclic(move |this| {
                //         let this: Weak<_> = this.clone();
                //         let this: Weak<dyn Fn(&[Value]) -> Result<Value, Trap>> = unsize(this);
                //         move |args: &[Value]| -> Result<Value, Trap> {
                //             assert_eq!(params.len(), args.len());
                //             let mut env = env.clone();
                //             for (p, a) in params.iter().zip(args) {
                //                 env = add(env, p.name, a.clone());
                //             }
                //             env = add(env, n, Value::Fn(this.upgrade().unwrap()));
                //             body(&env)
                //         }
                //     });
                //     let env = add(original, n, Value::Fn(f));
                //     r(&env)
                // })

                let mut captured_names = HashSet::new();
                free_names(v, &mut captured_names);
                captured_names.remove(&n);
                let mut inner_env = EnvResolver::from_other(&env);
                let mut captures = Vec::with_capacity(captured_names.len());
                for &c in &captured_names {
                    match env.lookup(c) {
                        ResolvedName::Value(_) => continue,
                        ResolvedName::Name(l) => {
                            captures.push(l);
                            inner_env.add(c);
                        }
                    }
                }
                for param in params {
                    inner_env.add(param.name);
                }
                inner_env.add(n);
                let body = Rc::new(compile_expr(body, &mut inner_env));
                inner_env.remove(n);
                for param in params.iter().rev() {
                    inner_env.remove(param.name);
                }
                assert_eq!(inner_env.names.len(), captures.len());
                env.add(n);
                let rest = compile_expr(r, env);
                env.remove(n);
                Box::new(move |env| {
                    let captured_values: Rc<[Value]> = captures
                        .iter()
                        .map(|n| env.lookup(*n))
                        .collect::<Vec<_>>()
                        .into();
                    let body = body.clone();
                    let f = Rc::new_cyclic(move |this| {
                        let this: Weak<_> = this.clone();
                        let this: Weak<Callable> = unsize(this);
                        move |env: &mut CompactEnv, args: &[Value]| -> Result<Value, Trap> {
                            let prev_frame = env.open_frame();
                            for v in captured_values.iter() {
                                env.add(v.clone());
                            }
                            for arg in args {
                                env.add(arg.clone());
                            }
                            let this = Value::Fn(Function(this.upgrade().unwrap()));
                            env.add(this);
                            let res = body(env);
                            env.remove();
                            for _ in args {
                                env.remove();
                            }
                            for _ in captured_values.iter() {
                                env.remove();
                            }
                            env.pop_frame(prev_frame);
                            res
                        }
                    });
                    env.add(Value::Fn(Function(f)));
                    let res = rest(env);
                    env.remove();
                    res
                })

            } else {
                panic!("can't eval non-lambda recursive defs");
            }
        }
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => compile_expr(e, env),
        ir::Expr::Trap(msg, _) => {
            let msg = msg.clone();
            Box::new(move |_| Err(Trap { message: msg.clone() }))
        }
    }
}

fn unsize(f: Weak<impl Fn(&mut CompactEnv, &[Value]) -> Result<Value, Trap> + 'static>) -> Weak<Callable> {
    f
}

fn strip_pis(expr: &ir::Expr) -> &ir::Expr {
    if let ir::Expr::Pi(_, e) = expr {
        strip_pis(e)
    } else {
        expr
    }
}

fn free_names(expr: &ir::Expr, result: &mut HashSet<ir::Name>) {
    match expr {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Trap(_, _) => {}
        ir::Expr::Name(n) => {
            result.insert(*n);
        }
        ir::Expr::Call(f, args) => {
            free_names(f, result);
            for a in args {
                free_names(a, result);
            }
        }
        ir::Expr::If(c, t, e) => {
            free_names(c, result);
            free_names(t, result);
            free_names(e, result);
        }
        ir::Expr::Op(a, _, b) => {
            free_names(a, result);
            free_names(b, result);
        }
        ir::Expr::Intrinsic(_, args) |
        ir::Expr::Construct(_, _, args) => {
            for a in args {
                free_names(a, result);
            }
        }
        ir::Expr::Lambda(params, body) => {
            free_names(body, result);
            for p in params {
                result.remove(&p.name);
            }
        }
        ir::Expr::Match(d, br) => {
            free_names(d, result);
            for b in br {
                free_names(&b.value, result);
                for binding in &b.bindings {
                    result.remove(binding);
                }
            }
        }
        ir::Expr::Let(b, v, r) |
        ir::Expr::LetRec(b, _, v, r) => {
            free_names(v, result);
            free_names(r, result);
            result.remove(b);
        }
        ir::Expr::Pi(_, x) |
        ir::Expr::PiApply(x, _) => {
            free_names(x, result);
        }
    }
}
