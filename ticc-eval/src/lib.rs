use std::{
    collections::HashMap,
    fmt,
    rc::{Rc, Weak},
};
use ticc_core::ir;

#[derive(Clone)]
pub enum Value {
    Int(u64),
    Bool(bool),
    String(Rc<str>),
    Composite(ir::Name, Rc<[Value]>),
    Fn(Rc<dyn Fn(&[Value]) -> Result<Value, Trap>>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(x) => f.debug_tuple("Int").field(x).finish(),
            Value::Bool(x) => f.debug_tuple("Bool").field(x).finish(),
            Value::String(x) => f.debug_tuple("String").field(x).finish(),
            Value::Composite(x, xs) => {
                let mut s = f.debug_tuple("Composite");
                s.field(x);
                for x in xs.iter() {
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

    fn into_string(self) -> Rc<str> {
        if let Value::String(x) = self {
            x
        } else {
            panic!("value is not a string");
        }
    }

    fn into_composite(self) -> (ir::Name, Rc<[Value]>) {
        if let Value::Composite(x, y) = self {
            (x, y)
        } else {
            panic!("value is not a composite");
        }
    }

    fn into_fn(self) -> Rc<dyn Fn(&[Value]) -> Result<Value, Trap>> {
        if let Value::Fn(x) = self {
            x
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

    fn lookup(&self, name: ir::Name) -> Value {
        self.values[&name].clone()
    }
}

#[derive(Debug)]
pub struct Trap {
    pub message: String,
}

trait EvalEnv {
    fn lookup(&self, name: ir::Name) -> Value;
}

struct ExtendedEnv {
    base: Rc<dyn EvalEnv>,
    name: ir::Name,
    value: Value,
}

impl EvalEnv for Env {
    fn lookup(&self, name: ir::Name) -> Value {
        self.lookup(name)
    }
}

impl EvalEnv for ExtendedEnv {
    fn lookup(&self, name: ir::Name) -> Value {
        if self.name == name {
            self.value.clone()
        } else {
            self.base.lookup(name)
        }
    }
}

fn add(env: Rc<dyn EvalEnv>, name: ir::Name, value: Value) -> Rc<dyn EvalEnv> {
    Rc::new(ExtendedEnv {
        base: env,
        name,
        value,
    })
}

pub fn eval(env: Env, expr: &ir::Expr) -> Result<Value, Trap> {
    let env: Rc<dyn EvalEnv> = Rc::new(env);
    // eval_impl(&env, expr)
    let compiled = compile_expr(expr);
    compiled(&env)
}

trait Subvalue: 'static {
    fn from_int(x: u64) -> Self;
    fn from_bool(x: bool) -> Self;
    fn from_string(x: Rc<str>) -> Self;
    fn from_composite(tag: ir::Name, fields: Rc<[Value]>) -> Self;
    fn from_fn(f: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self;
    fn from_value(v: Value) -> Self;
}

impl Subvalue for bool {
    fn from_int(_: u64) -> Self {
        panic!("value is not a bool")
    }

    fn from_bool(x: bool) -> Self {
        x
    }

    fn from_string(_: Rc<str>) -> Self {
        panic!("value is not a bool")
    }

    fn from_composite(_: ir::Name, _: Rc<[Value]>) -> Self {
        panic!("value is not a bool")
    }

    fn from_fn(_: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        panic!("value is not a bool")
    }

    fn from_value(v: Value) -> Self {
        v.into_bool()
    }
}

impl Subvalue for u64 {
    fn from_int(x: u64) -> Self {
        x
    }

    fn from_bool(_: bool) -> Self {
        panic!("value is not an int")
    }

    fn from_string(_: Rc<str>) -> Self {
        panic!("value is not an int")
    }

    fn from_composite(_: ir::Name, _: Rc<[Value]>) -> Self {
        panic!("value is not an int")
    }

    fn from_fn(_: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        panic!("value is not an int")
    }

    fn from_value(v: Value) -> Self {
        v.into_int()
    }
}

impl Subvalue for Rc<str> {
    fn from_int(_: u64) -> Self {
        panic!("value is not a string")
    }

    fn from_bool(_: bool) -> Self {
        panic!("value is not a string")
    }

    fn from_string(x: Rc<str>) -> Self {
        x
    }

    fn from_composite(_: ir::Name, _: Rc<[Value]>) -> Self {
        panic!("value is not a string")
    }

    fn from_fn(_: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        panic!("value is not a string")
    }

    fn from_value(v: Value) -> Self {
        v.into_string()
    }
}

impl Subvalue for (ir::Name, Rc<[Value]>) {
    fn from_int(_: u64) -> Self {
        panic!("value is not a composite")
    }

    fn from_bool(_: bool) -> Self {
        panic!("value is not a composite")
    }

    fn from_string(_: Rc<str>) -> Self {
        panic!("value is not a composite")
    }

    fn from_composite(tag: ir::Name, fields: Rc<[Value]>) -> Self {
        (tag, fields)
    }

    fn from_fn(_: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        panic!("value is not a composite")
    }

    fn from_value(v: Value) -> Self {
        v.into_composite()
    }
}

impl Subvalue for Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static> {
    fn from_int(_: u64) -> Self {
        panic!("value is not a composite")
    }

    fn from_bool(_: bool) -> Self {
        panic!("value is not a composite")
    }

    fn from_string(_: Rc<str>) -> Self {
        panic!("value is not a composite")
    }

    fn from_composite(_: ir::Name, _: Rc<[Value]>) -> Self {
        panic!("value is not a composite")
    }

    fn from_fn(f: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        f
    }

    fn from_value(v: Value) -> Self {
        v.into_fn()
    }
}

impl Subvalue for Value {
    fn from_int(x: u64) -> Self {
        Value::Int(x)
    }

    fn from_bool(x: bool) -> Self {
        Value::Bool(x)
    }

    fn from_string(x: Rc<str>) -> Self {
        Value::String(x)
    }

    fn from_composite(tag: ir::Name, fields: Rc<[Value]>) -> Self {
        Value::Composite(tag, fields)
    }

    fn from_fn(f: Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Self {
        Value::Fn(f)
    }

    fn from_value(v: Value) -> Self {
        v
    }
}


fn compile_expr<T: Subvalue>(expr: &ir::Expr) -> Box<dyn Fn(&Rc<dyn EvalEnv>) -> Result<T, Trap>> {
    match expr {
        ir::Expr::Bool(b) => {
            let b = *b;
            Box::new(move |_| Ok(T::from_bool(b)))
        }
        ir::Expr::Int(i) => {
            let i = *i;
            Box::new(move |_| Ok(T::from_int(i)))
        }
        ir::Expr::String(s) => {
            let s = s.clone();
            Box::new(move |_| Ok(T::from_string(s.clone())))
        }
        ir::Expr::Name(n) => {
            let n = *n;
            Box::new(move |env| Ok(T::from_value(env.lookup(n))))
        }
        ir::Expr::Call(f, args) => {
            let f = compile_expr::<Rc<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static>>(f);
            match args.len() {
                0 => Box::new(move |env| Ok(T::from_value(f(env)?(&[])?))),
                1 => {
                    let arg = compile_expr(&args[0]);
                    Box::new(move |env| {
                        let f = f(env)?;
                        let arg = arg(env)?;
                        Ok(T::from_value(f(&[arg])?))
                    })
                }
                2 => {
                    let arg1 = compile_expr(&args[0]);
                    let arg2 = compile_expr(&args[1]);
                    Box::new(move |env| {
                        let f = f(env)?;
                        let arg1 = arg1(env)?;
                        let arg2 = arg2(env)?;
                        Ok(T::from_value(f(&[arg1, arg2])?))
                    })
                }
                3 => {
                    let arg1 = compile_expr(&args[0]);
                    let arg2 = compile_expr(&args[1]);
                    let arg3 = compile_expr(&args[2]);
                    Box::new(move |env| {
                        let f = f(env)?;
                        let arg1 = arg1(env)?;
                        let arg2 = arg2(env)?;
                        let arg3 = arg3(env)?;
                        Ok(T::from_value(f(&[arg1, arg2, arg3])?))
                    })
                }
                _ => {
                    let args = args.iter().map(|e| compile_expr(e)).collect::<Vec<_>>();
                    Box::new(move |env| {
                        let f = f(env)?;
                        let mut arg_values = Vec::with_capacity(args.len());
                        for a in &args {
                            arg_values.push(a(env)?);
                        }
                        Ok(T::from_value(f(&arg_values)?))
                    })
                }
            }
        }
        ir::Expr::If(c, t, e) => {
            let c = compile_expr(c);
            let t = compile_expr(t);
            let e = compile_expr(e);
            Box::new(move |env| {
                if c(env)? {
                    t(env)
                } else {
                    e(env)
                }
            })
        }
        ir::Expr::Op(a, op, b) => {
            let a = compile_expr::<u64>(a);
            let b = compile_expr::<u64>(b);
            match op {
                ir::Op::Add => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_int(a.wrapping_add(b)))
                }),
                ir::Op::Subtract => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_int(a.wrapping_sub(b)))
                }),
                ir::Op::Multiply => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_int(a.wrapping_mul(b)))
                }),
                ir::Op::Divide => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_int(a.checked_div(b).unwrap_or(0)))
                }),
                ir::Op::Modulo => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_int(a.checked_rem(b).unwrap_or(a)))
                }),
                ir::Op::Less => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a < b))
                }),
                ir::Op::LessEq => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a <= b))
                }),
                ir::Op::Greater => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a > b))
                }),
                ir::Op::GreaterEq => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a >= b))
                }),
                ir::Op::Equal => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a == b))
                }),
                ir::Op::NotEqual => Box::new(move |env| {
                    let a = a(env)?;
                    let b = b(env)?;
                    Ok(T::from_bool(a != b))
                }),
            }
        }
        ir::Expr::Intrinsic(i, args) => {
            match i {
                ir::Intrinsic::StringLen => {
                    let arg = compile_expr::<Rc<str>>(&args[0]);
                    Box::new(move |env| {
                        let arg = arg(env)?;
                        Ok(T::from_int(arg.len() as u64))
                    })
                }
                ir::Intrinsic::StringConcat => {
                    let s1 = compile_expr::<Rc<str>>(&args[0]);
                    let s2 = compile_expr::<Rc<str>>(&args[1]);
                    Box::new(move |env| {
                        let s1 = s1(env)?;
                        let s2 = s2(env)?;
                        let mut res = String::with_capacity(s1.len() + s2.len());
                        res.push_str(&s1);
                        res.push_str(&s2);
                        Ok(T::from_string(res.into()))
                    })
                }
                ir::Intrinsic::StringCharAt => {
                    let idx = compile_expr::<u64>(&args[0]);
                    let s = compile_expr::<Rc<str>>(&args[1]);
                    Box::new(move |env| {
                        let idx = idx(env)? as usize;
                        let s = s(env)?;
                        let ch = s.as_bytes().get(idx).copied().unwrap_or(0);
                        Ok(T::from_int(u64::from(ch)))
                    })
                }
                ir::Intrinsic::StringSubstring => {
                    let start = compile_expr::<u64>(&args[0]);
                    let len = compile_expr::<u64>(&args[1]);
                    let s = compile_expr::<Rc<str>>(&args[2]);
                    Box::new(move |env| {
                        let start = start(env)? as usize;
                        let len = len(env)? as usize;
                        let s = s(env)?;
                        let s = s.get(start..).unwrap_or("");
                        let s = s.get(..len).unwrap_or(s);
                        Ok(T::from_string(s.into()))
                    })
                }
                ir::Intrinsic::StringFromChar => {
                    let ch = compile_expr::<u64>(&args[0]);
                    Box::new(move |env| {
                        let ch = ch(env)? as u8 as char;
                        Ok(T::from_string(ch.to_string().into()))
                    })
                }
                ir::Intrinsic::IntToString => {
                    let arg = compile_expr::<u64>(&args[0]);
                    Box::new(move |env| {
                        let arg = arg(env)?;
                        Ok(T::from_string(arg.to_string().into()))
                    })
                }
            }
        }
        ir::Expr::Lambda(params, body) => {
            let params = Rc::new(params.clone());
            let body = Rc::new(compile_expr(body));
            Box::new(move |env| {
                let env = env.clone();
                let params = params.clone();
                let body = body.clone();
                Ok(T::from_fn(Rc::new(move |args| {
                    assert_eq!(params.len(), args.len());
                    let mut env = env.clone();
                    for (p, a) in params.iter().zip(args) {
                        env = add(env, p.name, a.clone());
                    }
                    body(&env)
                })))
            })
        }
        ir::Expr::Match(x, branches) => {
            let x = compile_expr(x);
            let branches = branches
                .iter()
                .cloned()
                .map(|b| {
                    let compiled = compile_expr(&b.value);
                    (b, compiled)
                })
                .collect::<Vec<_>>();
            Box::new(move |env| {
                let (ctor, fields) = x(env)?;
                for (br, body) in &branches {
                    if br.ctor == ctor {
                        let mut env = env.clone();
                        for (b, f) in br.bindings.iter().zip(fields.iter()) {
                            env = add(env, *b, f.clone());
                        }
                        return body(&env);
                    }
                }
                panic!("no branch matched")
            })
        }
        ir::Expr::Construct(n, _, fields) => {
            let n = *n;
            let fields = fields.iter().map(|f| compile_expr(f)).collect::<Vec<_>>();
            Box::new(move |env| {
                let mut field_values = Vec::with_capacity(fields.len());
                for f in &fields {
                    field_values.push(f(env)?);
                }
                Ok(T::from_composite(n, field_values.into()))
            })
        }
        ir::Expr::Let(n, v, r) => {
            let n = *n;
            let v = compile_expr(v);
            let r = compile_expr(r);
            Box::new(move |env| {
                let v = v(env)?;
                let env = add(env.clone(), n, v);
                r(&env)
            })
        }
        ir::Expr::LetRec(n, _, v, r) => {
            let n = *n;
            let v = strip_pis(v);
            if let ir::Expr::Lambda(params, body) = v {
                let params = Rc::new(params.clone());
                let body = Rc::new(compile_expr(&body));
                let r = compile_expr(r);
                Box::new(move |env| {
                    let original = env.clone();
                    let env = env.clone();
                    let params = params.clone();
                    let body = body.clone();
                    let f = Rc::new_cyclic(move |this| {
                        let this: Weak<_> = this.clone();
                        let this: Weak<dyn Fn(&[Value]) -> Result<Value, Trap>> = unsize(this);
                        move |args: &[Value]| -> Result<Value, Trap> {
                            assert_eq!(params.len(), args.len());
                            let mut env = env.clone();
                            for (p, a) in params.iter().zip(args) {
                                env = add(env, p.name, a.clone());
                            }
                            env = add(env, n, Value::Fn(this.upgrade().unwrap()));
                            body(&env)
                        }
                    });
                    let env = add(original, n, Value::Fn(f));
                    r(&env)
                })
            } else {
                panic!("can't eval non-lambda recursive defs");
            }
        }
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => compile_expr(e),
        ir::Expr::Trap(msg, _) => {
            let msg = msg.clone();
            Box::new(move |_| Err(Trap { message: msg.clone() }))
        }
    }
}

fn unsize(f: Weak<impl Fn(&[Value]) -> Result<Value, Trap> + 'static>) -> Weak<dyn Fn(&[Value]) -> Result<Value, Trap> + 'static> {
    f
}

fn eval_impl(env: &Rc<dyn EvalEnv>, expr: &ir::Expr) -> Result<Value, Trap> {
    match expr {
        ir::Expr::Bool(b) => Ok(Value::Bool(*b)),
        ir::Expr::Int(i) => Ok(Value::Int(*i)),
        ir::Expr::String(s) => Ok(Value::String(s.clone())),
        ir::Expr::Name(n) => Ok(env.lookup(*n)),
        ir::Expr::Call(f, args) => {
            let f = eval_impl(env, f)?.into_fn();
            let args = args.iter()
                .map(|a| eval_impl(env, a))
                .collect::<Result<Vec<_>, _>>()?;
            f(&args)
        }
        ir::Expr::If(c, t, e) => {
            if eval_impl(env, c)?.into_bool() {
                eval_impl(env, t)
            } else {
                eval_impl(env, e)
            }
        }
        ir::Expr::Intrinsic(i, xs) => {
            Ok(match i {
                ir::Intrinsic::StringLen => {
                    let s = eval_impl(env, &xs[0])?.into_string();
                    Value::Int(s.len() as u64)
                }
                ir::Intrinsic::StringConcat => {
                    let s1 = eval_impl(env, &xs[0])?.into_string();
                    let s2 = eval_impl(env, &xs[1])?.into_string();
                    let mut res = String::with_capacity(s1.len() + s2.len());
                    res.push_str(&s1);
                    res.push_str(&s2);
                    Value::String(res.into())
                }
                ir::Intrinsic::StringCharAt => {
                    let idx = eval_impl(env, &xs[0])?.into_int() as usize;
                    let s = eval_impl(env, &xs[1])?.into_string();
                    let ch = s.as_bytes().get(idx).copied().unwrap_or(0);
                    Value::Int(u64::from(ch))
                }
                ir::Intrinsic::StringSubstring => {
                    let start = eval_impl(env, &xs[0])?.into_int() as usize;
                    let len = eval_impl(env, &xs[1])?.into_int() as usize;
                    let s = eval_impl(env, &xs[2])?.into_string();
                    let s = s.get(start..).unwrap_or("");
                    let s = s.get(..len).unwrap_or(s);
                    Value::String(s.into())
                }
                ir::Intrinsic::StringFromChar => {
                    let ch = eval_impl(env, &xs[0])?.into_int() as u8 as char;
                    Value::String(ch.to_string().into())
                }
                ir::Intrinsic::IntToString => {
                    let i = eval_impl(env, &xs[0])?.into_int();
                    Value::String(i.to_string().into())
                }
            })
        }
        ir::Expr::Op(a, op, b) => {
            let a = eval_impl(env, a)?.into_int();
            let b = eval_impl(env, b)?.into_int();
            Ok(match op {
                ir::Op::Add => Value::Int(a.wrapping_add(b)),
                ir::Op::Subtract => Value::Int(a.wrapping_sub(b)),
                ir::Op::Multiply => Value::Int(a.wrapping_mul(b)),
                ir::Op::Divide => Value::Int(a.checked_div(b).unwrap_or(0)),
                ir::Op::Modulo => Value::Int(a.checked_rem(b).unwrap_or(a)),
                ir::Op::Less => Value::Bool(a < b),
                ir::Op::LessEq => Value::Bool(a <= b),
                ir::Op::Greater => Value::Bool(a > b),
                ir::Op::GreaterEq => Value::Bool(a >= b),
                ir::Op::Equal => Value::Bool(a == b),
                ir::Op::NotEqual => Value::Bool(a != b),
            })
        }
        ir::Expr::Lambda(params, body) => {
            let env = env.clone();
            let params = params.clone();
            let body = body.clone();
            Ok(Value::Fn(Rc::new(move |args| {
                assert_eq!(params.len(), args.len());
                let mut env = env.clone();
                for (p, a) in params.iter().zip(args) {
                    env = add(env, p.name, a.clone());
                }
                eval_impl(&env, &body)
            })))
        }
        ir::Expr::Match(e, branches) => {
            let (ctor, fields) = eval_impl(env, e)?.into_composite();
            for br in branches {
                if br.ctor == ctor {
                    assert_eq!(br.bindings.len(), fields.len());
                    let mut env = env.clone();
                    for (b, f) in br.bindings.iter().zip(fields.iter()) {
                        env = add(env, *b, f.clone());
                    }
                    return eval_impl(&env, &br.value);
                }
            }
            unreachable!("no branch matched");
        }
        ir::Expr::Construct(ctor, _, fields) => {
            let mut f = Vec::new();
            for field in fields {
                f.push(eval_impl(env, field)?);
            }
            Ok(Value::Composite(*ctor, f.into()))
        }
        ir::Expr::Let(n, v, e) => {
            let v = eval_impl(env, v)?;
            let env = add(env.clone(), *n, v);
            eval_impl(&env, e)
        }
        ir::Expr::LetRec(n, _, v, e) => {
            let v = strip_pis(v);
            if let ir::Expr::Lambda(params, body) = v.clone() {
                let params = Rc::<[ir::LambdaParam]>::from(params);
                let body = Rc::new(*body);
                let f = make_rec_fn(env.clone(), *n, params, body);
                let env = add(env.clone(), *n, f);
                eval_impl(&env, e)
            } else {
                panic!("can't eval non-lambda recursive defs");
            }
        }
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => eval_impl(env, e),
        ir::Expr::Trap(msg, _) => Err(Trap {
            message: msg.clone(),
        }),
    }
}

fn make_rec_fn(
    env: Rc<dyn EvalEnv>,
    name: ir::Name,
    params: Rc<[ir::LambdaParam]>,
    body: Rc<ir::Expr>,
) -> Value {
    // Value::Fn(Rc::new(move |args| {
    //     let base_env = env.clone();
    //     assert_eq!(params.len(), args.len());
    //     let mut env = env.clone();
    //     for (p, a) in params.iter().zip(args) {
    //         env = add(env, p.name, a.clone());
    //     }
    //     env = add(env, name, make_rec_fn(base_env, name, params.clone(), body.clone()));
    //     eval_impl(&env, &body)
    // }))
    Value::Fn(Rc::new_cyclic(move |this| {
        let this: Weak<_> = this.clone();
        let this: Weak<dyn Fn(&[Value]) -> Result<Value, Trap>> = unsize(this);
        move |args: &[Value]| -> Result<Value, Trap> {
            assert_eq!(params.len(), args.len());
            let mut env = env.clone();
            for (p, a) in params.iter().zip(args) {
                env = add(env, p.name, a.clone());
            }
            env = add(env, name, Value::Fn(this.upgrade().unwrap()));
            eval_impl(&env, &body)
        }
    }))
}

fn strip_pis(expr: &ir::Expr) -> &ir::Expr {
    if let ir::Expr::Pi(_, e) = expr {
        strip_pis(e)
    } else {
        expr
    }
}
