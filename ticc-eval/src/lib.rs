use std::{
    collections::HashMap,
    fmt,
    rc::Rc,
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
    eval_impl(&env, expr)
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
                    let s = s.get(start..).unwrap_or("").get(..len).unwrap_or("");
                    Value::String(s.into())
                }
                ir::Intrinsic::StringFromChar => {
                    let ch = eval_impl(env, &xs[0])?.into_int() as u8 as char;
                    Value::String(ch.to_string().into())
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
    Value::Fn(Rc::new(move |args| {
        let base_env = env.clone();
        assert_eq!(params.len(), args.len());
        let mut env = env.clone();
        for (p, a) in params.iter().zip(args) {
            env = add(env, p.name, a.clone());
        }
        env = add(env, name, make_rec_fn(base_env, name, params.clone(), body.clone()));
        eval_impl(&env, &body)
    }))
}

fn strip_pis(expr: &ir::Expr) -> &ir::Expr {
    if let ir::Expr::Pi(_, e) = expr {
        strip_pis(e)
    } else {
        expr
    }
}
