pub mod lambda;
mod tagged;

use std::fmt;
use ticc_core::ir::ByteString;
use crate::tagged::Tagged;

#[derive(Clone)]
pub enum Value<F> {
    Int(u64),
    Bool(bool),
    String(ByteString),
    Composite(Tagged<F>),
    Fn(F),
}

impl<F> fmt::Debug for Value<F> {
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

impl<F> Value<F> {
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

    fn into_composite(self) -> Tagged<F> {
        if let Value::Composite(x) = self {
            x
        } else {
            panic!("value is not a composite");
        }
    }

    fn into_fn(self) -> F {
        if let Value::Fn(x) = self {
            x
        } else {
            panic!("value is not an fn");
        }
    }
}

#[derive(Debug)]
pub struct Trap {
    pub message: String,
}
