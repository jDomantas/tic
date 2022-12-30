use std::fmt::Write;
use ticc_core::ir;
use ticc_eval::Value;
use crate::{CompilationUnit, Trap, InterpretError};

pub(crate) fn eval(compilation: &mut CompilationUnit) -> Result<String, Trap> {
    let program = crate::codegen::emit_core(compilation);
    let mut env = ticc_eval::Env::new();
    let mut output = String::new();
    for v in &program.values {
        match ticc_eval::eval(env.clone(), &v.value) {
            Ok(value) => {
                if let Some(name) = &v.export_name {
                    output.push_str(name);
                    output.push_str(" = ");
                    write_value(&value, false, &program.names, &mut output);
                    output.push('\n');
                }
                env.add(v.name, value);
            }
            Err(ticc_eval::Trap { message }) => return Err(Trap { message }),
        }
    }
    Ok(output)
}

pub(crate) fn eval_main(compilation: &mut CompilationUnit, input: &str) -> Result<String, InterpretError> {
    let program = crate::codegen::emit_core(compilation);
    let mut main = None;
    for v in program.values.iter().rev() {
        if v.export_name.as_deref() == Some("main") {
            match &v.ty {
                ir::Ty::Fn(a, b) => match (a.as_slice(), &**b) {
                    (&[ir::Ty::String], ir::Ty::String) => {
                        main = Some(v.name);
                    }
                    _ => return Err(InterpretError::InvalidMain),
                },
                _ => return Err(InterpretError::InvalidMain),
            }
        }
    }
    let main = match main {
        Some(m) => m,
        None => return Err(InterpretError::NoMain),
    };
    let mut env = ticc_eval::Env::new();
    let mut main_to_run = None;
    for v in &program.values {
        match ticc_eval::eval(env.clone(), &v.value) {
            Ok(value) => {
                if v.name == main {
                    main_to_run = Some(value.clone());
                }
                env.add(v.name, value);
            }
            Err(ticc_eval::Trap { message }) => return Err(InterpretError::Trap(Trap { message })),
        }
    }
    let Some(Value::Fn(main_fn)) = main_to_run else {
        panic!("main exists but not function value was obtained");
    };
    match main_fn.call(&[Value::String(input.into())]) {
        Ok(Value::String(output)) => Ok(output.as_ref().to_owned()),
        Ok(_) => panic!("main returned non-string"),
        Err(ticc_eval::Trap { message }) => Err(InterpretError::Trap(Trap { message })),
    }
}

fn write_value(value: &Value, atom: bool, names: &ir::NameGenerator<'_>, into: &mut String) {
    match value {
        Value::Int(i) => {
            write!(into, "{}", i).unwrap();
        }
        Value::Bool(b) => {
            write!(into, "{}", b).unwrap();
        }
        Value::String(s) => {
            write!(into, "{:?}", s).unwrap();
        }
        Value::Composite(ctor, fields) => {
            if atom && fields.len() > 0 {
                into.push('(');
            }
            into.push_str(names.debug_info(*ctor));
            for field in fields.iter() {
                into.push(' ');
                write_value(field, true, names, into);
            }
            if atom && fields.len() > 0 {
                into.push(')');
            }
        }
        Value::Fn(_) => {
            if atom {
                into.push_str("(\\_ -> _)");
            } else {
                into.push_str("\\_ -> _");
            }
        }
    }
}
