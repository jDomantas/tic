use std::fmt::Write;
use ticc_core::ir;
use ticc_eval::Value;
use crate::{CompilationUnit, Trap, InterpretError};

pub(crate) fn eval(compilation: &mut CompilationUnit) -> Result<String, Trap> {
    let program = crate::codegen::emit_core(compilation);
    let mut output = String::new();
    for (i, v) in program.values.iter().enumerate() {
        let Some(name) = &v.export_name else { continue };
        if v.export_name.is_none() {
            continue;
        }
        let mut expr = v.value.clone();
        for v in program.values[..i].iter().rev() {
            expr = ir::Expr::Let(v.name, Box::new(v.value.clone()), Box::new(expr));
        }
        match ticc_eval::eval_expr(&expr) {
            Ok(value) => {
                output.push_str(name);
                output.push_str(" = ");
                write_value(&value, false, &program.names, &mut output);
                output.push('\n');
            }
            Err(ticc_eval::Trap { message }) => return Err(Trap { message }),
        }
    }
    Ok(output)
}

pub(crate) fn eval_main(compilation: &mut CompilationUnit, input: &str) -> Result<String, InterpretError> {
    let program = crate::codegen::emit_core(compilation);
    let mut expr = None;
    for v in program.values.iter().rev() {
        if v.export_name.as_deref() == Some("main") {
            match &v.ty {
                ir::Ty::Fn(a, b) => match (a.as_slice(), &**b) {
                    (&[ir::Ty::String], ir::Ty::String) => {
                        // ok
                    }
                    _ => return Err(InterpretError::InvalidMain),
                },
                _ => return Err(InterpretError::InvalidMain),
            }
            expr = Some(ir::Expr::Call(
                Box::new(v.value.clone()),
                Vec::from([
                    ir::Expr::String(input.into()),
                ]),
            ));
        } else if let Some(e) = expr {
            expr = Some(ir::Expr::Let(v.name, Box::new(v.value.clone()), Box::new(e)));
        }
    }
    let Some(expr) = expr else {
        return Err(InterpretError::NoMain);
    };
    match ticc_eval::eval_expr(&expr) {
        Ok(Value::String(s)) => Ok(s.to_string()),
        Ok(_) => panic!("got incorrect value"),
        Err(t) => Err(InterpretError::Trap(Trap { message: t.message })),
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
        Value::Composite(tagged) => {
            if atom && tagged.fields().len() > 0 {
                into.push('(');
            }
            into.push_str(names.debug_info(tagged.tag()));
            for field in tagged.fields().iter() {
                into.push(' ');
                write_value(field, true, names, into);
            }
            if atom && tagged.fields().len() > 0 {
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
