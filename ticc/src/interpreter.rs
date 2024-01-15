use std::fmt::Write;
use ticc_core::ir;
use ticc_eval::Value;
use crate::{CompilationUnit, Trap, InterpretError};

pub(crate) fn eval(compilation: &mut CompilationUnit) -> Result<String, Trap> {
    let program = crate::codegen::emit_core(compilation);
    let mut export_names = Vec::new();
    let mut expor_exprs = Vec::new();
    for v in &program.defs {
        if let Some(name) = &v.export_name {
            export_names.push(name);
            expor_exprs.push(ir::Expr::Name(v.name));
        }
    }
    match ticc_eval::eval(&program, &expor_exprs) {
        Ok(values) => {
            let mut output = String::new();
            for (n, v) in std::iter::zip(export_names, values) {
                output.push_str(n);
                output.push_str(" = ");
                write_value(&v, false, &program.names, &mut output);
                output.push('\n');
            }
            Ok(output)
        }
        Err(ticc_eval::Trap { message }) => return Err(Trap { message }),
    }
}

pub(crate) fn eval_main(compilation: &mut CompilationUnit, input: &[u8]) -> Result<Vec<u8>, InterpretError> {
    let verify = compilation.options.verify;
    let program = crate::codegen::emit_core(compilation);
    let mut main = None;
    for v in program.defs.iter().rev() {
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
    let expr = ir::Expr::Call(
        Box::new(ir::Expr::Name(main)),
        Vec::from([
            ir::Expr::String(input.into()),
        ]),
    );
    if verify {
        ticc_core::assert_valid_with_values(&program, &[(expr.clone(), ir::Ty::String)]);
    }
    match ticc_eval::eval(&program, &[expr]) {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Value::String(x) => Ok(x[..].to_owned()),
                _ => unreachable!(),
            }
        }
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
        Value::Composite(tagged) => {
            if atom && tagged.fields().len() > 0 {
                into.push('(');
            }
            into.push_str(names.original_name(tagged.tag()));
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
