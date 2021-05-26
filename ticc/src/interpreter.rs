use std::fmt::Write;
use ticc_core::ir;
use ticc_eval::Value;
use crate::{Compilation, Trap};

pub(crate) fn eval(compilation: &mut Compilation) -> Result<String, Trap> {
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

fn write_value(value: &Value, atom: bool, names: &ir::NameGenerator<'_>, into: &mut String) {
    match value {
        Value::Int(i) => {
            write!(into, "{}", i).unwrap();
        }
        Value::Bool(b) => {
            write!(into, "{}", b).unwrap();
        }
        Value::Composite(ctor, fields) => {
            if atom && fields.len() > 0 {
                into.push('(');
            }
            into.push_str(names.debug_info(*ctor));
            for field in fields.iter() {
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
