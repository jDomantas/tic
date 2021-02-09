use crate::Error;
use crate::compiler::ir;
use crate::compiler::syntax::{node, AstNode};

pub(crate) fn check_numbers(item: &mut ir::Item) {
    let syntax = item.syntax.tree.root();
    let errors = &mut item.errors;
    syntax.for_each_descendant(|node| {
        if let Some(num) = node::NumberExpr::cast(node.clone()) {
            let token = num.token();
            let err = match parse_int(token.text()) {
                Ok(_) => None,
                Err(ParseError::InvalidDigit) => Some("invalid number"),
                Err(ParseError::Overflow) => Some("number is too large"),
            };
            if let Some(err) = err {
                errors.push(Error {
                    message: err.to_owned(),
                    span: token.span(),
                });
            }
        }
    })
}

enum ParseError {
    Overflow,
    InvalidDigit,
}

fn parse_int(s: &str) -> Result<u64, ParseError> {
    assert!(!s.is_empty());
    let mut acc = 0u64;
    for c in s.chars() {
        let digit = c.to_digit(10).ok_or(ParseError::InvalidDigit)?;
        acc = acc.checked_mul(10)
            .and_then(|acc| acc.checked_add(u64::from(digit)))
            .ok_or(ParseError::Overflow)?;
    }
    Ok(acc)
}
