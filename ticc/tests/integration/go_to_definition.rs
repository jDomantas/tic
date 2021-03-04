use crate::common;

fn check_def_span(source: &str, expected: &str) {
    let (source, pos) = common::extract_single_pos(source);
    let (expected, span) = common::extract_single_span(expected);
    assert_eq!(source, expected, "sources must match");

    let mut compilation = ticc::Compilation::from_source(&source);
    let def_span = compilation.find_definition(pos).expect("no def span");
    assert_eq!(span, def_span);
}

fn check_no_def(source: &str) {
    let (source, pos) = common::extract_single_pos(source);
    let mut compilation = ticc::Compilation::from_source(&source);
    let def_span = compilation.find_definition(pos);
    if def_span.is_some() {
        panic!("expected no span for: {}", source);
    }
}

#[test]
fn find_definition() {
    let tests = [
        (
            "let a = 1; let b = a{};",
            "let {a} = 1; let b = a;",
        ),
        (
            "type A a = A{} a ;",
            "type A a = {A} a ;",
        ),
        (
            "type A a = A {}a;",
            "type A {a} = A a;",
        ),
        (
            "type A a = A a{};",
            "type A {a} = A a;",
        ),
        (
            "type List a = Nil{} | Cons a rec;",
            "type List a = {Nil} | Cons a rec;"
        ),
        (
            "type List a = Nil | Co{}ns a rec;",
            "type List a = Nil | {Cons} a rec;"
        ),
        (
            "type List a = Nil | Cons{} a rec;",
            "type List a = Nil | {Cons} a rec;"
        ),
        (
            "type List a = Nil | Cons {}a rec;",
            "type List {a} = Nil | Cons a rec;"
        ),
    ];
    for &(from, to) in &tests {
        check_def_span(from, to);
    }
}

#[test]
fn no_definition() {
    let tests = [
        "let a = 1{};",
        "let{} a = 1;",
        "let a {}= 1;",
        "let a = 1; let b ={} a;",
        "let a = 1; let b = a {};",
        "let a = 1; let b = a;{}",
        "type List a = Nil | Cons a r{}ec;",
    ];
    for &test in &tests {
        check_no_def(test);
    }
}
