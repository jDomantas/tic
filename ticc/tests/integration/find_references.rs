use crate::common;

fn check_references(source: &str, expected: &[&str]) {
    let (source, pos) = common::extract_single_pos(source);
    let expected_spans = expected
        .iter()
        .map(|&s| {
            let (src, span) = common::extract_single_span(s);
            assert_eq!(source, src, "sources must match");
            span
        })
        .collect::<Vec<_>>();

    let mut compilation = ticc::Compilation::from_source(&source);
    let refs = compilation.find_references(pos).unwrap_or_else(|| {
        panic!("no symbol at pos for {:?}", source);
    });
    assert_eq!(refs, expected_spans, "references for {:?}", source);
}

fn check_no_refs(source: &str) {
    let (source, pos) = common::extract_single_pos(source);
    let mut compilation = ticc::Compilation::from_source(&source);
    let refs = compilation.find_references(pos);
    if refs.is_some() {
        panic!("expected no span for: {}", source);
    }
}

#[test]
fn find_references() {
    let tests: &[(&str, &[&str])] = &[
        (
            "let a{} = 1; let b = a;",
            &["let a = 1; let b = {a};"],
        ),
        (
            "let a = 1; let b = a{};",
            &["let a = 1; let b = {a};"],
        ),
        (
            "type A{} a = A a ;",
            &[],
        ),
        (
            "type A {}a = A a;",
            &["type A a = A {a};"],
        ),
        (
            "let a{} = 1; let b = a + a;",
            &[
                "let a = 1; let b = {a} + a;",
                "let a = 1; let b = a + {a};",
            ],
        ),
        (
            "let a = 1; let b = a{} + a;",
            &[
                "let a = 1; let b = {a} + a;",
                "let a = 1; let b = a + {a};",
            ],
        ),
        (
            "let a = 1; let b = a + a{};",
            &[
                "let a = 1; let b = {a} + a;",
                "let a = 1; let b = a + {a};",
            ],
        ),
    ];
    for &(from, to) in tests {
        check_references(from, to);
    }
}

#[test]
fn no_references() {
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
        check_no_refs(test);
    }
}
