use crate::common;

fn check_completions(source: &str, expected: &[&str]) {
    let (source, pos) = common::extract_single_pos(source);
    let mut expected = expected.iter().copied().map(|s| s.to_owned()).collect::<Vec<_>>();
    expected.sort();

    let mut compilation = common::single_file_compilation(&source);
    let completions = compilation.completions(pos).unwrap_or_else(|| {
        panic!("no completions at pos for {:?}", source);
    });
    let actual = completions.into_iter().map(|c| c.name).collect::<Vec<_>>();

    assert_eq!(actual, expected, "completions for {:?}", source);
}

fn check_completionswith_import(source: &str, dep_source: &str, expected: &[&str]) {
    let (source, pos) = common::extract_single_pos(source);
    let mut expected = expected.iter().copied().map(|s| s.to_owned()).collect::<Vec<_>>();
    expected.sort();

    let mut compilation = common::compilation_with_dep(&source, dep_source);
    let completions = compilation.completions(pos).unwrap_or_else(|| {
        panic!("no completions at pos for {:?}", source);
    });
    let actual = completions.into_iter().map(|c| c.name).collect::<Vec<_>>();

    assert_eq!(actual, expected, "completions for {:?}", source);
}

fn check_no_completions(source: &str) {
    let (source, pos) = common::extract_single_pos(source);
    let mut compilation = common::single_file_compilation(&source);
    let completions = compilation.completions(pos);
    if let Some(completions) = completions {
        let actual = completions.into_iter().map(|c| c.name).collect::<Vec<_>>();
        panic!("expected no completions for: {}, got {:?}", source, actual);
    }
}

#[test]
fn completions() {
    let tests: &[(&str, &[&str])] = &[
        (
            "let a = 1; let b = {};",
            &["a"],
        ),
        (
            r#"let b = \x -> {}"#,
            &["x"],
        ),
        (
            "type T = A | B; let a = {}",
            &["A", "B"],
        ),
        (
            "let a = 1; let b = 2; let c = {}",
            &["a", "b"],
        ),
        (
            "let a = let b = 2; {}",
            &["b"],
        ),
        (
            r#"let f = \x -> \y -> x + {}"#,
            &["x", "y"],
        ),
        (
            "let a = {};",
            &[],
        ),
        (
            "type List = Nil | Cons {}",
            &[],
        ),
    ];
    for &(source, results) in tests {
        check_completions(source, results);
    }
}

#[test]
fn no_completions() {
    let tests = [
        "let{} a = 1;",
        "let {}",
        "type List = Nil | {}",
    ];
    for &test in &tests {
        check_no_completions(test);
    }
}

#[test]
fn import_completions() {
    let tests: &[(&str, &str, &[&str])] = &[
        (
            "export let a : int = 1; export let b : int = 2;",
            r#"import dep from "dep"; let a = dep.{};"#,
            &["a", "b"],
        ),
        (
            "export let a : int = 1; export let b : int = 2;",
            r#"import dep from "dep"; let a = {};"#,
            &["dep"],
        ),
        (
            "export type Foo = Bar;",
            r#"import dep from "dep"; let a = match true with | {}"#,
            &["dep"],
        ),
        (
            "export type Foo = export Bar;",
            r#"import dep from "dep"; let a = match true with | dep.{}"#,
            &["Bar"],
        ),
        (
            "export type Foo = Bar;",
            r#"import dep from "dep"; let a = match true with | dep.{}"#,
            &[],
        ),
        (
            "export type Foo = Bar;",
            r#"import (Foo) from "dep"; let a : {}"#,
            &["Foo"],
        ),
    ];
    for &(dep, source, results) in tests {
        check_completionswith_import(source, dep, results);
    }
}
