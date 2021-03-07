use ticc::Compilation;

#[test]
fn no_errors() {
    let mut compilation = Compilation::from_source(r#"
        type List a = Nil | Cons a rec;

        let map : (a -> b) -> List a -> List b =
            \f -> \fold list ->
                match list with
                | Nil -> Nil
                | Cons x xs -> Cons (f x) xs
                end;
    "#);

    let diagnostics = compilation.diagnostics().collect::<Vec<_>>();

    assert!(diagnostics.is_empty());
}
