use crate::common;

#[test]
fn no_errors() {
    let mut compilation = common::single_file_compilation(r#"
        type List a = Nil | Cons a rec;

        export let map : (a -> b) -> List a -> List b =
            \f -> \fold list ->
                match list with
                | Nil -> Nil
                | Cons x xs -> Cons (f x) xs
                end;
    "#);

    let diagnostics = compilation.diagnostics().collect::<Vec<_>>();

    assert!(diagnostics.is_empty());
}
