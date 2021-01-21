use crate::{Compilation, Pos};

#[test]
fn go_to_definition() {
    let mut compilation = Compilation::from_source(
        r#"type A a = A  a ;"#,
        // 012345678901234567
    );

    let mut def_pos = |pos: Pos| compilation.find_definition(pos).map(|s| s.start);

    assert_eq!(None, def_pos(Pos::new(13)));
    assert_eq!(Some(Pos::new(7)), def_pos(Pos::new(14)));
    assert_eq!(Some(Pos::new(7)), def_pos(Pos::new(15)));
    assert_eq!(None, def_pos(Pos::new(16)));
    assert_eq!(None, def_pos(Pos::new(17)));
}

#[test]
fn go_to_definition_2() {
    let mut compilation = Compilation::from_source(
        r#"type List a = Nil | Cons a rec;"#,
        // 0123456789012345678901234567890
    );

    let mut def_pos = |pos: Pos| compilation.find_definition(pos).map(|s| s.start);

    assert_eq!(None, def_pos(Pos::new(24)));
    assert_eq!(Some(Pos::new(10)), def_pos(Pos::new(25)));
    assert_eq!(Some(Pos::new(10)), def_pos(Pos::new(26)));
    assert_eq!(None, def_pos(Pos::new(27)));
}
