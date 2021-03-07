use std::collections::HashSet;
use crate::codegen::cir;

pub(crate) fn verify(program: &cir::Program) {
    let mut scope = HashSet::new();
    assert_eq!(program.order.len(), program.values.len(), "order does not match defs");
    for &n in &program.order {
        assert!(program.values.contains_key(&n), "order name {:?} does not exist", n);
    }
    let mut defs = HashSet::new();
    for (&n, e) in &program.values {
        assert!(defs.insert(n), "name {:?} defined multiple times", n);
        single_def(e, &mut defs);
    }
    for &n in &program.order {
        let e = &program.values[&n];
        verify_scope(e, &mut scope);
        scope.insert(n);
    }
    assert_eq!(scope.len(), program.values.len());
    for &n in &defs {
        assert!(program.debug_info.contains_key(&n), "name {:?} does not have debug info", n);
    }
    for export in &program.exports {
        assert!(program.values.contains_key(&export.name), "exported name {:?} does not exist", export.name);
    }
}

fn verify_scope(expr: &cir::Expr, scope: &mut HashSet<cir::Name>) {
    match expr {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) => {}
        cir::Expr::Name(n) => assert!(scope.contains(n), "name {:?} not in scope", n),
        cir::Expr::Call(a, b) |
        cir::Expr::Op(a, _, b) => {
            verify_scope(a, scope);
            verify_scope(b, scope);
        }
        cir::Expr::If(a, b, c) => {
            verify_scope(a, scope);
            verify_scope(b, scope);
            verify_scope(c, scope);
        }
        cir::Expr::Lambda(n, a) => {
            scope.insert(*n);
            verify_scope(a, scope);
            scope.remove(n);
        }
        cir::Expr::Match(a, bs) => {
            verify_scope(a, scope);
            for b in bs {
                for &bind in &b.bindings {
                    scope.insert(bind);
                }
                verify_scope(&b.value, scope);
                for &bind in &b.bindings {
                    scope.remove(&bind);
                }
            }
        }
        cir::Expr::Construct(_, a) => {
            for a in a {
                verify_scope(a, scope);
            }
        }
        cir::Expr::Let(n, a, b) => {
            verify_scope(a, scope);
            scope.insert(*n);
            verify_scope(b, scope);
            scope.remove(n);
        }
        cir::Expr::LetRec(n, a, b) => {
            scope.insert(*n);
            verify_scope(a, scope);
            verify_scope(b, scope);
            scope.remove(n);
        }
    }
}

fn single_def(expr: &cir::Expr, defs: &mut HashSet<cir::Name>) {
    match expr {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) |
        cir::Expr::Name(_) => {}
        cir::Expr::Call(a, b) |
        cir::Expr::Op(a, _, b) => {
            single_def(a, defs);
            single_def(b, defs);
        }
        cir::Expr::If(a, b, c) => {
            single_def(a, defs);
            single_def(b, defs);
            single_def(c, defs);
        }
        cir::Expr::Lambda(n, a) => {
            assert!(defs.insert(*n), "name {:?} defined multiple times", n);
            single_def(a, defs);
        }
        cir::Expr::Match(a, bs) => {
            single_def(a, defs);
            for b in bs {
                for &bind in &b.bindings {
                    assert!(defs.insert(bind), "name {:?} defined multiple times", bind);
                }
                single_def(&b.value, defs);
            }
        }
        cir::Expr::Construct(_, a) => {
            for a in a {
                single_def(a, defs);
            }
        }
        cir::Expr::Let(n, a, b) => {
            assert!(defs.insert(*n), "name {:?} defined multiple times", n);
            single_def(a, defs);
            single_def(b, defs);
        }
        cir::Expr::LetRec(n, a, b) => {
            assert!(defs.insert(*n), "name {:?} defined multiple times", n);
            single_def(a, defs);
            single_def(b, defs);
        }
    }
}
