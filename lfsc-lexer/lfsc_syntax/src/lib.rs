pub mod ast;
pub mod sexp;

#[macro_export]
macro_rules! binder {
    (let $v:ident, $e:expr, $body:expr) => {
        $crate::ast::Term::App(
            Box::new(binder!(lam, $v, $body)),
            Box::new($e),
        )
    };
    (pi, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Pi,
            var: Some(var!($v)),
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
    (pi, $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Pi,
            var: None,
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
    (lam, $v:ident, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Lam,
            var: Some(var!($v)),
            ty: None,
            body: Box::new($body),
        }
    };
    (lam, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Lam,
            var: None,
            ty: None,
            body: Box::new($body),
        }
    };
    (lam, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Lam,
            var: Some(var!($v)),
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
    (biglam, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::BigLam,
            var: Some(var!($v)),
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
}

#[macro_export]
macro_rules! abinder {
    (lam, $body:expr) => {
        $crate::ast::AlphaTerm::Lam(Box::new($body))
    };
    (pi, $ty:expr, $body:expr) => {
        $crate::ast::AlphaTerm::Pi(Box::new($ty), Box::new($body))
    };
}

#[macro_export]
macro_rules! var {
    ($name:ident) => {
        $name
        // stringify!($name).to_owned()
    };
}

#[macro_export]
macro_rules! term {
    ($v:ident) => { $crate::ast::Term::Var(var!($v)) };
    (($($inner:tt)+)) => { term_!($($inner)+) };
}

#[macro_export]
macro_rules! term_ {
    ($v:ident) => {$crate::ast::Term::Var(var!($v))};
    (λ $v:ident $b:tt) => {
        binder!(lam, $v, term!($b))
    };
    (let $e:tt $b:tt) => {
        $crate::ast::Term::App(
            binder!(lam, term!($b)).into(),
            term!($e).into(),
        )
    };
    (let $v:ident $e:tt $b:tt) => {
        $crate::ast::Term::App {
            binder!(lam, $v, term!($b)).into(),
            term!($e).into(),
        }
    };
    ($($x:tt)+) => {
            rec!($($x)+)
   };
}

#[macro_export]
macro_rules! rec {
    ($x:tt) => {
        term!($x)
    };
    ($f:tt $($x:tt)+) => {
        app!(term!($f), rec!($($x)+))
    };
}

#[macro_export]
macro_rules! app {
    ($f:expr, $x:expr) => {
        $crate::ast::Term::App {
            fun: $f.into(),
            arg: $x.into(),
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::ast::Term::*;

    #[test]
    fn var_test() {
        let term = term!(x);
        assert_eq!(term, Var("x".to_owned()));
    }
    #[test]
    fn var_keyword_test() {
        let term = term!(let);
        assert_eq!(term, Var("let".to_owned()));
    }
    #[test]
    fn apply_test() {
        let term = term!((f x));
        assert_eq!(
            term,
            App {
                fun: Var("f".to_owned()).into(),
                arg: Box::new(Var("x".to_owned())),
            }
        );
    }
    #[test]
    fn nested_apply_test() {
        let term = term!((f g x));
        assert_eq!(
            term,
            App {
                fun: Box::new(Var("f".to_owned())),
                arg: Box::new(App {
                    fun: Box::new(Var("g".to_owned())),
                    arg: Box::new(Var("x".to_owned())),
                }),
            }
        );
    }
    #[test]
    fn simple_let_test() {
        let term = term!((let x y x));
        assert_eq!(
            term,
            App {
                fun: binder!(lam, x, term!(x)).into(),
                arg: Box::new(Var("y".to_owned()))
            }
        );
    }
    #[test]
    fn nameless_let_test() {
        let term = term!((let y x));
        assert_eq!(
            term,
            App {
                fun: binder!(lam, term!(x)).into(),
                arg: Box::new(Var("y".to_owned()))
            }
        );
    }
    #[test]
    fn let_test() {
        let term = term!((let x (f y) x));
        assert_eq!(
            term,
            App {
                fun: binder!(lam, x, term!(x)).into(),
                arg: App {
                    fun: Box::new(Var("f".to_owned())),
                    arg: Box::new(Var("y".to_owned()))
                }
                .into()
            }
        );
    }
    #[test]
    fn apply_is_let_test() {
        let term = term!(((let x y x) z));
        assert_eq!(
            term,
            App {
                fun: App {
                    fun: binder!(lam, x, term!(x)).into(),
                    arg: term!(y).into()
                }
                .into(),
                arg: Box::new(Var("z".to_owned())),
            }
        );
    }
    #[test]
    fn apply_on_apply_test() {
        let term = term!(((f x) (g y)));
        assert_eq!(
            term,
            App {
                fun: Box::new(App {
                    fun: Box::new(Var("f".to_owned())),
                    arg: Box::new(Var("x".to_owned())),
                }),
                arg: Box::new(App {
                    fun: Box::new(Var("g".to_owned())),
                    arg: Box::new(Var("y".to_owned())),
                }),
            }
        );
    }
}
