pub mod ast;
pub mod sexp;

#[macro_export]
macro_rules! binder {
    (let $v:ident, $e:expr, $body:expr) => {
        $crate::ast::Term::App(
            Box::new(binder!(lam, $v, $body)),
            vec![$e],
        )
        // $crate::ast::Term::App(
        //     Box::new(binder!(lam, $v, $body)),
        //     Box::new($e),
        // )
    };
    (pi, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Pi,
            var: $v,
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
    (lam, $v:ident, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Lam,
            var: $v,
            ty: None,
            body: Box::new($body),
        }
    };
    (lam, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::Lam,
            var: $v,
            ty: Some(Box::new($ty)),
            body: Box::new($body),
        }
    };
    (biglam, $v:ident : $ty:expr, $body:expr) => {
        $crate::ast::Term::Binder {
            kind: $crate::ast::BinderKind::BigLam,
            var: $v,
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
        stringify!($name)
        // stringify!($name).to_owned()
    };
}

#[macro_export]
macro_rules! term {
    ($v:ident) => { $crate::ast::Term::Ident(Symbol(var!($v))) };
    (($($inner:tt)+)) => { term_!($($inner)+) };
}

#[macro_export]
macro_rules! term_ {
    ($v:ident) => {$crate::ast::Term::Ident(Symbol(var!($v)))};
    (λ $v:ident $b:tt) => {
        binder!(lam, $v, term!($b))
    };
    // (let $e:tt $b:tt) => {
    //     $crate::ast::Term::App(
    //         binder!(lam, term!($b)).into(),
    //         term!($e).into(),
    //     )
    // };
    (let $v:ident $e:tt $b:tt) => {
        $crate::ast::Term::App(
            binder!(lam, $v, term!($b)).into(),
            term!($e).into(),
        )
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
        $crate::ast::Term::App (
            $f.into(),
            $x.into(),
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::ast::Term::*;

    #[test]
    fn var_test() {
        let term = term!(x);
        assert_eq!(term, Ident(Symbol("x")));
    }
    #[test]
    fn var_keyword_test() {
        let term = term!(let);
        assert_eq!(term, Ident(Symbol("let")));
    }
    #[test]
    fn apply_test() {
        let term = term!((f x));
        assert_eq!(
            term,
            App (
                Var("f".to_owned()).into(),
                Box::new(Var("x".to_owned())),
            )
        );
    }
    #[test]
    fn nested_apply_test() {
        let term = term!((f g x));
        assert_eq!(
            term,
            App (
                Box::new(Var("f".to_owned())),
                Box::new(App (
                    Box::new(Var("g".to_owned())),
                    Box::new(Var("x".to_owned())),
                )),
            )
        );
    }
    #[test]
    fn simple_let_test() {
        let term = term!((let x y x));
        assert_eq!(
            term,
            App (
                binder!(lam, x, term!(x)).into(),
                Box::new(Var("y".to_owned()))
            )
        );
    }
    #[test]
    fn nameless_let_test() {
        let term = term!((let y x));
        assert_eq!(
            term,
            App (
                binder!(lam, term!(x)).into(),
                Box::new(Var("y".to_owned()))
            )
        );
    }
    #[test]
    fn let_test() {
        let term = term!((let x (f y) x));
        assert_eq!(
            term,
            App (
                binder!(lam, x, term!(x)).into(),
                App (
                    Box::new(Var("f".to_owned())),
                    Box::new(Var("y".to_owned()))
                ).into()
            )
        );
    }
    #[test]
    fn apply_is_let_test() {
        let term = term!(((let x y x) z));
        assert_eq!(
            term,
            App (
                App (
                    binder!(lam, x, term!(x)).into(),
                    term!(y).into()).into(),
                Box::new(Var("z".to_owned())),
            )
        );
    }
    #[test]
    fn apply_on_apply_test() {
        let term = term!(((f x) (g y)));
        assert_eq!(
            term,
            App (
                Box::new(App (
                    Box::new(Var("f".to_owned())),
                    Box::new(Var("x".to_owned())),
                )),
                Box::new(App (
                    Box::new(Var("g".to_owned())),
                    Box::new(Var("y".to_owned())),
                )),
            )
        );
    }
}
