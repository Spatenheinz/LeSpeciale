#[cfg(test)]
mod tests {
    use crate::ast::Term::*;

    fn var_test() {
        let term = term!(x);
        assert_eq!(term, Var("x".to_owned()));
    }
    fn var_keyword_test() {
        let term = term!(let);
        assert_eq!(term, Var("let".to_owned()));
    }

    fn apply_test() {
        let term = term!(f x);
        assert_eq!(term, App {
            fun: Box::new(Var("f".to_owned())),
            arg: Box::new(Var("x".to_owned())),
        });
    }
    fn nested_apply_test() {
        let term = term!(f g x);
        assert_eq!(term, App {
            fun: Box::new(Var("f".to_owned())),
            arg: Box::new(App {
                fun: Box::new(Var("g".to_owned())),
                arg: Box::new(Var("x".to_owned())),
            }),
        });
    }
    // fn let_test() {
    //     let term = term!(let x = (f y) in x);
    //     assert_eq!(term, Let {
    //         var: "x".to_owned(),
    //         val: Box::new(App {
    //             fun: Box::new(Var("f".to_owned())),
    //             arg: Box::new(Var("y".to_owned())),
    //         }),
    //         body: Box::new(Var("x".to_owned())),
    //     });
    // }
    fn apply_is_let_test() {
        let term = term!((let x y x) z);
        assert_eq!(term, App {
            fun: Box::new(Let {
                var: "x".to_owned(),
                val: Box::new(Var("y".to_owned())),
                body: Box::new(Var("x".to_owned())),
            }),
            arg: Box::new(Var("z".to_owned())),
        });
    }
}
