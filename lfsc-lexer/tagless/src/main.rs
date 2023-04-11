use std::io;
use core::fmt::Display;
// use tagless::tagless;

macro_rules! lam {
    ($x:ident, $e:expr) => {
        <Eval as Term>::lam(|$x| $x)($e)
    };
}


// Typeclass that defines the constructors for a
// the type of int and the type of arrow.
// Class TSym repr where ~ trait TSYM { type Repr<T> }
trait TSYM {
    type Repr<T>;
    fn tint() -> Self::Repr<i32>;
    fn tarr<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(a: Self::Repr<A>, b: Self::Repr<B>) -> Self::Repr<Fun<A,B>>
        where for<'a> F: 'a;
}

// // newtype that can be stringified
// struct ShowT;

// impl TSYM for ShowT {
//     type Repr<T> = String;
//     fn tint() -> Self::Repr<i32> {
//        "Int".to_string()
//     }
//     fn tarr<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(a: Self::Repr<A>, b: Self::Repr<B>) -> Self::Repr<F>
//         where for<'a> F: 'a, A: Display, B: Display {
//         format!("{} -> {}", a, b)
//     }
// }
// //Quantification on types
// TSYM is a trait object
// struct TQ<T: TSYM>(Box<dyn Fn(()) -> T);
struct TQ<T: TSYM,U>(Fun<(), T::Repr<U>>);

impl<T: TSYM> = U>, U> TSYM for TQ<T,U> {
    type Repr<P> = TQ<T, U>;
    fn tint() -> Self::Repr<i32> {
        TQ(Box::new(|_| T::tint()))
    }
    fn tarr<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(a: Self::Repr<A>, b: Self::Repr<B>) -> Self::Repr<F>
        where for<'a> F: 'a {
        TQ(Box::new(|_| T::tarr(a.0(()), b.0(()))))
    }
}
// impl<T: TSYM + Display> Display for TQ<T> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

// // newtype TQ t = TQ{unTQ :: forall trepr. TSYM trepr => trepr t}
// impl<T: TSYM + Display> TSYM for TQ<T> {
//     type Repr<P> = TQ<T>;
//     fn tint() -> Self::Repr<i32> {
//         Self::tint()
//     }
//     fn tarr<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(a: Self::Repr<A>, b: Self::Repr<B>) -> Self::Repr<F>
//         where for<'a> F: 'a, A: Display, B: Display {
//         Self::tarr::<TQ<T>,TQ<T>,Fun<TQ<T>, TQ<T>>>(a, b)
//     }
// }

struct R<H,A>(dyn Fn(H) -> A);

type Fun<A, B> = Box<dyn Fn(A) -> B>;

trait Type {}
impl Type for () {}
// trait Universe {}
// #[derive(Debug)]
// struct Type;
// impl Universe for Type {}

trait Term {
    type Repr<T>;

    fn type_() -> Self::Repr<()>;
    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A, B>>
    where
        for<'a> F: 'a;
    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B>;
    fn pi<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(x: Self::Repr<A>, f: Self::Repr<F>) -> Self::Repr<B>
    where
        for<'a> F: 'a, A: Type, B: Type;
}

#[derive(Debug)]
struct Eval;

impl Term for Eval {
    type Repr<T> = T;
    fn type_() -> Self::Repr<()> {
    }
    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A,B>>
    where
        for<'a> F: 'a,
    {
        Box::new(f)
    }

    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B> {
        f(arg)
    }
    fn pi<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>
        (a: Self::Repr<A>, f: Self::Repr<F>) -> Self::Repr<B>
    where
        for<'a> F: 'a, A: Type, B: Type {
        f(a)
    }
}

// struct Check;
// impl Term for Check {
//     type Repr<T> = T;
//     fn type_() -> Self::Repr<()> {
//         ()
//     }
// }
// struct Synth;
// impl Term for Synth {
//     type Repr<T> = T;
//     fn type_() -> Self::Repr<()> {
//     }
//     fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A,B>>
//     where
//         for<'a> F: 'a,
//     {
//         todo!()
//     }
//     fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B> {

//     }
//     fn pi<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(_x: Self::Repr<A>, f: Self::Repr<F>) -> Self::Repr<B>
//     where
//         for<'a> F: 'a, A: Type, B: Type {
//         f::<Synth>()
//     }
// }

// fn example<T: Term>() {
//     expr = Term::pi(Term::type_(), Term::lam(|x| x));
// }

fn main() {
    let mut input_text = String::new();
    io::stdin()
        .read_line(&mut input_text)
        .expect("failed to read from stdin");

    let trimmed = input_text.trim();
    let i = trimmed.parse::<i32>().unwrap();
    // we only want to typecheck if
    // let expr  = <Eval as ExprSym>::add(&<Eval as ExprSym>::int(i), &<Eval as ExprSym>::int(2));
    // let result = expr::<Eval>();
// let expr = <ShowT<()> as TSYM>::tint();
    // println!("{:?}", expr);
}
