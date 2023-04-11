// phantoms for typesafe debruijn indices
trait Nat {}
struct Zero;
impl Nat for Zero {}
struct Succ<N: Nat>{
    _pred: core::marker::PhantomData<N>
}

impl <N: Nat> Nat for Succ<N> {}

type Fun<A, B> = Box<dyn Fn(A) -> B>;

trait Universe {}
struct Type;
impl Universe for Type {}


trait Term {
    type Repr<T>;

    fn type_() -> Self::Repr<Type>;
    fn lam<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(f: F) -> Self::Repr<Fun<A, B>>
    where
        for<'a> F: 'a;
    fn app<F: Fn(A) -> B, A, B>(f: Self::Repr<F>, arg: Self::Repr<A>) -> Self::Repr<B>;
    fn pi<A, B, F: Fn(Self::Repr<A>) -> Self::Repr<B>>(x: Self::Repr<A>, f: Self::Repr<F>) -> Self::Repr<B>
    where
        for<'a> F: 'a, A: Universe, B: Universe;
}


// enum Value {
//     // Universe,
//     // Type,
//     Pi(Box<Value>, Box<Value>),
//     // Lambda(Box<Value>, Box<Value>),
// }

struct Eval;
impl Term for Eval {
    type Repr<T> = T;
    fn type_() -> Self::Repr<Type> {
        Type
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
        (a: Self::Repr<A>, f: Self::Repr<F>) -> Self::Repr<B> {
        f(a)
    }
}

// struct ReadBack;
// impl Term for ReadBack {
//     type Repr<T> = Value;
// }


// struct Synth;
// impl Term for Synth {
//     type Repr<T> = Value;
//     fn type_() -> Self::Repr<Value> {
//         Value::Universe
//     }
// }

// struct Check;
// impl Term for Check {
//     type Repr<T> = ();
//     // fn type_() -> Self:: {
//     //     Value::Universe
//     // }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         let x = Eval::type_();
//         let y = Synth::type_();
//         let z = Check::type_();
//     }
// }
