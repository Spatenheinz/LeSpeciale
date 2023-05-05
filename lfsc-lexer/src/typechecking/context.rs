use lfsc_syntax::ast::Ident;

use super::values::{Neutral, Type, Value, RT, TypecheckingErrors, TResult, ResRT};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct LookupErr {err: String}

pub type Rlctx<'a, T> = Rc<LocalContext<'a, T>>;

// pub type RGCTX<'a, T> = Rc<GlobalContext<'a, T>>;
pub type Rgctx<'a, T> = Rc<GlobalContext<'a, T>>;

#[derive(Debug)]
pub struct GlobalContext<'a, K: Copy> {
    pub kind: RT<'a, K>,
    keys: RefCell<Vec<K>>,
    values: RefCell<Vec<TypeEntry<'a, K>>>,
}

#[derive(Debug)]
#[cfg(feature = "conslist")]
pub enum LocalContext<'a, K: Copy> {
    Nil,
    Cons(TypeEntry<'a, K>, Rlctx<'a, K>),
}

// pub fn init_with_str<'a>() -> Rc<GlobalContext<'a, &'a str>> {
pub fn init_with_str<'a>() -> GlobalContext<'a, &'a str> {
    let ctx = GlobalContext::new();
    ctx.define("type", Rc::new(Type::Box),  Rc::new(Type::Star));
    ctx.define("mpz",  Rc::new(Type::Star), Rc::new(Type::ZT));
    ctx.define("mpq",  Rc::new(Type::Star), Rc::new(Type::QT));
    ctx
    // Rc::new(ctx)
}


fn from_entry_to_value<'a, K: Copy>(entry: &TypeEntry<'a, K>, key: Ident<K>)
                                     -> RT<'a, K> {
       match entry {
         TypeEntry::Def { val, .. } => val.clone(),
         TypeEntry::IsA { ty, .. } =>
               Rc::new(Value::Neutral(ty.clone(),
                          Rc::new( match key {
                                     Ident::Symbol(key) => Neutral::Var(key),
                                     Ident::DBI(dbi) => Neutral::DBI(dbi)
                                   }))),
         }
}

fn from_entry_to_type<'a, K: Copy>(entry: &TypeEntry<'a, K>)
                                    -> RT<'a, K> {
       match entry {
         TypeEntry::Def { ty, .. } => ty.clone(),
         TypeEntry::IsA { ty, .. } => ty.clone(),
       }
}

// pub fn get_mark<'a, K>(key: Ident<K>,
//                        n: u32,
//                        lctx: Rlctx<'a, K>,
//                        gctx: Rgctx<'a, K>) -> LResult<u32, K>
// where K: std::fmt::Debug + Clone + PartialEq
//     {
//     match key {
//        Ident::DBI(i) => lctx.get(i),
//        Ident::Symbol(name) => {
//            gctx.get(&name)
//        }
//        }?;
//      match val {
//             TypeEntry::IsA { marks, .. } => Ok((*marks.borrow() >> n) & 1),
//             TypeEntry::Def { .. } => return Err(TypecheckingErrors::Mark),
//         }
//     }
// pub fn set_mark<'a, K>(key: Ident<K>,
//                        n: u32,
//                        lctx: Rlctx<'a, K>,
//                        gctx: Rgctx<'a, K>)
// where K: PartialEq + Clone + std::fmt::Debug
// {
//     let val = match key {
//                 Ident::DBI(i) => lctx.get(i),
//                 Ident::Symbol(name) => gctx.get(&name)
//              };
//     if let Ok(TypeEntry::IsA { marks, .. }) = val {
//        let mut marks = marks.borrow_mut();
//        if (*marks >> n) & 1 == 1 {
//            *marks |= 1 << n;
//        } else {
//            *marks &= !(1 << n);
//        }
//     }
// }

impl<'a, K> GlobalContext<'a, K>
where K: PartialEq + std::fmt::Debug + Copy
{
    pub fn new() -> Self {
        Self {
            kind: Rc::new(Value::Box),
            keys: RefCell::new(Vec::new()),
            values: RefCell::new(Vec::new()),
        }
    }

    pub fn contains(&self, key: &K) -> bool {
        self.keys.borrow().contains(key)
    }

    pub fn insert(&self, key: K, ty: RT<'a, K>) {
       self.keys.borrow_mut().push(key);
       self.values.borrow_mut().push(TypeEntry::IsA { ty, marks: RefCell::new(0)})
    }

    pub fn define(&self, name: K, ty: RT<'a, K>, val: RT<'a, K>) {
     self.keys.borrow_mut().push(name);
      self.values.borrow_mut().push(TypeEntry::Def { ty, val })
    }

    // fn get(&self, key: &K) -> LResult<&TypeEntry<'a, K>, K>
    // where K: std::fmt::Debug
    // {
    //       self.keys
    //           .iter()
    //           .rev()
    //           .zip(self.values.iter().rev())
    //           .find(|(&ref n, _)| n == key)
    //           .map(|(_, v)| v)
    //           .ok_or(lookup_err(Ident::Symbol(key)))
    // }


    pub fn get_value(&self, key: &K) -> ResRT<'a, K>
    where K: std::fmt::Debug {
        self.keys
            .borrow()
            .iter()
            .rev()
            .zip(self.values.borrow().iter().rev())
            .find(|(&n, _)| n == *key)
            .map(|(_, v)| from_entry_to_value(v, Ident::Symbol(*key)))
            .ok_or(lookup_err(Ident::Symbol(key)))
    }

    pub fn get_type(&self, key: &K) -> ResRT<'a, K>
    where K: std::fmt::Debug {
        self.keys
            .borrow()
            .iter()
            .rev()
            .zip(self.values.borrow().iter().rev())
            .find(|(&n, _)| n == *key)
            .map(|(_, v)| from_entry_to_type(v))
            .ok_or(lookup_err(Ident::Symbol(key)))
    }
}

fn lookup_err<K, T>(key: Ident<K>) -> TypecheckingErrors<T>
where K: std::fmt::Debug,
      T: Copy
{
    TypecheckingErrors::LookupFailed(LookupErr { err: format!("{:?} not found", key) })
}

impl<'a, K> LocalContext<'a, K>
where K: PartialEq + std::fmt::Debug + Copy
{
    pub fn new() -> Self {
        Self::Nil
    }

    pub fn insert(ty: RT<'a, K>, ctx: Rlctx<'a, K>) -> Rlctx<'a, K> {
        Rc::new(LocalContext::Cons(
            TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }
    pub fn get(&self, key: u32) -> TResult<&TypeEntry<'a, K>, K> {
        match self {
            LocalContext::Nil => Err(lookup_err(Ident::<K>::DBI(key))),
            LocalContext::Cons(ty, ctx) => {
                if key == 0 {
                    Ok(ty)
                } else {
                    ctx.get(key - 1)
                }
            }
        }
    }

    pub fn get_value(&self, key: u32) -> ResRT<'a, K> {
        self.get(key).map(|v| from_entry_to_value(v, Ident::<K>::DBI(key)))
    }

    pub fn get_type(&self, key: u32) -> ResRT<'a, K> {
        self.get(key).map(|v| from_entry_to_type(v))
    }
}

#[derive(Debug)]
pub enum TypeEntry<'a, Key: Copy>
// where Key: Clone
{
    // Dec { ty: RT<'a, Key> },
    Def { ty: RT<'a, Key>, val: RT<'a, Key> },
    // the val of IsA is the neutral term Neutral t
    // Symbolics can only ever be a IsA.
    IsA { ty: RT<'a, Key>, marks: RefCell<u32> },
}

#[cfg(test)]
mod tests {
    use super::*;
}
