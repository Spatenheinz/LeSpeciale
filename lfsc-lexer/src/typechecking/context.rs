use lfsc_syntax::ast::Ident;

use super::errors::TypecheckingErrors;
use super::values::{Neutral, Type, Value, RT, TResult, ResRT};
use core::fmt;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct LookupErr {err: String}

pub type Rlctx<'term, T> = Rc<LocalContext<'term, T>>;

pub type Rgctx<'own, 'term, T> = &'own GlobalContext<'term, T>;

#[derive(Debug)]
pub struct GlobalContext<'term, K: Copy + PartialEq + std::fmt::Debug> {
    pub kind: RT<'term, K>,
    keys: Vec<K>,
    values: Vec<TypeEntry<'term, K>>,
}

pub enum LocalContext<'a, K: Copy + PartialEq + std::fmt::Debug> {
    Nil,
    Cons(TypeEntry<'a, K>, Rlctx<'a, K>),
}

impl<'term, T: Copy + fmt::Debug + PartialEq + std::fmt::Debug> fmt::Debug for LocalContext<'term, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LocalContext::Nil => write!(f, "Nil"),
            LocalContext::Cons(a, b) => write!(f, ":- {:?}\n{:?})", a, b),
        }
    }
}
pub fn init_with_str<'a>() -> GlobalContext<'a, &'a str> {
    let mut ctx = GlobalContext::new();
    ctx.define("type", Rc::new(Type::Box),  Rc::new(Type::Star));
    ctx.define("mpz",  Rc::new(Type::Star), Rc::new(Type::ZT));
    ctx.define("mpq",  Rc::new(Type::Star), Rc::new(Type::QT));
    ctx
}


fn from_entry_to_value<'a, K: Copy + PartialEq + std::fmt::Debug>(entry: &TypeEntry<'a, K>, key: Ident<K>)
                                     -> RT<'a, K> {
       match entry {
         TypeEntry::Def { val, .. } => val.clone(),
         TypeEntry::IsA { ty, .. } => {
             if let Value::Neutral(_, hol) = ty.borrow() {
                 if let Neutral::Hole(hol,_) = hol.borrow() {
                     // if let Some(inner) = &*hol.borrow() {
                     //     return inner.clone()
                     // }
                     return ty.clone();
                 }
             }
               Rc::new(Value::Neutral(ty.clone(),
                          Rc::new( match key {
                                     Ident::Symbol(key) => Neutral::Var(key),
                                     Ident::DBI(dbi) => Neutral::DBI(dbi)
                                   })))
            },
         TypeEntry::Val { val } => val.clone(),

         }
}

fn from_entry_to_type<'a, K: Copy + PartialEq + std::fmt::Debug>(entry: &TypeEntry<'a, K>)
                                    -> RT<'a, K> {
       match entry {
         TypeEntry::Def { ty, .. } => ty.clone(),
         TypeEntry::IsA { ty, .. } => ty.clone(),
         TypeEntry::Val { val } => panic!("from_entry_to_type: val"),
       }
}

// pub fn get_mark<'a, K>(key: Ident<K>,
//                        n: u32,
//                        lctx: Rlctx<'a, K>,
//                        gctx: Rgctx<'a, K>) -> LResult<u32, K>
// where K: std::fmt::Debug + Clone + PartialEq + std::fmt::Debug
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
// where K: PartialEq + std::fmt::Debug + Clone + std::fmt::Debug
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
where K: PartialEq + std::fmt::Debug + std::fmt::Debug + Copy
{
    pub fn new() -> Self {
        Self {
            kind: Rc::new(Value::Box),
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn contains(&self, key: &K) -> bool {
        self.keys.contains(key)
    }

    pub fn insert(&mut self, key: K, ty: RT<'a, K>) {
       self.keys.push(key);
       self.values.push(TypeEntry::IsA { ty, marks: RefCell::new(0)})
    }

    pub fn define(&mut self, name: K, ty: RT<'a, K>, val: RT<'a, K>) {
      self.keys.push(name);
      self.values.push(TypeEntry::Def { ty, val })
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
            .iter()
            .rev()
            .zip(self.values.iter().rev())
            .find(|(&n, _)| n == *key)
            .map(|(_, v)| from_entry_to_value(v, Ident::Symbol(*key)))
            .ok_or(lookup_err(Ident::Symbol(key)))
    }

    pub fn get_type(&self, key: &K) -> ResRT<'a, K>
    where K: std::fmt::Debug {
        self.keys
            .iter()
            .rev()
            .zip(self.values.iter().rev())
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
            TypeEntry::Val { val : ty }, ctx))
            // TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }

    pub fn decl(ty: RT<'a, K>, ctx: Rlctx<'a, K>) -> Rlctx<'a, K> {
        Rc::new(LocalContext::Cons(
            // TypeEntry::Val { val : ty }, ctx))
            TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }
    pub fn define(ty: RT<'a, K>, val:RT<'a,K>, ctx: Rlctx<'a, K>) -> Rlctx<'a, K> {
        Rc::new(LocalContext::Cons(
            TypeEntry::Def { ty, val }, ctx))
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
pub enum TypeEntry<'a, Key: Copy + PartialEq + std::fmt::Debug>
// where Key: Clone
{
    // Dec { ty: RT<'a, Key> },
    Def { ty: RT<'a, Key>, val: RT<'a, Key> },
    // the val of IsA is the neutral term Neutral t
    // Symbolics can only ever be a IsA.
    IsA { ty: RT<'a, Key>, marks: RefCell<u32> },
    Val { val: RT<'a, Key> },
}

#[cfg(test)]
mod tests {
    use super::*;
}
