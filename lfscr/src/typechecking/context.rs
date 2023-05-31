use lfsc_syntax::ast::{Ident, BuiltIn};

use super::errors::TypecheckingErrors;
use super::values::{Neutral, Type, Value, RT, TResult, ResRT};
use core::fmt;
use std::borrow::Borrow;
use std::collections::{HashMap, BTreeMap};
use std::rc::Rc;
use std::cell::RefCell;

use std::hash::Hash;


#[derive(Debug)]
pub struct LookupErr {err: String}

pub type Rlctx<'term, T> = Rc<LocalContext<'term, T>>;

pub type Rgctx<'own, 'term, T> = &'own GlobalContext<'term, T>;

#[derive(Debug)]
pub struct GlobalContext<'term, K: BuiltIn> {
    pub kind: RT<'term, K>,
    kvs: HashMap<K, TypeEntry<'term, K>>
}

pub enum LocalContext<'a, K: BuiltIn> {
    Nil,
    Cons(TypeEntry<'a, K>, Rlctx<'a, K>),
}

impl<'term, T: BuiltIn> fmt::Debug for LocalContext<'term, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LocalContext::Nil => write!(f, "Nil"),
            LocalContext::Cons(a, b) => write!(f, ":- {:?}\n{:?})", a, b),
        }
    }
}
pub fn init_with_str<'term>() -> GlobalContext<'term, &'term str> {
    let mut ctx = GlobalContext::new();
    ctx.define("type", Rc::new(Type::Box),  Rc::new(Type::Star));
    ctx.define("mpz",  Rc::new(Type::Star), Rc::new(Type::ZT));
    ctx.define("mpq",  Rc::new(Type::Star), Rc::new(Type::QT));
    ctx
}


fn from_entry_to_value<'term, K: BuiltIn>(entry: &TypeEntry<'term, K>, key: Ident<K>)
                                     -> RT<'term, K> {
       match entry {
         TypeEntry::Def { val, .. } => val.clone(),
         TypeEntry::IsA { ty, .. } => {
             if let Value::Neutral(_, hol) = ty.borrow() {
                 if let Neutral::Hole(_) = hol.borrow() {
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

fn from_entry_to_type<'term, K: BuiltIn>(entry: &TypeEntry<'term, K>)
                                    -> RT<'term, K> {
       match entry {
         TypeEntry::Def { ty, .. } => ty.clone(),
         TypeEntry::IsA { ty, .. } => ty.clone(),
         TypeEntry::Val { val } => panic!("from_entry_to_type: val"),
       }
}

impl<'term, K> GlobalContext<'term, K>
where K: BuiltIn
{
    pub fn new() -> Self {
        Self {
            kind: Rc::new(Value::Box),
            kvs: HashMap::new(),
            // keys: Vec::new(),
            // values: Vec::new(),
        }
    }

    pub fn contains(&self, key: &K) -> bool {
        self.kvs.contains_key(key)
        // self.keys.contains(key)
    }

    pub fn insert(&mut self, key: K, ty: RT<'term, K>) {
        self.kvs.insert(key, TypeEntry::IsA { ty, marks: RefCell::new(0)});
       // self.keys.push(key);
       // self.values.push(TypeEntry::IsA { ty, marks: RefCell::new(0)})
    }

    pub fn define(&mut self, key: K, ty: RT<'term, K>, val: RT<'term, K>) {
        self.kvs.insert(key, TypeEntry::Def { ty, val });
      // self.keys.push(name);
      // self.values.push(TypeEntry::Def { ty, val })
    }

    // fn get(&self, key: &K) -> LResult<&TypeEntry<'term, K>, K>
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


    pub fn get_value(&self, key: &K) -> ResRT<'term, K>
    where K: std::fmt::Debug {
        self.kvs.get(key).map(|v| from_entry_to_value(v, Ident::Symbol(*key)))
            .ok_or(lookup_err(Ident::Symbol(key)))
        // self.keys
        //     .iter()
        //     .rev()
        //     .zip(self.values.iter().rev())
        //     .find(|(&n, _)| n == *key)
        //     .map(|(_, v)| from_entry_to_value(v, Ident::Symbol(*key)))
        //     .ok_or(lookup_err(Ident::Symbol(key)))
    }

    pub fn get_type(&self, key: &K) -> ResRT<'term, K>
    where K: std::fmt::Debug {
        self.kvs.get(key).map(|v| from_entry_to_type(v))
            .ok_or(lookup_err(Ident::Symbol(key)))
        // self.keys
        //     .iter()
        //     .rev()
        //     .zip(self.values.iter().rev())
        //     .find(|(&n, _)| n == *key)
        //     .map(|(_, v)| from_entry_to_type(v))
        //     .ok_or(lookup_err(Ident::Symbol(key)))
    }
}

fn lookup_err<K, T>(key: Ident<K>) -> TypecheckingErrors<T>
where K: std::fmt::Debug,
      T: Copy
{
    TypecheckingErrors::LookupFailed(LookupErr { err: format!("{:?} not found", key) })
}

impl<'term, K> LocalContext<'term, K>
where K: BuiltIn
{
    pub fn new() -> Self {
        Self::Nil
    }

    pub fn insert(ty: RT<'term, K>, ctx: Rlctx<'term, K>) -> Rlctx<'term, K> {
        Rc::new(LocalContext::Cons(
            TypeEntry::Val { val : ty }, ctx))
            // TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }

    pub fn decl(ty: RT<'term, K>, ctx: Rlctx<'term, K>) -> Rlctx<'term, K> {
        Rc::new(LocalContext::Cons(
            // TypeEntry::Val { val : ty }, ctx))
            TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }
    pub fn define(ty: RT<'term, K>, val:RT<'term,K>, ctx: Rlctx<'term, K>) -> Rlctx<'term, K> {
        Rc::new(LocalContext::Cons(
            TypeEntry::Def { ty, val }, ctx))
    }

    pub fn get(&self, key: u32) -> TResult<&TypeEntry<'term, K>, K> {
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

    pub fn get_value(&self, key: u32) -> ResRT<'term, K> {
        self.get(key).map(|v| from_entry_to_value(v, Ident::<K>::DBI(key)))
    }

    pub fn get_type(&self, key: u32) -> ResRT<'term, K> {
        self.get(key).map(|v| from_entry_to_type(v))
    }
}

#[derive(Debug)]
pub enum TypeEntry<'term, Key: BuiltIn>
{
    Def { ty: RT<'term, Key>, val: RT<'term, Key> },
    // the val of IsA is the neutral term Neutral t
    // Symbolics can only ever be term IsA.
    IsA { ty: RT<'term, Key>, marks: RefCell<u32> },
    Val { val: RT<'term, Key> },
}

#[cfg(test)]
mod tests {
    use super::*;
}
