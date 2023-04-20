use super::values::{Neutral, Type, Value, RT, TResult, TypecheckingErrors};
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Cow;

#[derive(Debug)]
pub struct LookupErr {err: String}

pub type LResult<T, K> = TResult<T, K>;


#[cfg(feature = "conslist")]
pub type RLCTX<'a, T> = Rc<LocalContext<'a, T>>;
#[cfg(not(feature = "conslist"))]
pub type RLCTX<'a, T> = &'a LocalContext<'a, T>;
// pub type RLCTX<'a, T> = Cow<'a, LocalContext<'a, T>>;
pub type RGCTX<'a, T> = &'a GlobalContext<'a, T>;

#[derive(Debug)]
pub struct GlobalContext<'a, K: Clone> {
    pub kind: RT<'a, K>,
    keys: Vec<K>,
    values: Vec<TypeEntry<'a, K>>,
}

#[derive(Debug, Clone)]
#[cfg(feature = "conslist")]
pub enum LocalContext<'a, K: Clone> {
    Nil,
    Cons(TypeEntry<'a, K>, RLCTX<'a, K>),
}

#[derive(Debug, Clone)]
#[cfg(not(feature = "conslist"))]
pub struct LocalContext<'a, K: Clone> {
    values: Vec<TypeEntry<'a, K>>,
}

#[derive(Debug)]
pub enum Key<K> {
    Name(K),
    DBI(u32),
}

// pub fn init_with_str<'a>() -> Rc<GlobalContext<'a, &'a str>> {
pub fn init_with_str<'a>() -> GlobalContext<'a, &'a str> {
    let mut ctx = GlobalContext::new();
    ctx.define("type", Type::Box, Type::Star);
    ctx.define("mpz", Type::Star, Type::ZT);
    ctx.define("mpq", Type::Star, Type::QT);
    ctx
    // Rc::new(ctx)
}

fn from_entry_to_value<'a, K: Clone>(entry: &TypeEntry<'a, K>, key: Key<K>)
                                     -> RT<'a, K> {
       match entry {
         TypeEntry::Def { val, .. } => val.clone(),
         TypeEntry::IsA { ty, .. } =>
               Rc::new(Value::Neutral(ty.clone(),
                          Rc::new( match key {
                                     Key::Name(key) => Neutral::Var(key),
                                     Key::DBI(dbi) => Neutral::DBI(dbi)
                                   }))),
         }
}

fn from_entry_to_type<'a, K: Clone>(entry: &TypeEntry<'a, K>)
                                    -> RT<'a, K> {
       match entry {
         TypeEntry::Def { ty, .. } => ty.clone(),
         TypeEntry::IsA { ty, .. } => ty.clone(),
       }
}

pub fn get_mark<'a, K>(key: Key<K>,
                       n: u32,
                       lctx: RLCTX<'a, K>,
                       rctx: RGCTX<'a, K>) -> LResult<u32, K>
where K: std::fmt::Debug + Clone + PartialEq
    {
    let val = match key {
                Key::DBI(i) => lctx.get(i),
                Key::Name(name) => rctx.get(&name)
             }?;
     match val {
            TypeEntry::IsA { marks, .. } => Ok((*marks.borrow() >> n) & 1),
            TypeEntry::Def { .. } => return Err(TypecheckingErrors::Mark),
        }
    }
pub fn set_mark<'a, K>(key: Key<K>,
                       n: u32,
                       lctx: RLCTX<'a, K>,
                       rctx: RGCTX<'a, K>)
where K: PartialEq + Clone + std::fmt::Debug
{
    let val = match key {
                Key::DBI(i) => lctx.get(i),
                Key::Name(name) => rctx.get(&name)
             };
    if let Ok(TypeEntry::IsA { marks, .. }) = val {
       let mut marks = marks.borrow_mut();
       if (*marks >> n) & 1 == 1 {
           *marks |= 1 << n;
       } else {
           *marks &= !(1 << n);
       }
    }
}

impl<'a, K> GlobalContext<'a, K>
where K: PartialEq + Clone
{
    pub fn new() -> Self {
        Self {
            kind: Rc::new(Value::Box),
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: K, ty: Type<'a, K>) {
       self.keys.push(key);
       self.values.push(TypeEntry::IsA { ty: Rc::new(ty), marks: RefCell::new(0)})
    }

    pub fn define(&mut self, name: K, ty: Type<'a, K>, val: Value<'a, K>) {
     self.keys.push(name);
      self.values.push(TypeEntry::Def { ty: Rc::new(ty), val: Rc::new(val) })
    }

    fn get(&self, key: &K) -> LResult<&TypeEntry<'a, K>, K>
    where K: std::fmt::Debug
    {
          self.keys
              .iter()
              .rev()
              .zip(self.values.iter().rev())
              .find(|(&ref n, _)| n == key)
              .map(|(_, v)| v)
              .ok_or(lookup_err(Key::Name(key)))
    }


    pub fn get_value(&self, key: &K) -> LResult<RT<'a, K>, K>
    where K: std::fmt::Debug {
        self.get(key).map(|v| from_entry_to_value(v, Key::Name(key.clone())))
    }

    pub fn get_type(&self, key: &K) -> LResult<RT<'a, K>, K>
    where K: std::fmt::Debug {
        self.get(key).map(|v| from_entry_to_type(v))
    }
}

fn lookup_err<K, T>(key: Key<K>) -> TypecheckingErrors<T>
where K: std::fmt::Debug {
    TypecheckingErrors::LookupFailed(LookupErr { err: format!("{:?} not found", key) })
}

impl<'a, K> LocalContext<'a, K>
    where K: PartialEq + Clone + std::fmt::Debug
{
    #[cfg(feature = "conslist")]
    pub fn new() -> Self {
        Self::Nil
    }

    #[cfg(not(feature = "conslist"))]
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }

    #[cfg(feature = "conslist")]
    pub fn insert(ty: RT<'a, K>, ctx: RLCTX<'a, K>) -> RLCTX<'a, K> {
        Rc::new(LocalContext::Cons(
            TypeEntry::IsA { ty, marks: RefCell::new(0)}, ctx))
    }
    #[cfg(feature = "conslist")]
    pub fn get(&self, key: u32) -> LResult<&TypeEntry<'a, K>, K> {
        match self {
            LocalContext::Nil => Err(lookup_err(Key::<K>::DBI(key))),
            LocalContext::Cons(ty, ctx) => {
                if key == 0 {
                    Ok(ty)
                } else {
                    ctx.get(key - 1)
                }
            }
        }
    }

    #[cfg(feature = "conslist")]
    pub fn get_value(&self, key: u32) -> LResult<RT<'a, K>, K> {
        self.get(key).map(|v| from_entry_to_value(v, Key::<K>::DBI(key)))
    }

    #[cfg(feature = "conslist")]
    pub fn get_type(&self, key: u32) -> LResult<RT<'a, K>, K> {
        self.get(key).map(|v| from_entry_to_type(v))
    }
}

#[derive(Debug, Clone)]
enum TypeEntry<'a, Key> where Key: Clone {
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
