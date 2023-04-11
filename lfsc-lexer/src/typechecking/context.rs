use super::values::{Neutral, Type, Value, RT, TResult, TypecheckingErrors};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct LookupErr {err: String}

pub type LResult<T> = TResult<T>;

pub type RLCTX<'a, T> = Rc<RefCell<LocalContext<'a, T>>>;

#[derive(Debug)]
pub struct GlobalContext<'a, K> {
    keys: Vec<K>,
    values: Vec<TypeEntry<'a, K>>,
}

#[derive(Debug)]
pub struct LocalContext<'a, K> {
    parent: Rc<GlobalContext<'a, K>>,
    values: Vec<TypeEntry<'a, K>>,
}

pub fn init_with_str<'a>() -> Rc<GlobalContext<'a, &'a str>> {
    let mut ctx = GlobalContext::new();
    ctx.insert("type", Type::Kind);
    ctx.insert("mpz", Type::Kind);
    ctx.insert("mpq", Type::Kind);
    Rc::new(ctx)
}

fn from_entry_to_value<'a, K: Clone>(entry: &TypeEntry<'a, K>, key: Key<K>)
                                     -> RT<'a, K> {
       match entry {
         TypeEntry::Def { val, .. } => val.clone(),
         TypeEntry::IsA { ty } =>
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
         TypeEntry::IsA { ty } => ty.clone(),
       }
}


// #[derive(Debug)]
pub trait Context<'a, K> {
    fn get(&self, key: &Key<K>) -> LResult<&TypeEntry<'a, K>>;
    fn get_value(&self, key: Key<K>) -> LResult<RT<'a, K>>;
    fn get_type(&self, key: Key<K>) -> LResult<RT<'a, K>>;
}

impl<'a, K> GlobalContext<'a, K>
where
    K: PartialEq + Clone
{
    pub fn new() -> Self {
        Self {
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    fn insert(&mut self, key: K, ty: Type<'a, K>) {
       self.keys.push(key);
       self.values.push(TypeEntry::IsA { ty: Rc::new(ty) })
    }

    pub fn define(&mut self, name: K, ty: Type<'a, K>, val: Value<'a, K>) {
     self.keys.push(name);
      self.values.push(TypeEntry::Def { ty: Rc::new(ty), val: Rc::new(val) })
    }
}

fn lookup_err<K>(key: &Key<K>) -> TypecheckingErrors
where K: std::fmt::Debug {
    TypecheckingErrors::LookupFailed(LookupErr { err: format!("{:?} not found", key) })
}

impl<'a, K> Context<'a, K> for GlobalContext<'a, K>
    where K: PartialEq + Clone + std::fmt::Debug {

    fn get(&self, key: &Key<K>) -> LResult<&TypeEntry<'a, K>> {
        match key {
            Key::Name(x) => {
                self.keys
                    .iter()
                    .rev()
                    .zip(self.values.iter().rev())
                    .find(|(&ref n, _)| n == x)
                    .map(|(_, v)| v)
                    .ok_or(lookup_err(key))
            }
            Key::DBI(dbi) => Err(lookup_err(key)),
        }
    }

    fn get_value(&self, key: Key<K>) -> LResult<RT<'a, K>> {
        self.get(&key).map(|v| from_entry_to_value(v, key))
    }

    fn get_type(&self, key: Key<K>) -> LResult<RT<'a, K>> {
        self.get(&key).map(|v| from_entry_to_type(v))
    }
}

#[derive(Debug, Clone)]
pub enum Key<K> {
    Name(K),
    DBI(u32),
}
impl<'a, K> LocalContext<'a, K>
    where K: PartialEq + Clone + std::fmt::Debug
{
    pub fn new(parent: Rc<GlobalContext<'a, K>>) -> Self {
        Self {
            parent,
            values: Vec::new(),
        }
    }

    pub fn insert(&mut self, ty: RT<'a, K>) {
        self.values.push(TypeEntry::IsA { ty })
    }
}

impl<'a, K> Context<'a, K> for LocalContext<'a, K>
    where K: PartialEq + Clone + std::fmt::Debug
{

    fn get(&self, key: &Key<K>) -> LResult<&TypeEntry<'a, K>> {
        match key {
            Key::Name(name) => self.parent.get(key),
            Key::DBI(dbi) => self
                .values
                .get(*dbi as usize)
                .ok_or(lookup_err(key)),
        }
    }
    fn get_value(&self, key: Key<K>) -> LResult<RT<'a, K>> {
        self.get(&key).map(|v| from_entry_to_value(v, key))
    }

    fn get_type(&self, key: Key<K>) -> LResult<RT<'a, K>> {
        self.get(&key).map(|v| from_entry_to_type(v))
    }
}

#[derive(Debug, Clone)]
enum TypeEntry<'a, Key> {
    Def { ty: RT<'a, Key>, val: RT<'a, Key> },
    // the val of IsA is the neutral term Neutral t
    IsA { ty: RT<'a, Key> },
}

#[cfg(test)]
mod tests {
    use super::*;
}
