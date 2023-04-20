
use super::values::RT;

pub fn occurs<'a, T>(idx: u32, ty: RT<'a, T>) -> bool {
    match ty {
        Kind | ZT | QT | Z(_) | Q(_) => false,
        Lam(closure) => {
            occurs(idx+1, closure.ty)
        },
        Pi(dom, closure) => {
            occurs(idx, dom) || occurs(idx+1, closure.ty)
        },
        Neutral(neutral) => {
            occurs_neutral(idx, neutral)
        },
    }
}

fn occurs_neutral<'a,T>(idx: u32, neutral: Neutral<'a, T>) -> bool {
    match neutral {
        DBI(i) => i == idx,
        App(ty, neutral) => occurs(id, ty) || free_neutral(idx, neutral),
        Hole(x) => x.is_some_and(|x| occurs(idx, x)),
        _ => false,
    }
}
