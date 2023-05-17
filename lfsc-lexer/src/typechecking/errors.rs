use lfsc_syntax::ast::AlphaTerm;

#[derive(Debug)]
pub enum TypecheckingErrors<T>
where T: Copy
{
    //Readback errors
    ValueUsedAsType,
    ReadBackMismatch,
    // ReadBackMismatch(RT<'ctx, T>, RT<'ctx, T>),
    WrongNumberOfArguments,

    SymbolAlreadyDefined(T),

    NotPi,
    NotZ,
    NotQ,
    LookupFailed(super::context::LookupErr),
    CannotInferLambda,
    CannotInferHole,

    UnexpectedSC,
    KindLevelDefinition,

    NotFullyApplied,
    DependentTypeNotAllowed,

    NotDatatype,
    ExpectedSort,
    // ExpectedSort(Value<T>),
    // we have these errors in Side-conditions.
    ExpectedNum,
    ExpectedSameNum,
    Mismatch(AlphaTerm<T>, AlphaTerm<T>),
    DivByZero,
    NaN,
    ReachedFail,
    NotSymbolic,
    NoMatch,
    Mark, // tried to get mark for non-symbolic variable.
    // For hacking holes
    ReadHole,

    ValAsType,
}

impl From<TypecheckingErrors<&str>> for String {
    fn from(value: TypecheckingErrors<&str>) -> Self {
        match value {
            TypecheckingErrors::Mismatch(a,b) => {
                format!("Error mismatching types:\n\t{}\n\t{}", a, b)
            }
            _ => format!("Error:\n\t{:?}", value)
        }
    }
}
