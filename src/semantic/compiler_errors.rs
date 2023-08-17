use crate::types::diagnostics::ReportToken;
//@TODO unify with type errors
#[derive(Debug)]
//Compilation errors can only be returned after providing proof that the error has been reported.
pub enum CompilerError {
    TypeInferenceError(ReportToken),
    TypeCheckError(ReportToken),
}
