use crate::ast::lexer::TokenSpanIndex;
use crate::ast::parser::Spanned;
use crate::interner::InternedString;
use crate::semantic::compiler_errors::CompilerError;
use crate::semantic::context::FileTableEntry;

use crate::semantic::mir::{MIRExpr, MIRExprLValue};
use crate::semantic::mir_printer::MIRPrinter;
use crate::{
    ast::lexer::Operator,
    semantic::{
        hir::{HIRExprMetadata, HIRType, HIRTypeDisplayer},
        hir_type_resolution::RootElementType,
        type_checker::FunctionName,
    },
};
use std::fmt::Display;

use super::type_constructor_db::{TypeConstructParams, TypeConstructorId};
use super::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

pub trait ErrorReporter {
    fn report<T: CompilerErrorData>(
        &self,
        error: T,
        span: &impl Spanned,
        compiler_code_location: &'static str,
    ) -> CompilerErrorContext<T>;
}

#[macro_export]
macro_rules! report {
    ($self:ident, $span:expr, $error:expr) => {
        $self.report($error, $span, loc!())
    };
}

pub struct CompilerErrorContext<T>
where
    T: CompilerErrorData,
{
    pub error: T,
    pub on_element: RootElementType,
    pub location: TokenSpanIndex,
    pub compiler_code_location: &'static str,
}

pub trait CompilerErrorData {}

pub trait ContextualizedCompilerError<T: CompilerErrorData> {
    fn at(
        self,
        on_element: RootElementType,
        location: TokenSpanIndex,
        compiler_code_location: &'static str,
    ) -> CompilerErrorContext<T>;

    fn at_spanned(
        self,
        on_element: RootElementType,
        span: &impl Spanned,
        compiler_code_location: &'static str,
    ) -> CompilerErrorContext<T>;
}

impl<T: Sized + CompilerErrorData> ContextualizedCompilerError<T> for T {
    fn at(
        self,
        on_element: RootElementType,
        location: TokenSpanIndex,
        compiler_code_location: &'static str,
    ) -> CompilerErrorContext<T> {
        CompilerErrorContext {
            error: self,
            on_element,
            location,
            compiler_code_location,
        }
    }

    fn at_spanned(
        self,
        on_element: RootElementType,
        span: &impl Spanned,
        compiler_code_location: &'static str,
    ) -> CompilerErrorContext<T> {
        let span = span.get_span();
        self.at(on_element, span.start, compiler_code_location)
    }
}

pub trait CompilerErrorDisplay {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

pub struct TypeMismatch<TContext> {
    pub context: TContext,
    pub expected: TypeInstanceId,
    pub actual: TypeInstanceId,
}
impl<T> CompilerErrorData for TypeMismatch<T> {}

pub struct AssignContext<'source> {
    pub assign_lvalue_expr: MIRExprLValue<'source>,
}

impl<'source> CompilerErrorDisplay for CompilerErrorContext<TypeMismatch<AssignContext<'source>>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let var_type_str = self.error.expected.to_string(type_db);
        let expr_type_str = self.error.actual.to_string(type_db);

        write!(f, "Assigned type mismatch: {on_element}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_element = self.on_element.diag_name(),
            var = MIRPrinter::new(type_db).print_lvalue(&self.error.context.assign_lvalue_expr),
        )
    }
}

pub struct ReturnTypeContext();

impl CompilerErrorDisplay for CompilerErrorContext<TypeMismatch<ReturnTypeContext>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.error.actual.to_string(type_db);
        let expected_name = self.error.expected.to_string(type_db);
        write!(f, "Return type mismatch: {on_element} returns {return_type_name} but expression returns {expr_return_type_name}",
            on_element = self.on_element.diag_name(),
            return_type_name = expected_name,
            expr_return_type_name = passed_name,
        )
    }
}

pub struct FunctionCallContext {
    pub called_function_name: FunctionName,
    pub argument_position: usize,
}

impl CompilerErrorDisplay for CompilerErrorContext<TypeMismatch<FunctionCallContext>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.error.actual.to_string(type_db);
        let expected_name = self.error.expected.to_string(type_db);
        match &self.error.context.called_function_name {
            FunctionName::Function(function_name) => {
                write!(f, "Function argument type mismatch: {on_element}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(),
                    function_called = *function_name,
                    position = self.error.context.argument_position
                )
            }
            FunctionName::IndexAccess => {
                write!(f, "Function argument type mismatch: {on_element}, on index operator, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(),
                    position = self.error.context.argument_position
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } =>
                write!(f, "Function argument type mismatch: {on_element}, on call to method {method} of {type_name}, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(),
                    position = self.error.context.argument_position,
                    type_name = type_name,
                    method = function_name
                )
        }
    }
}

pub struct FunctionCallArgumentCountMismatch {
    pub called_function_name: FunctionName,
    pub expected_count: usize,
    pub passed_count: usize,
}
impl CompilerErrorData for FunctionCallArgumentCountMismatch {}

impl CompilerErrorDisplay for CompilerErrorContext<FunctionCallArgumentCountMismatch> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &self.error.called_function_name {
            FunctionName::Function(call_name) => {
                write!(f, "Argument count mismatch: {on_element}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(),
                    function_called = *call_name,
                    expected_args = self.error.expected_count,
                    passed_args = self.error.passed_count,
                )
            }
            FunctionName::IndexAccess => {
                write!(f, "Argument count mismatch: {on_element}, index operator expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(),
                    expected_args = self.error.expected_count,
                    passed_args = self.error.passed_count,
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } => {
                write!(f, "Argument count mismatch: {on_element}, call to method {method} of {type_name} expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(),
                    method = function_name,
                    type_name = type_name,
                    expected_args = self.error.expected_count,
                    passed_args = self.error.passed_count,
                )
            }
        }
    }
}

pub struct CallToNonCallableType {
    pub actual_type: Option<TypeInstanceId>,
}
impl CompilerErrorData for CallToNonCallableType {}

impl CompilerErrorDisplay for CompilerErrorContext<CallToNonCallableType> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.error.actual_type {
            Some(type_id) => write!(
                f,
                "{on_element}, call to non-callable type {non_callable_type_name}",
                on_element = self.on_element.diag_name(),
                non_callable_type_name = type_id.to_string(type_db)
            ),
            None => write!(
                f,
                "{on_element}, call to non-callable type",
                on_element = self.on_element.diag_name(),
            ),
        }
    }
}

pub struct TypeNotFound {
    pub type_name: HIRType,
}
impl CompilerErrorData for TypeNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<TypeNotFound> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type not found: {type_not_found}",
            on_element = self.on_element.diag_name(),
            type_not_found = HIRTypeDisplayer::new(&self.error.type_name),
        )
    }
}

pub struct TypePromotionFailure {
    pub target_type: TypeInstanceId,
}
impl CompilerErrorData for TypePromotionFailure {}

impl CompilerErrorDisplay for CompilerErrorContext<TypePromotionFailure> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type promotion failure: Cannot promote an integer literal to type: {target_type_name}",
            on_element = self.on_element.diag_name(),
            target_type_name = self.error.target_type.to_string(type_db),
        )
    }
}

pub struct UnexpectedTypeFound {
    pub type_def: TypeInstanceId,
}
impl CompilerErrorData for UnexpectedTypeFound {}

impl CompilerErrorDisplay for CompilerErrorContext<UnexpectedTypeFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unexpected type found in expression: {unexpected_type}",
            on_element = self.on_element.diag_name(),
            unexpected_type = self.error.type_def.to_string(type_db),
        )
    }
}

pub struct OutOfTypeBounds<'source> {
    pub typ: TypeInstanceId,
    pub expr: HIRExprMetadata<'source>,
}
impl CompilerErrorData for OutOfTypeBounds<'_> {}

impl<'source> CompilerErrorDisplay for CompilerErrorContext<OutOfTypeBounds<'source>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, literal value {expr:?} is out of bounds for type {type}. You can try extracting this value to a different variable and assign a larger type.",
            on_element = self.on_element.diag_name(),
            expr = self.error.expr,
            type = self.error.typ.to_string(type_db),
        )
    }
}

pub struct InvalidCast<'source> {
    pub expr: MIRExpr<'source>,
    pub cast_to: TypeInstanceId,
}
impl CompilerErrorData for InvalidCast<'_> {}

impl<'source> CompilerErrorDisplay for CompilerErrorContext<InvalidCast<'source>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, value {expr} of type {type} cannot be casted to {cast_type}",
            on_element = self.on_element.diag_name(),
            expr = MIRPrinter::new( type_db).print(&self.error.expr),
            type = self.error.expr.get_type().to_string(type_db),
            cast_type = self.error.cast_to.to_string(type_db),
        )
    }
}

pub struct BinaryOperatorNotFound {
    pub lhs: TypeInstanceId,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}
impl CompilerErrorData for BinaryOperatorNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<BinaryOperatorNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, binary operator {operator} not found for types: {lhs_type} {operator} {rhs_type}",
            on_element = self.on_element.diag_name(),
            operator = self.error.operator.to_string(),
            lhs_type = self.error.lhs.to_string(type_db),
            rhs_type = self.error.rhs.to_string(type_db)
        )
    }
}

pub struct UnaryOperatorNotFound {
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}
impl CompilerErrorData for UnaryOperatorNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<UnaryOperatorNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unary operator {operator} not found for type: {rhs_type}",
            on_element = self.on_element.diag_name(),
            operator = self.error.operator.to_string(),
            rhs_type = self.error.rhs.to_string(type_db)
        )
    }
}

pub struct FieldNotFound {
    pub object_type: TypeInstanceId,
    pub field: InternedString,
}
impl CompilerErrorData for FieldNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<FieldNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, tried to access field {field} on type {type_name} but no such field exists.",
            on_element = self.on_element.diag_name(),
            field = self.error.field,
            type_name = self.error.object_type.to_string(type_db)
        )
    }
}

pub struct MethodNotFound {
    pub object_type: TypeInstanceId,
    pub method: InternedString,
}
impl CompilerErrorData for MethodNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<MethodNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, tried to access method {method} on type {type_name} but no such method exists.",
            on_element = self.on_element.diag_name(),
            method = self.error.method,
            type_name = self.error.object_type.to_string(type_db)
        )
    }
}

pub struct FieldOrMethodNotFoundInTypeConstructor {
    pub object_type: TypeConstructorId,
    pub field_or_method: InternedString,
}
impl CompilerErrorData for FieldOrMethodNotFoundInTypeConstructor {}

impl CompilerErrorDisplay for CompilerErrorContext<FieldOrMethodNotFoundInTypeConstructor> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, tried to access field/method {field_or_method} on type {type_name} but no such field or method exists.",
            on_element = self.on_element.diag_name(),
            field_or_method = self.error.field_or_method,
            type_name = self.error.object_type.to_string(&type_db.constructors)
        )
    }
}

pub struct InsufficientTypeInformationForArray {}
impl CompilerErrorData for InsufficientTypeInformationForArray {}

impl CompilerErrorDisplay for CompilerErrorContext<InsufficientTypeInformationForArray> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, array expression failed type inference: Array has no items, and/or variable declaration has no type declaration or type hint.",
            on_element = self.on_element.diag_name()
        )
    }
}

pub struct ArrayExpressionsNotAllTheSameType {
    pub expected_type: TypeInstanceId,
}
impl CompilerErrorData for ArrayExpressionsNotAllTheSameType {}

impl CompilerErrorDisplay for CompilerErrorContext<ArrayExpressionsNotAllTheSameType> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, expressions in array expected to be of type {type_name} but an expression of a different type was found",
            on_element = self.on_element.diag_name(),
            type_name = self.error.expected_type.to_string(type_db)
        )
    }
}

pub struct IfStatementNotBoolean {
    pub actual_type: TypeInstanceId,
}
impl CompilerErrorData for IfStatementNotBoolean {}

impl CompilerErrorDisplay for CompilerErrorContext<IfStatementNotBoolean> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, if statement expects boolean expression, but actual type is {actual_type}",
            on_element = self.on_element.diag_name(),
            actual_type = self.error.actual_type.to_string(type_db)
        )
    }
}

pub struct TypeInferenceFailure {
    pub variable: String,
}
impl CompilerErrorData for TypeInferenceFailure {}

impl CompilerErrorDisplay for CompilerErrorContext<TypeInferenceFailure> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type inference failed for variable {variable}",
            on_element = self.on_element.diag_name(),
            variable = self.error.variable,
        )
    }
}

pub struct UnexpectedTypeInferenceMismatch<'source> {
    pub inferred: TypeInstanceId,
    pub checked: TypeInstanceId,
    pub expr: MIRExpr<'source>,
}
impl CompilerErrorData for UnexpectedTypeInferenceMismatch<'_> {}

impl<'source> CompilerErrorDisplay
    for CompilerErrorContext<UnexpectedTypeInferenceMismatch<'source>>
{
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "Type inference/check bug: {on_element}, type inferred for expression {expr_str} was {inferred_type}, but type checker detected {checked_type}",
            on_element = self.on_element.diag_name(),
            expr_str = MIRPrinter::new( type_db).print(&self.error.expr),
            inferred_type = self.error.inferred.to_string(type_db),
            checked_type = self.error.checked.to_string(type_db)
        )
    }
}

pub struct InternalError {
    pub error: String,
}
impl CompilerErrorData for InternalError {}

impl CompilerErrorDisplay for CompilerErrorContext<InternalError> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Compiler internal error: {err}",
            on_element = self.on_element.diag_name(),
            err = self.error.error
        )
    }
}

pub struct TypeConstructionFailure {
    pub error: TypeConstructionError,
}
impl CompilerErrorData for TypeConstructionFailure {}

impl CompilerErrorDisplay for CompilerErrorContext<TypeConstructionFailure> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type construction failed: {variable}",
            on_element = self.on_element.diag_name(),
            variable = match self.error.error {
                TypeConstructionError::TypeNotFound { name } => format!("Type not found: {}", name),
                TypeConstructionError::IncorrectNumberOfArgs { expected, received } =>
                    format!("Incorrect number of args: expected {expected}, received {received}"),
                TypeConstructionError::InsufficientInformation =>
                    "Insufficient information to construct type".into(),
                TypeConstructionError::InvalidTypeConstructionArguments =>
                    "Invalid type construction arguments".into(),
            }
        )
    }
}

pub struct VariableNotFound {
    pub variable_name: InternedString,
}
impl CompilerErrorData for VariableNotFound {}

impl CompilerErrorDisplay for CompilerErrorContext<VariableNotFound> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, variable not found: {variable:#?}",
            on_element = self.on_element.diag_name(),
            variable = self.error.variable_name,
        )
    }
}

pub struct AssignToNonLValueError {}
impl CompilerErrorData for AssignToNonLValueError {}

impl CompilerErrorDisplay for CompilerErrorContext<AssignToNonLValueError> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Attempted to assign to a non-lvalue expression",
            on_element = self.on_element.diag_name()
        )
    }
}

pub struct DerefOnNonPointerError {
    pub attempted_type: TypeInstanceId,
}
impl CompilerErrorData for DerefOnNonPointerError {}

impl CompilerErrorDisplay for CompilerErrorContext<DerefOnNonPointerError> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Attempted to deref a non-pointer type: {attempted_type}",
            on_element = self.on_element.diag_name(),
            attempted_type = self.error.attempted_type.to_string(type_db)
        )
    }
}

pub struct DerefOnNonPointerErrorUnconstructed {
    pub attempted_type: TypeConstructParams,
}

impl CompilerErrorData for DerefOnNonPointerErrorUnconstructed {}

impl CompilerErrorDisplay for CompilerErrorContext<DerefOnNonPointerErrorUnconstructed> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Attempted to deref a non-pointer type: {attempted_type}",
            on_element = self.on_element.diag_name(),
            attempted_type = self.error.attempted_type.to_string(&type_db.constructors)
        )
    }
}

pub struct VarargsNotSupported {}
impl CompilerErrorData for VarargsNotSupported {}

impl CompilerErrorDisplay for CompilerErrorContext<VarargsNotSupported> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, varargs not supported on non-intrinsic or external functions",
            on_element = self.on_element.diag_name(),
        )
    }
}

pub struct ImplMismatchTypeArgs {}
impl CompilerErrorData for ImplMismatchTypeArgs {}

impl CompilerErrorDisplay for CompilerErrorContext<ImplMismatchTypeArgs> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, amount of type parameters is not the same as the struct. Check the struct type arguments and the impl type arguments, and make sure they have the same amount.",
            on_element = self.on_element.diag_name(),
        )
    }
}

pub struct RefOnNonLValueError {}
impl CompilerErrorData for RefOnNonLValueError {}

impl CompilerErrorDisplay for CompilerErrorContext<RefOnNonLValueError> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, lvalue is required when creating a reference",
            on_element = self.on_element.diag_name()
        )
    }
}

#[derive(Debug)]
pub struct ReportToken(
    //this is private because we don't want outside code to construct this struct directly
    std::marker::PhantomData<()>,
);

impl ReportToken {
    pub fn as_type_inference_error<T>(self) -> Result<T, crate::types::diagnostics::CompilerError> {
        Err(crate::types::diagnostics::CompilerError::TypeInferenceError(self))
    }
    pub fn as_type_check_error<T>(self) -> Result<T, crate::types::diagnostics::CompilerError> {
        Err(crate::types::diagnostics::CompilerError::TypeCheckError(
            self,
        ))
    }
}

pub struct CompilerErrorList<T: CompilerErrorData> {
    pub errors: Vec<CompilerErrorContext<T>>,
}

impl<T: CompilerErrorData> CompilerErrorList<T> {
    pub fn new() -> CompilerErrorList<T> {
        CompilerErrorList { errors: vec![] }
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn push(&mut self, error: CompilerErrorContext<T>) -> ReportToken {
        self.errors.push(error);
        ReportToken(std::marker::PhantomData)
    }

    pub fn push_inference_error<R>(
        &mut self,
        error: CompilerErrorContext<T>,
    ) -> Result<R, CompilerError> {
        self.errors.push(error);
        Err(CompilerError::TypeInferenceError(ReportToken(
            std::marker::PhantomData,
        )))
    }

    pub fn push_typecheck_error<R>(
        &mut self,
        error: CompilerErrorContext<T>,
    ) -> Result<R, CompilerError> {
        self.errors.push(error);
        Err(CompilerError::TypeCheckError(ReportToken(
            std::marker::PhantomData,
        )))
    }
}

impl<T, Idx> std::ops::Index<Idx> for CompilerErrorList<T>
where
    T: CompilerErrorData,
    Idx: std::slice::SliceIndex<[CompilerErrorContext<T>]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        return &self.errors[index];
    }
}

macro_rules! make_type_errors {
    ($($field:ident = $typename:ty), *) => {

        pub struct TypeErrors<'source>{
            $(
                pub $field: CompilerErrorList<$typename>,
            )*
        }

        impl<'source> TypeErrors<'source> {
            pub fn new() -> TypeErrors<'source> {
                TypeErrors {
                    $(
                        $field: CompilerErrorList::new(),
                    )*
                }
            }
            pub fn count(&self) -> usize {
                $(
                    self.$field.len() +
                )* 0
            }
        }

        impl<'errors, 'callargs, 'type_db, 'source, 'files> Display for TypeErrorPrinter<'errors, 'type_db, 'source, 'files> {

            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.errors.count() == 0 {
                    return Ok(());
                }
                $(
                    for err in self.errors.$field.errors.iter() {
                        let file = &self.file_table[err.location.file.0];
                        let file_name = &file.path;
                        let tok = file.token_table.tokens[err.location.index];
                        let span = file.token_table.spans[tok.span_index.index];
                        let line = span.start.line;
                        let column = span.start.column;
                        write!(f, "{file_name}:{line}:{column}: ")?;
                        err.fmt_err(self.type_db, f)?;
                        write!(f, "\n")?;
                        if REPORT_COMPILER_ERR_LOCATION {
                                write!(f, "Error generated at {}\n", err.compiler_code_location)?;
                        }
                    }
                )*

                return Ok(());
            }
        }


        pub struct TypeErrorPrinter<'errors, 'type_db, 'source, 'files> {
            pub errors: &'errors TypeErrors<'source>,
            pub type_db: &'type_db TypeInstanceManager,
            pub file_table: &'files [FileTableEntry],
        }

        impl<'errors, 'callargs, 'type_db, 'source, 'files> TypeErrorPrinter<'errors, 'type_db, 'source, 'files> {
            pub fn new(
                errors: &'errors TypeErrors<'source>,
                type_db: &'type_db TypeInstanceManager,

                file_table: &'files [FileTableEntry],
            ) -> TypeErrorPrinter<'errors, 'type_db, 'source, 'files> {
                TypeErrorPrinter { errors, type_db, file_table }
            }
        }

    }
}

pub const REPORT_COMPILER_ERR_LOCATION: bool = false;

make_type_errors!(
    assign_mismatches = TypeMismatch<AssignContext<'source>>,
    return_type_mismatches = TypeMismatch<ReturnTypeContext>,
    function_call_mismatches = TypeMismatch<FunctionCallContext>,
    function_call_argument_count = FunctionCallArgumentCountMismatch,
    call_non_callable = CallToNonCallableType,
    type_not_found = TypeNotFound,
    type_promotion_failure = TypePromotionFailure,
    variable_not_found = VariableNotFound,
    unexpected_types = UnexpectedTypeFound,
    binary_op_not_found = BinaryOperatorNotFound,
    unary_op_not_found = UnaryOperatorNotFound,
    field_not_found = FieldNotFound,
    method_not_found = MethodNotFound,
    field_or_method_not_found_in_type_constructor = FieldOrMethodNotFoundInTypeConstructor,
    insufficient_array_type_info = InsufficientTypeInformationForArray,
    array_expressions_not_all_the_same_type = ArrayExpressionsNotAllTheSameType,
    if_statement_unexpected_type = IfStatementNotBoolean,
    type_inference_failure = TypeInferenceFailure,
    type_construction_failure = TypeConstructionFailure,
    out_of_bounds = OutOfTypeBounds<'source>,
    invalid_casts = InvalidCast<'source>,
    type_inference_check_mismatch = UnexpectedTypeInferenceMismatch<'source>,
    invalid_derefed_type = DerefOnNonPointerError,
    invalid_derefed_type_unconstructed = DerefOnNonPointerErrorUnconstructed,
    invalid_refed_type = RefOnNonLValueError,
    varargs_not_supported = VarargsNotSupported,
    internal_error = InternalError,
    impl_mismatch_type_args = ImplMismatchTypeArgs
);
