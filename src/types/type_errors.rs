use crate::ast::lexer::TokenSpanIndex;
use crate::ast::parser::Spanned;
use crate::interner::{InternedString, StringInterner};
use crate::semantic::context::{FileTableEntry, FileTableIndex};

use crate::semantic::mir::MIRExpr;
use crate::semantic::mir_printer::MIRExprPrinter;
use crate::{
    ast::lexer::Operator,
    semantic::{
        hir::{Checked, HIRExprMetadata, HIRType, HIRTypeDisplayer},
        hir_type_resolution::RootElementType,
        type_checker::FunctionName,
    },
};
use std::fmt::Display;

use super::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

pub struct TypeErrorData<T>
where
    T: TypeError,
{
    pub error: T,
    pub on_element: RootElementType,
    pub file: FileTableIndex,
    pub location: TokenSpanIndex,
}

pub trait TypeError {}

pub trait TypeErrorAtLocation<T: TypeError> {
    fn at(
        self,
        on_element: RootElementType,
        file: FileTableIndex,
        location: TokenSpanIndex,
    ) -> TypeErrorData<T>;
    fn at_spanned<S: Spanned>(
        self,
        on_element: RootElementType,
        file: FileTableIndex,
        span: &S,
    ) -> TypeErrorData<T>;
}

impl<T: Sized + TypeError> TypeErrorAtLocation<T> for T {
    fn at(
        self,
        on_element: RootElementType,
        file: FileTableIndex,
        location: TokenSpanIndex,
    ) -> TypeErrorData<T> {
        TypeErrorData {
            error: self,
            on_element,
            file,
            location,
        }
    }

    fn at_spanned<S: Spanned>(
        self,
        on_element: RootElementType,
        file: FileTableIndex,
        span: &S,
    ) -> TypeErrorData<T> {
        let span = span.get_span();
        self.at(on_element, file, span.start)
    }
}

pub trait TypeErrorDisplay {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

pub struct TypeMismatch<TContext> {
    pub context: TContext,
    pub expected: TypeInstanceId,
    pub actual: TypeInstanceId,
}
impl<T> TypeError for TypeMismatch<T> {}

pub struct AssignContext {
    pub target_variable_name: InternedString,
}

impl TypeErrorDisplay for TypeErrorData<TypeMismatch<AssignContext>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let var_type_str = self.error.expected.as_string(type_db);
        let expr_type_str = self.error.actual.as_string(type_db);

        write!(f, "Assigned type mismatch: {on_element}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_element = self.on_element.diag_name(interner),
            var = interner.borrow(self.error.context.target_variable_name)
        )
    }
}

pub struct ReturnTypeContext();

impl TypeErrorDisplay for TypeErrorData<TypeMismatch<ReturnTypeContext>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.error.actual.as_string(type_db);
        let expected_name = self.error.expected.as_string(type_db);
        write!(f, "Return type mismatch: {on_element} returns {return_type_name} but expression returns {expr_return_type_name}",
            on_element = self.on_element.diag_name(interner),
            return_type_name = expected_name,
            expr_return_type_name = passed_name,
        )
    }
}

pub struct FunctionCallContext {
    pub called_function_name: FunctionName,
    pub argument_position: usize,
}

impl TypeErrorDisplay for TypeErrorData<TypeMismatch<FunctionCallContext>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.error.actual.as_string(type_db);
        let expected_name = self.error.expected.as_string(type_db);
        match &self.error.context.called_function_name {
            FunctionName::Function(function_name) => {
                write!(f, "Function argument type mismatch: {on_element}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    function_called = interner.get_string(*function_name),
                    position = self.error.context.argument_position
                )
            }
            FunctionName::IndexAccess => {
                write!(f, "Function argument type mismatch: {on_element}, on index operator, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    position = self.error.context.argument_position
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } =>
                write!(f, "Function argument type mismatch: {on_element}, on call to method {method} of {type_name}, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    position = self.error.context.argument_position,
                    type_name = &interner.borrow(*type_name),
                    method = &interner.borrow(*function_name)
                )
        }
    }
}

pub struct FunctionCallArgumentCountMismatch {
    pub called_function_name: FunctionName,
    pub expected_count: usize,
    pub passed_count: usize,
}
impl TypeError for FunctionCallArgumentCountMismatch {}

impl TypeErrorDisplay for TypeErrorData<FunctionCallArgumentCountMismatch> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &self.error.called_function_name {
            FunctionName::Function(call_name) => {
                write!(f, "Argument count mismatch: {on_element}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(interner),
                    function_called = interner.get_string(*call_name),
                    expected_args = self.error.expected_count,
                    passed_args = self.error.passed_count,
                )
            }
            FunctionName::IndexAccess => {
                write!(f, "Argument count mismatch: {on_element}, index operator expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(interner),
                    expected_args = self.error.expected_count,
                    passed_args = self.error.passed_count,
                )
            }
            FunctionName::Method {
                function_name: _,
                type_name: _,
            } => todo!("method calls not fully implemented"),
        }
    }
}

pub struct CallToNonCallableType {
    pub actual_type: TypeInstanceId,
}
impl TypeError for CallToNonCallableType {}

impl TypeErrorDisplay for TypeErrorData<CallToNonCallableType> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, call to non-callable type {non_callable_type_name}",
            on_element = self.on_element.diag_name(interner),
            non_callable_type_name = self.error.actual_type.as_string(type_db),
        )
    }
}

pub struct TypeNotFound {
    pub type_name: HIRType,
}
impl TypeError for TypeNotFound {}

impl TypeErrorDisplay for TypeErrorData<TypeNotFound> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type not found: {type_not_found}",
            on_element = self.on_element.diag_name(interner),
            type_not_found = HIRTypeDisplayer::new(&self.error.type_name, interner),
        )
    }
}

pub struct UnexpectedTypeFound {
    pub type_def: TypeInstanceId,
}
impl TypeError for UnexpectedTypeFound {}

impl TypeErrorDisplay for TypeErrorData<UnexpectedTypeFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unexpected type found in expression: {unexpected_type}",
            on_element = self.on_element.diag_name(interner),
            unexpected_type = self.error.type_def.as_string(type_db),
        )
    }
}

pub struct OutOfTypeBounds<'source> {
    pub typ: TypeInstanceId,
    pub expr: HIRExprMetadata<'source>,
}
impl TypeError for OutOfTypeBounds<'_> {}

impl<'source> TypeErrorDisplay for TypeErrorData<OutOfTypeBounds<'source>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, literal value {expr:?} is out of bounds for type {type}. You can try extracting this value to a different variable and assign a larger type.",
            on_element = self.on_element.diag_name(interner),
            expr = self.error.expr,
            type = self.error.typ.as_string(type_db),
        )
    }
}

pub struct InvalidCast<'source> {
    pub expr: MIRExpr<'source, Checked>,
    pub cast_to: TypeInstanceId,
}
impl TypeError for InvalidCast<'_> {}

impl<'source> TypeErrorDisplay for TypeErrorData<InvalidCast<'source>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, value {expr} of type {type} cannot be casted to {cast_type}",
            on_element = self.on_element.diag_name(interner),
            expr = MIRExprPrinter::new(interner).print(&self.error.expr),
            type = self.error.expr.get_type().as_string(type_db),
            cast_type = self.error.cast_to.as_string(type_db),
        )
    }
}

pub struct BinaryOperatorNotFound {
    pub lhs: TypeInstanceId,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}
impl TypeError for BinaryOperatorNotFound {}

impl TypeErrorDisplay for TypeErrorData<BinaryOperatorNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, binary operator {operator} not found for types: {lhs_type} {operator} {rhs_type}",
            on_element = self.on_element.diag_name(interner),
            operator = self.error.operator.to_string(),
            lhs_type = self.error.lhs.as_string(type_db),
            rhs_type = self.error.rhs.as_string(type_db)
        )
    }
}

pub struct UnaryOperatorNotFound {
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}
impl TypeError for UnaryOperatorNotFound {}

impl TypeErrorDisplay for TypeErrorData<UnaryOperatorNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unary operator {operator} not found for type: {rhs_type}",
            on_element = self.on_element.diag_name(interner),
            operator = self.error.operator.to_string(),
            rhs_type = self.error.rhs.as_string(type_db)
        )
    }
}

pub struct FieldOrMethodNotFound {
    pub object_type: TypeInstanceId,
    pub field_or_method: InternedString,
}
impl TypeError for FieldOrMethodNotFound {}

impl TypeErrorDisplay for TypeErrorData<FieldOrMethodNotFound> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, tried to access field/method {field_or_method} on type {type_name} but no such field or method exists.",
            on_element = self.on_element.diag_name(interner),
            field_or_method = interner.borrow(self.error.field_or_method),
            type_name = self.error.object_type.as_string(type_db)
        )
    }
}

pub struct InsufficientTypeInformationForArray {}
impl TypeError for InsufficientTypeInformationForArray {}

impl TypeErrorDisplay for TypeErrorData<InsufficientTypeInformationForArray> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, array expression failed type inference: Array has no items, and/or variable declaration has no type declaration or type hint.",
            on_element = self.on_element.diag_name(interner)
        )
    }
}

pub struct ArrayExpressionsNotAllTheSameType {
    pub expected_type: TypeInstanceId,
}
impl TypeError for ArrayExpressionsNotAllTheSameType {}

impl TypeErrorDisplay for TypeErrorData<ArrayExpressionsNotAllTheSameType> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, expressions in array expected to be of type {type_name} but an expression of a different type was found",
            on_element = self.on_element.diag_name(interner),
            type_name = self.error.expected_type.as_string(type_db)
        )
    }
}

pub struct IfStatementNotBoolean {
    pub actual_type: TypeInstanceId,
}
impl TypeError for IfStatementNotBoolean {}

impl TypeErrorDisplay for TypeErrorData<IfStatementNotBoolean> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, if statement expects boolean expression, but actual type is {actual_type}",
            on_element = self.on_element.diag_name(interner),
            actual_type = self.error.actual_type.as_string(type_db)
        )
    }
}

pub struct TypeInferenceFailure {
    pub variable: String,
}
impl TypeError for TypeInferenceFailure {}

impl TypeErrorDisplay for TypeErrorData<TypeInferenceFailure> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type inference failed for variable {variable}",
            on_element = self.on_element.diag_name(interner),
            variable = self.error.variable,
        )
    }
}

pub struct UnexpectedTypeInferenceMismatch<'source> {
    pub inferred: TypeInstanceId,
    pub checked: TypeInstanceId,
    pub expr: MIRExpr<'source, Checked>,
}
impl TypeError for UnexpectedTypeInferenceMismatch<'_> {}

impl<'source> TypeErrorDisplay for TypeErrorData<UnexpectedTypeInferenceMismatch<'source>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "Type inference/check bug: {on_element}, type inferred for expression {expr_str} was {inferred_type}, but type checker detected {checked_type}",
            on_element = self.on_element.diag_name(interner),
            expr_str = MIRExprPrinter::new(interner).print(&self.error.expr),
            inferred_type = self.error.inferred.as_string(type_db),
            checked_type = self.error.checked.as_string(type_db)
        )
    }
}

pub struct TypeConstructionFailure {
    pub error: TypeConstructionError,
}
impl TypeError for TypeConstructionFailure {}

impl TypeErrorDisplay for TypeErrorData<TypeConstructionFailure> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type construction failed: {variable}",
            on_element = self.on_element.diag_name(interner),
            variable = match self.error.error {
                TypeConstructionError::TypeNotFound { name } =>
                    format!("Type not found: {}", interner.borrow(name)),
                TypeConstructionError::IncorrectNumberOfArgs { expected, received } =>
                    format!("Incorrect number of args: expected {expected}, received {received}"),
            }
        )
    }
}

pub struct VariableNotFound {
    pub variable_name: InternedString,
}
impl TypeError for VariableNotFound {}

impl TypeErrorDisplay for TypeErrorData<VariableNotFound> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, variable not found: {variable:#?}",
            on_element = self.on_element.diag_name(interner),
            variable = interner.borrow(self.error.variable_name),
        )
    }
}

pub struct AssignToNonLValueError {
}
impl TypeError for AssignToNonLValueError {}

impl TypeErrorDisplay for TypeErrorData<AssignToNonLValueError> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Attempted to assign to a non-lvalue expression",
            on_element = self.on_element.diag_name(interner)
        )
    }
}

pub struct DerefOnNonPointerError {
    pub attempted_type: TypeInstanceId,
}
impl TypeError for DerefOnNonPointerError {}

impl TypeErrorDisplay for TypeErrorData<DerefOnNonPointerError> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, Attempted to deref a non-pointer type: {attempted_type}",
            on_element = self.on_element.diag_name(interner),
            attempted_type = self.error.attempted_type.as_string(type_db)
        )
    }
}

pub struct RefOnNonLValueError {}
impl TypeError for RefOnNonLValueError {}

impl TypeErrorDisplay for TypeErrorData<RefOnNonLValueError> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, lvalue is required when creating a reference",
            on_element = self.on_element.diag_name(interner)
        )
    }
}

macro_rules! make_type_errors {
    ($($field:ident = $typename:ty), *) => {

        pub struct TypeErrors<'source>{
            $(
                pub $field: Vec<TypeErrorData<$typename>>,
            )*
        }

        impl<'source> TypeErrors<'source> {
            pub fn new() -> TypeErrors<'source> {
                TypeErrors {
                    $(
                        $field: vec![],
                    )*
                }
            }
            pub fn count(&self) -> usize {
                $(
                    self.$field.len() +
                )* 0
            }
        }

        impl<'errors, 'callargs, 'type_db, 'source, 'interner, 'files> Display for TypeErrorPrinter<'errors, 'type_db, 'source, 'interner, 'files> {

            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.errors.count() == 0 {
                    return Ok(());
                }
                $(
                    for err in self.errors.$field.iter() {
                        let file = &self.file_table[err.file.0];
                        let file_name = &file.path;
                        let tok = file.token_table.tokens[err.location.0];
                        let span = file.token_table.spans[tok.span_index.0];
                        let line = span.start.line;
                        let column = span.start.column;
                        write!(f, "{file_name}:{line}:{column}: ")?;
                        err.fmt_err(self.type_db, self.interner, f)?;
                        write!(f, "\n")?;
                    }
                )*

                return Ok(());
            }
        }


        pub struct TypeErrorPrinter<'errors, 'type_db, 'source, 'interner, 'files> {
            pub errors: &'errors TypeErrors<'source>,
            pub type_db: &'type_db TypeInstanceManager<'interner>,
            pub interner: &'interner StringInterner,
            pub file_table: &'files [FileTableEntry],
        }

        impl<'errors, 'callargs, 'type_db, 'source, 'interner, 'files> TypeErrorPrinter<'errors, 'type_db, 'source, 'interner, 'files> {
            pub fn new(
                errors: &'errors TypeErrors<'source>,
                type_db: &'type_db TypeInstanceManager<'interner>,
                interner: &'interner StringInterner,
                file_table: &'files [FileTableEntry],
            ) -> TypeErrorPrinter<'errors, 'type_db, 'source, 'interner, 'files> {
                TypeErrorPrinter { errors, type_db, interner, file_table }
            }
        }

    }
}

make_type_errors!(
    assign_mismatches = TypeMismatch<AssignContext>,
    return_type_mismatches = TypeMismatch<ReturnTypeContext>,
    function_call_mismatches = TypeMismatch<FunctionCallContext>,
    function_call_argument_count = FunctionCallArgumentCountMismatch,
    call_non_callable = CallToNonCallableType,
    type_not_found = TypeNotFound,
    variable_not_found = VariableNotFound,
    unexpected_types = UnexpectedTypeFound,
    binary_op_not_found = BinaryOperatorNotFound,
    unary_op_not_found = UnaryOperatorNotFound,
    field_or_method_not_found = FieldOrMethodNotFound,
    insufficient_array_type_info = InsufficientTypeInformationForArray,
    array_expressions_not_all_the_same_type = ArrayExpressionsNotAllTheSameType,
    if_statement_unexpected_type = IfStatementNotBoolean,
    type_inference_failure = TypeInferenceFailure,
    type_construction_failure = TypeConstructionFailure,
    out_of_bounds = OutOfTypeBounds<'source>,
    invalid_casts = InvalidCast<'source>,
    type_inference_check_mismatch = UnexpectedTypeInferenceMismatch<'source>,
    invalid_derefed_type = DerefOnNonPointerError,
    invalid_refed_type = RefOnNonLValueError
);
