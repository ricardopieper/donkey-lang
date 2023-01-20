use std::fmt::Display;

use crate::{
    ast::lexer::{Operator, SourceString},
    semantic::{
        hir::{Checked, HIRAstMetadata, HIRExpr, HIRExprMetadata, HIRType},
        hir_printer::{expr_str, operator_str},
        hir_type_resolution::RootElementType,
        type_checker::FunctionName,
    },
};

use super::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

pub trait TypeErrorDisplay {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

pub struct TypeMismatch<'source, TContext> {
    pub on_element: RootElementType<'source>,
    pub context: TContext,
    pub expected: TypeInstanceId,
    pub actual: TypeInstanceId,
}

pub struct AssignContext<'source> {
    pub target_variable_name: SourceString<'source>,
}

impl TypeErrorDisplay for TypeMismatch<'_, AssignContext<'_>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let var_type_str = self.expected.as_string(type_db);
        let expr_type_str = self.actual.as_string(type_db);

        write!(f,  "Assigned type mismatch: {on_element}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_element = self.on_element,
            var = self.context.target_variable_name
        )
    }
}

pub struct ReturnTypeContext<'source>(pub Option<HIRAstMetadata<'source>>);

impl TypeErrorDisplay for TypeMismatch<'_, ReturnTypeContext<'_>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        write!(f,  "Return type mismatch: {on_element} returns {return_type_name} but expression {expr_str:?} returns {expr_return_type_name}",
            on_element = self.on_element.get_name(),
            return_type_name = expected_name,
            expr_str = self.context.0,
            expr_return_type_name = passed_name,
        )
    }
}

pub struct FunctionCallContext<'source> {
    pub called_function_name: FunctionName<'source>,
    pub argument_position: usize,
}

impl TypeErrorDisplay for TypeMismatch<'_, FunctionCallContext<'_>> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        match &self.context.called_function_name {
            FunctionName::Function(function_name) => {
                write!(f,  "Function argument type mismatch: {on_element}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element,
                    function_called = function_name,
                    position = self.context.argument_position
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Function argument type mismatch: {on_element}, on index operator, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element,
                    position = self.context.argument_position
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } =>
                write!(f,  "Function argument type mismatch: {on_element}, on call to method {function_name} of {type_name}, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element,
                    position = self.context.argument_position
                )
        }
    }
}

pub struct FunctionCallArgumentCountMismatch<'source> {
    pub on_element: RootElementType<'source>,
    pub called_function_name: FunctionName<'source>,
    pub expected_count: usize,
    pub passed_count: usize,
}

impl TypeErrorDisplay for FunctionCallArgumentCountMismatch<'_> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &self.called_function_name {
            FunctionName::Function(call_name) => {
                write!(f,  "Argument count mismatch: {on_element}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element,
                    function_called = call_name,
                    expected_args = self.expected_count,
                    passed_args = self.passed_count,
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Argument count mismatch: {on_element}, index operator expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element,
                    expected_args = self.expected_count,
                    passed_args = self.passed_count,
                )
            }
            FunctionName::Method {
                function_name: _,
                type_name: _,
            } => todo!("method calls not fully implemented"),
        }
    }
}

pub struct CallToNonCallableType<'source> {
    pub on_element: RootElementType<'source>,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for CallToNonCallableType<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, call to non-callable type {non_callable_type_name}",
            on_element = self.on_element,
            non_callable_type_name = self.actual_type.as_string(type_db),
        )
    }
}

pub struct TypeNotFound<'source> {
    pub on_element: RootElementType<'source>,
    pub type_name: HIRType<'source>,
}

impl TypeErrorDisplay for TypeNotFound<'_> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type not found: {type_not_found}",
            on_element = self.on_element,
            type_not_found = self.type_name,
        )
    }
}

pub struct UnexpectedTypeFound<'source> {
    pub on_element: RootElementType<'source>,
    pub type_def: TypeInstanceId,
}

impl TypeErrorDisplay for UnexpectedTypeFound<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unexpected type found in expression: {unexpected_type}",
            on_element = self.on_element,
            unexpected_type = self.type_def.as_string(type_db),
        )
    }
}

pub struct OutOfTypeBounds<'source> {
    pub on_element: RootElementType<'source>,
    pub typ: TypeInstanceId,
    pub expr: HIRExprMetadata<'source>,
}

impl TypeErrorDisplay for OutOfTypeBounds<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, literal value {expr:?} is out of bounds for type {type}. You can try extracting this value to a different variable and assign a larger type.",
            on_element = self.on_element,
            expr = self.expr,
            type = self.typ.as_string(type_db),
        )
    }
}

pub struct InvalidCast<'source> {
    pub on_element: RootElementType<'source>,
    pub expr: HIRExpr<'source, TypeInstanceId, Checked>,
    pub cast_to: TypeInstanceId,
}

impl TypeErrorDisplay for InvalidCast<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, value {expr} of type {type} cannot be casted to {cast_type}",
            on_element = self.on_element,
            expr = expr_str(&self.expr),
            type = self.expr.get_type().as_string(type_db),
            cast_type = self.cast_to.as_string(type_db),
        )
    }
}

pub struct BinaryOperatorNotFound<'source> {
    pub on_element: RootElementType<'source>,
    pub lhs: TypeInstanceId,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for BinaryOperatorNotFound<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, binary operator {operator} not found for types: {lhs_type} {operator} {rhs_type}",
            on_element = self.on_element,
            operator = operator_str(self.operator),
            lhs_type = self.lhs.as_string(type_db),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct UnaryOperatorNotFound<'source> {
    pub on_element: RootElementType<'source>,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for UnaryOperatorNotFound<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, unary operator {operator} not found for type: {rhs_type}",
            on_element = self.on_element,
            operator = operator_str(self.operator),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct FieldOrMethodNotFound<'source> {
    pub on_element: RootElementType<'source>,
    pub object_type: TypeInstanceId,
    pub field_or_method: SourceString<'source>,
}

impl TypeErrorDisplay for FieldOrMethodNotFound<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, tried to access field/method {field_or_method} on type {type_name} but no such field or method exists.",
            on_element = self.on_element,
            field_or_method =self.field_or_method,
            type_name = self.object_type.as_string(type_db)
        )
    }
}

pub struct InsufficientTypeInformationForArray<'source> {
    pub on_element: RootElementType<'source>,
}

impl TypeErrorDisplay for InsufficientTypeInformationForArray<'_> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, array expression failed type inference: Array has no items, and/or variable declaration has no type declaration or type hint.",
            on_element = self.on_element
        )
    }
}

pub struct ArrayExpressionsNotAllTheSameType<'source> {
    pub on_element: RootElementType<'source>,
    pub expected_type: TypeInstanceId,
}

impl TypeErrorDisplay for ArrayExpressionsNotAllTheSameType<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, expressions in array expected to be of type {type_name} but an expression of a different type was found",
            on_element = self.on_element,
            type_name = self.expected_type.as_string(type_db)
        )
    }
}

pub struct IfStatementNotBoolean<'source> {
    pub on_element: RootElementType<'source>,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for IfStatementNotBoolean<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, if statement expects boolean expression, but actual type is {actual_type}",
            on_element = self.on_element,
            actual_type = self.actual_type.as_string(type_db)
        )
    }
}

pub struct TypeInferenceFailure<'source> {
    pub on_element: RootElementType<'source>,
    pub variable: String,
}

impl TypeErrorDisplay for TypeInferenceFailure<'_> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type inference failed for variable {variable}",
            on_element = self.on_element,
            variable = self.variable,
        )
    }
}

pub struct UnexpectedTypeInferenceMismatch<'source> {
    pub on_element: RootElementType<'source>,
    pub inferred: TypeInstanceId,
    pub checked: TypeInstanceId,
    pub expr: HIRExpr<'source, TypeInstanceId, Checked>,
}

impl TypeErrorDisplay for UnexpectedTypeInferenceMismatch<'_> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "Type inference/check bug: {on_element}, type inferred for expression {expr_str} was {inferred_type}, but type checker detected {checked_type}",
            on_element = self.on_element,
            expr_str = expr_str(&self.expr),
            inferred_type = self.inferred.as_string(type_db),
            checked_type = self.checked.as_string(type_db)
        )
    }
}

pub struct TypeConstructionFailure<'source> {
    pub on_element: RootElementType<'source>,
    pub error: TypeConstructionError,
}

impl<'source> TypeErrorDisplay for TypeConstructionFailure<'source> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type construction failed: {variable:#?}",
            on_element = self.on_element,
            variable = self.error,
        )
    }
}

pub struct VariableNotFound<'source> {
    pub on_element: RootElementType<'source>,
    pub variable_name: SourceString<'source>,
}

impl TypeErrorDisplay for VariableNotFound<'_> {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, variable not found: {variable:#?}",
            on_element = self.on_element,
            variable = self.variable_name,
        )
    }
}

macro_rules! make_type_errors {
    ($($field:ident : $typename:ty), *) => {

        pub struct TypeErrors<'source>{
            $(
                pub $field: $typename,
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

        impl<'errors, 'callargs, 'type_db, 'source> Display for TypeErrorPrinter<'errors, 'type_db, 'source> {

            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.errors.count() == 0 {
                    return Ok(());
                }
                $(
                    for err in self.errors.$field.iter() {
                        err.fmt_err(self.type_db,f)?;
                        write!(f, "\n")?;
                    }
                )*

                return Ok(());
            }
        }


        pub struct TypeErrorPrinter<'errors, 'type_db, 'source> {
            pub errors: &'errors TypeErrors<'source>,
            pub type_db: &'type_db TypeInstanceManager<'source>,
        }

        impl<'errors, 'callargs, 'type_db, 'source> TypeErrorPrinter<'errors, 'type_db, 'source> {
            pub fn new(
                errors: &'errors TypeErrors<'source>,
                type_db: &'type_db TypeInstanceManager<'source>,
            ) -> TypeErrorPrinter<'errors, 'type_db, 'source> {
                TypeErrorPrinter { errors, type_db }
            }
        }

    }

}

make_type_errors!(
    assign_mismatches: Vec<TypeMismatch<'source, AssignContext<'source>>>,
    return_type_mismatches: Vec<TypeMismatch<'source, ReturnTypeContext<'source>>>,
    function_call_mismatches: Vec<TypeMismatch<'source, FunctionCallContext<'source>>>,
    function_call_argument_count: Vec<FunctionCallArgumentCountMismatch<'source>>,
    call_non_callable: Vec<CallToNonCallableType<'source>>,
    type_not_found: Vec<TypeNotFound<'source>>,
    variable_not_found: Vec<VariableNotFound<'source>>,
    unexpected_types: Vec<UnexpectedTypeFound<'source>>,
    binary_op_not_found: Vec<BinaryOperatorNotFound<'source>>,
    unary_op_not_found: Vec<UnaryOperatorNotFound<'source>>,
    field_or_method_not_found: Vec<FieldOrMethodNotFound<'source>>,
    insufficient_array_type_info: Vec<InsufficientTypeInformationForArray<'source>>,
    array_expressions_not_all_the_same_type: Vec<ArrayExpressionsNotAllTheSameType<'source>>,
    if_statement_unexpected_type: Vec<IfStatementNotBoolean<'source>>,
    type_inference_failure: Vec<TypeInferenceFailure<'source>>,
    type_construction_failure: Vec<TypeConstructionFailure<'source>>,
    out_of_bounds: Vec<OutOfTypeBounds<'source>>,
    invalid_casts: Vec<InvalidCast<'source>>,
    type_inference_check_mismatch: Vec<UnexpectedTypeInferenceMismatch<'source>>
);
