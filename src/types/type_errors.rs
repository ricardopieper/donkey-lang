use crate::interner::{InternedString, StringInterner};
use crate::{
    ast::lexer::Operator,
    semantic::{
        hir::{Checked, HIRExpr, HIRExprMetadata, HIRType, HIRTypeDisplayer},
        hir_printer::{operator_str, HIRExprPrinter},
        hir_type_resolution::RootElementType,
        type_checker::FunctionName,
    },
};
use std::fmt::Display;

use super::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

pub trait TypeErrorDisplay {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

pub struct TypeMismatch<TContext> {
    pub on_element: RootElementType,
    pub context: TContext,
    pub expected: TypeInstanceId,
    pub actual: TypeInstanceId,
}

pub struct AssignContext {
    pub target_variable_name: InternedString,
}

impl TypeErrorDisplay for TypeMismatch<AssignContext> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let var_type_str = self.expected.as_string(type_db);
        let expr_type_str = self.actual.as_string(type_db);

        write!(f,  "Assigned type mismatch: {on_element}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_element = self.on_element.diag_name(interner),
            var = interner.get_string(self.context.target_variable_name).to_string()
        )
    }
}

pub struct ReturnTypeContext();

impl TypeErrorDisplay for TypeMismatch<ReturnTypeContext> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        write!(f,  "Return type mismatch: {on_element} returns {return_type_name} but expression returns {expr_return_type_name}",
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

impl TypeErrorDisplay for TypeMismatch<FunctionCallContext> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        match &self.context.called_function_name {
            FunctionName::Function(function_name) => {
                write!(f,  "Function argument type mismatch: {on_element}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    function_called = interner.get_string(*function_name).to_string(),
                    position = self.context.argument_position
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Function argument type mismatch: {on_element}, on index operator, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    position = self.context.argument_position
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } =>
                write!(f,  "Function argument type mismatch: {on_element}, on call to method {method} of {type_name}, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_element = self.on_element.diag_name(interner),
                    position = self.context.argument_position,
                    type_name = &interner.borrow(*type_name),
                    method = &interner.borrow(*function_name)
                )
        }
    }
}

pub struct FunctionCallArgumentCountMismatch {
    pub on_element: RootElementType,
    pub called_function_name: FunctionName,
    pub expected_count: usize,
    pub passed_count: usize,
}

impl TypeErrorDisplay for FunctionCallArgumentCountMismatch {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &self.called_function_name {
            FunctionName::Function(call_name) => {
                write!(f,  "Argument count mismatch: {on_element}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(interner),
                    function_called = interner.get_string(*call_name).to_string(),
                    expected_args = self.expected_count,
                    passed_args = self.passed_count,
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Argument count mismatch: {on_element}, index operator expects {expected_args} arguments, but {passed_args} were passed",
                    on_element = self.on_element.diag_name(interner),
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

pub struct CallToNonCallableType {
    pub on_element: RootElementType,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for CallToNonCallableType {
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
            non_callable_type_name = self.actual_type.as_string(type_db),
        )
    }
}

pub struct TypeNotFound {
    pub on_element: RootElementType,
    pub type_name: HIRType,
}

impl TypeErrorDisplay for TypeNotFound {
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
            type_not_found = HIRTypeDisplayer::new(&self.type_name, interner),
        )
    }
}

pub struct UnexpectedTypeFound {
    pub on_element: RootElementType,
    pub type_def: TypeInstanceId,
}

impl TypeErrorDisplay for UnexpectedTypeFound {
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
            unexpected_type = self.type_def.as_string(type_db),
        )
    }
}

pub struct OutOfTypeBounds<'source> {
    pub on_element: RootElementType,
    pub typ: TypeInstanceId,
    pub expr: HIRExprMetadata<'source>,
}

impl<'source> TypeErrorDisplay for OutOfTypeBounds<'source> {
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
            expr = self.expr,
            type = self.typ.as_string(type_db),
        )
    }
}

pub struct InvalidCast<'source> {
    pub on_element: RootElementType,
    pub expr: HIRExpr<'source, TypeInstanceId, Checked>,
    pub cast_to: TypeInstanceId,
}

impl<'source> TypeErrorDisplay for InvalidCast<'source> {
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
            expr = HIRExprPrinter::new(interner).print(&self.expr),
            type = self.expr.get_type().as_string(type_db),
            cast_type = self.cast_to.as_string(type_db),
        )
    }
}

pub struct BinaryOperatorNotFound {
    pub on_element: RootElementType,
    pub lhs: TypeInstanceId,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for BinaryOperatorNotFound {
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
            operator = operator_str(self.operator),
            lhs_type = self.lhs.as_string(type_db),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct UnaryOperatorNotFound {
    pub on_element: RootElementType,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for UnaryOperatorNotFound {
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
            operator = operator_str(self.operator),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct FieldOrMethodNotFound {
    pub on_element: RootElementType,
    pub object_type: TypeInstanceId,
    pub field_or_method: InternedString,
}

impl TypeErrorDisplay for FieldOrMethodNotFound {
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
            field_or_method = interner.get_string(self.field_or_method).to_string(),
            type_name = self.object_type.as_string(type_db)
        )
    }
}

pub struct InsufficientTypeInformationForArray {
    pub on_element: RootElementType,
}

impl TypeErrorDisplay for InsufficientTypeInformationForArray {
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
    pub on_element: RootElementType,
    pub expected_type: TypeInstanceId,
}

impl TypeErrorDisplay for ArrayExpressionsNotAllTheSameType {
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
            type_name = self.expected_type.as_string(type_db)
        )
    }
}

pub struct IfStatementNotBoolean {
    pub on_element: RootElementType,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for IfStatementNotBoolean {
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
            actual_type = self.actual_type.as_string(type_db)
        )
    }
}

pub struct TypeInferenceFailure {
    pub on_element: RootElementType,
    pub variable: String,
}

impl TypeErrorDisplay for TypeInferenceFailure {
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
            variable = self.variable,
        )
    }
}

pub struct UnexpectedTypeInferenceMismatch<'source> {
    pub on_element: RootElementType,
    pub inferred: TypeInstanceId,
    pub checked: TypeInstanceId,
    pub expr: HIRExpr<'source, TypeInstanceId, Checked>,
}

impl<'source> TypeErrorDisplay for UnexpectedTypeInferenceMismatch<'source> {
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
            expr_str = HIRExprPrinter::new(interner).print(&self.expr),
            inferred_type = self.inferred.as_string(type_db),
            checked_type = self.checked.as_string(type_db)
        )
    }
}

pub struct TypeConstructionFailure {
    pub on_element: RootElementType,
    pub error: TypeConstructionError,
}

impl TypeErrorDisplay for TypeConstructionFailure {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager<'_>,
        interner: &StringInterner,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{on_element}, type construction failed: {variable:#?}",
            on_element = self.on_element.diag_name(interner),
            variable = self.error,
        )
    }
}

pub struct VariableNotFound {
    pub on_element: RootElementType,
    pub variable_name: InternedString,
}

impl TypeErrorDisplay for VariableNotFound {
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

        impl<'errors, 'callargs, 'type_db, 'source, 'interner> Display for TypeErrorPrinter<'errors, 'type_db, 'source, 'interner> {

            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.errors.count() == 0 {
                    return Ok(());
                }
                $(
                    for err in self.errors.$field.iter() {
                        err.fmt_err(self.type_db, self.interner, f)?;
                        write!(f, "\n")?;
                    }
                )*

                return Ok(());
            }
        }


        pub struct TypeErrorPrinter<'errors, 'type_db, 'source, 'interner> {
            pub errors: &'errors TypeErrors<'source>,
            pub type_db: &'type_db TypeInstanceManager<'interner>,
            pub interner: &'interner StringInterner,
        }

        impl<'errors, 'callargs, 'type_db, 'source, 'interner> TypeErrorPrinter<'errors, 'type_db, 'source, 'interner> {
            pub fn new(
                errors: &'errors TypeErrors<'source>,
                type_db: &'type_db TypeInstanceManager<'interner>,
                interner: &'interner StringInterner,
            ) -> TypeErrorPrinter<'errors, 'type_db, 'source, 'interner> {
                TypeErrorPrinter { errors, type_db, interner }
            }
        }

    }

}

make_type_errors!(
    assign_mismatches: Vec<TypeMismatch<AssignContext>>,
    return_type_mismatches: Vec<TypeMismatch<ReturnTypeContext>>,
    function_call_mismatches: Vec<TypeMismatch<FunctionCallContext>>,
    function_call_argument_count: Vec<FunctionCallArgumentCountMismatch>,
    call_non_callable: Vec<CallToNonCallableType>,
    type_not_found: Vec<TypeNotFound>,
    variable_not_found: Vec<VariableNotFound>,
    unexpected_types: Vec<UnexpectedTypeFound>,
    binary_op_not_found: Vec<BinaryOperatorNotFound>,
    unary_op_not_found: Vec<UnaryOperatorNotFound>,
    field_or_method_not_found: Vec<FieldOrMethodNotFound>,
    insufficient_array_type_info: Vec<InsufficientTypeInformationForArray>,
    array_expressions_not_all_the_same_type: Vec<ArrayExpressionsNotAllTheSameType>,
    if_statement_unexpected_type: Vec<IfStatementNotBoolean>,
    type_inference_failure: Vec<TypeInferenceFailure>,
    type_construction_failure: Vec<TypeConstructionFailure>,
    out_of_bounds: Vec<OutOfTypeBounds<'source>>,
    invalid_casts: Vec<InvalidCast<'source>>,
    type_inference_check_mismatch: Vec<UnexpectedTypeInferenceMismatch<'source>>
);
