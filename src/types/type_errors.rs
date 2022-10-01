use std::fmt::Display;

use crate::{
    ast::{lexer::Operator, parser::Expr},
    semantic::{hir::{HIRType, HIRExpr, NotChecked}, hir_printer::{operator_str, expr_str}, type_checker::FunctionName},
};

use super::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

pub trait TypeErrorDisplay {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

pub struct TypeMismatch<TContext> {
    pub on_function: String,
    pub context: TContext,
    pub expected: TypeInstanceId,
    pub actual: TypeInstanceId,
}

pub struct AssignContext {
    pub target_variable_name: String,
}

impl TypeErrorDisplay for TypeMismatch<AssignContext> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let var_type_str = self.expected.as_string(type_db);
        let expr_type_str = self.actual.as_string(type_db);

        write!(f,  "Assigned type mismatch: In function {on_function}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_function = self.on_function,
            var = self.context.target_variable_name
        )
    }
}

pub struct ReturnTypeContext();

impl TypeErrorDisplay for TypeMismatch<ReturnTypeContext> {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        write!(f,  "Return type mismatch: Function {on_function} returns {return_type_name} but expression returns {expr_return_type_name}",
            on_function = self.on_function,
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
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        match &self.context.called_function_name {
            FunctionName::Function(function_name) => {
                write!(f,  "Function argument type mismatch: In function {on_function}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_function = self.on_function,
                    function_called = function_name,
                    position = self.context.argument_position
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Function argument type mismatch: In function {on_function}, on index operator, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_function = self.on_function,
                    position = self.context.argument_position
                )
            }
            FunctionName::Method {
                function_name,
                type_name,
            } =>  
                write!(f,  "Function argument type mismatch: In function {on_function}, on call to method {function_name} of {type_name}, parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
                    on_function = self.on_function,
                    position = self.context.argument_position
                )
        }
    }
}

pub struct FunctionCallArgumentCountMismatch {
    pub on_function: String,
    pub called_function_name: FunctionName,
    pub expected_count: usize,
    pub passed_count: usize,
}

impl TypeErrorDisplay for FunctionCallArgumentCountMismatch {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &self.called_function_name {
            FunctionName::Function(call_name) => {
                write!(f,  "Argument count mismatch: In function {on_function}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
                    on_function = self.on_function,
                    function_called = call_name,
                    expected_args = self.expected_count,
                    passed_args = self.passed_count,
                )
            }
            FunctionName::IndexAccess => {
                write!(f,  "Argument count mismatch: In function {on_function}, index operator expects {expected_args} arguments, but {passed_args} were passed",
                    on_function = self.on_function,
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
    pub on_function: String,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for CallToNonCallableType {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, call to non-callable type {non_callable_type_name}",
            on_function = self.on_function,
            non_callable_type_name = self.actual_type.as_string(type_db),
        )
    }
}

pub struct TypeNotFound {
    pub on_function: String,
    pub type_name: HIRType,
}

impl TypeErrorDisplay for TypeNotFound {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, type not found: {type_not_found}",
            on_function = self.on_function,
            type_not_found = self.type_name,
        )
    }
}

pub struct UnexpectedTypeFound {
    pub on_function: String,
    pub type_def: TypeInstanceId,
}

impl TypeErrorDisplay for UnexpectedTypeFound {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, unexpected type found in expression: {unexpected_type}",
            on_function = self.on_function,
            unexpected_type = self.type_def.as_string(type_db),
        )
    }
}


pub struct OutOfTypeBounds {
    pub on_function: String,
    pub expr: HIRExpr<TypeInstanceId>
}

impl TypeErrorDisplay for OutOfTypeBounds {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, value {expr} is out of bounds for type {type}. You can try extracting this value to a different variable and assign a larger type.",
            on_function = self.on_function,
            expr = expr_str(&self.expr),
            type = self.expr.get_type().as_string(type_db),
        )
    }
}


pub struct InvalidCast {
    pub on_function: String,
    pub expr: HIRExpr<TypeInstanceId>,
    pub cast_to: TypeInstanceId
}

impl TypeErrorDisplay for InvalidCast {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, value {expr} of type {type} cannot be casted to {cast_type}",
            on_function = self.on_function,
            expr = expr_str(&self.expr),
            type = self.expr.get_type().as_string(type_db),
            cast_type = self.cast_to.as_string(type_db),
        )
    }
}

pub struct BinaryOperatorNotFound {
    pub on_function: String,
    pub lhs: TypeInstanceId,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for BinaryOperatorNotFound {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, binary operator {operator} not found for types: {lhs_type} {operator} {rhs_type}",
            on_function = self.on_function,
            operator = operator_str(self.operator),
            lhs_type = self.lhs.as_string(type_db),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct UnaryOperatorNotFound {
    pub on_function: String,
    pub rhs: TypeInstanceId,
    pub operator: Operator,
}

impl TypeErrorDisplay for UnaryOperatorNotFound {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, unary operator {operator} not found for type: {rhs_type}",
            on_function = self.on_function,
            operator = operator_str(self.operator),
            rhs_type = self.rhs.as_string(type_db)
        )
    }
}

pub struct FieldOrMethodNotFound {
    pub on_function: String,
    pub object_type: TypeInstanceId,
    pub field_or_method: String,
}

impl TypeErrorDisplay for FieldOrMethodNotFound {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, tried to access field/method {field_or_method} on type {type_name} but no such field or method exists.",
            on_function = self.on_function,
            field_or_method =self.field_or_method,
            type_name = self.object_type.as_string(type_db)
        )
    }
}

pub struct InsufficientTypeInformationForArray {
    pub on_function: String,
}

impl TypeErrorDisplay for InsufficientTypeInformationForArray {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, array expression failed type inference: Array has no items, and/or variable declaration has no type declaration or type hint.",
            on_function = self.on_function
        )
    }
}


pub struct ArrayExpressionsNotAllTheSameType {
    pub on_function: String,
    pub expected_type: TypeInstanceId
}

impl TypeErrorDisplay for ArrayExpressionsNotAllTheSameType {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, expresins in array expected to be of type {type_name} but an expression of a different type was found",
            on_function = self.on_function,
            type_name = self.expected_type.as_string(type_db)
        )
    }
}

pub struct IfStatementNotBoolean {
    pub on_function: String,
    pub actual_type: TypeInstanceId,
}

impl TypeErrorDisplay for IfStatementNotBoolean {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, if statement expects boolean expression, but actual type is {actual_type}",
            on_function = self.on_function,
            actual_type = self.actual_type.as_string(type_db)
        )
    }
}

pub struct TypeInferenceFailure {
    pub on_function: String,
    pub variable: String,
}

impl TypeErrorDisplay for TypeInferenceFailure {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, type inference failed for variable {variable}",
            on_function = self.on_function,
            variable = self.variable,
        )
    }
}


pub struct UnexpectedTypeInferenceMismatch {
    pub on_function: String,
    pub inferred: TypeInstanceId,
    pub checked: TypeInstanceId,
    pub expr: HIRExpr<TypeInstanceId, NotChecked>
}

impl TypeErrorDisplay for UnexpectedTypeInferenceMismatch {
    fn fmt_err(
        &self,
        type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "Type inference/check bug: In function {on_function}, type inferred for expression {expr_str} was {inferred_type}, but type checker detected {checked_type}",
            on_function = self.on_function,
            expr_str = expr_str(&self.expr),
            inferred_type = self.inferred.as_string(type_db),
            checked_type = self.checked.as_string(type_db)
        )
    }
}

pub struct TypeConstructionFailure {
    pub on_function: String,
    pub error: TypeConstructionError,
}

impl TypeErrorDisplay for TypeConstructionFailure {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, type construction failed: {variable:#?}",
            on_function = self.on_function,
            variable = self.error,
        )
    }
}

pub struct VariableNotFound {
    pub on_function: String,
    pub variable_name: String,
}

impl TypeErrorDisplay for VariableNotFound {
    fn fmt_err(
        &self,
        _type_db: &TypeInstanceManager,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, variable not found: {variable:#?}",
            on_function = self.on_function,
            variable = self.variable_name,
        )
    }
}

macro_rules! make_type_errors {
    ($($field:ident : $typename:ty), *) => {

        pub struct TypeErrors {
            $(
                pub $field: $typename,
            )*
        }

        impl TypeErrors {
            pub fn new() -> TypeErrors {
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

        impl<'errors, 'callargs, 'type_db> Display for TypeErrorPrinter<'errors, 'type_db> {

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


        pub struct TypeErrorPrinter<'errors, 'type_db> {
            pub errors: &'errors TypeErrors,
            pub type_db: &'type_db TypeInstanceManager,
        }

        impl<'errors, 'callargs, 'type_db> TypeErrorPrinter<'errors, 'type_db> {
            pub fn new(
                errors: &'errors TypeErrors,
                type_db: &'type_db TypeInstanceManager,
            ) -> TypeErrorPrinter<'errors, 'type_db> {
                TypeErrorPrinter { errors, type_db }
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
    out_of_bounds: Vec<OutOfTypeBounds>,
    invalid_casts: Vec<InvalidCast>,
    type_inference_check_mismatch: Vec<UnexpectedTypeInferenceMismatch>
);
