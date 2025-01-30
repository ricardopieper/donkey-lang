use std::collections::HashMap;

use crate::{
    ast::parser::SpannedOperator,
    interner::*,
    semantic::{
        hir::{HIRUserTypeInfo, MethodCall, TypeVariable},
        hir_printer::{self, HIRPrinter},
    },
    types::{
        diagnostics::{
            BinaryOperatorNotFound, FieldNotFound, RootElementType, TypeErrors,
            UnaryOperatorNotFound,
        },
        type_constructor_db::TypeConstructorDatabase,
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};

use super::hir::{
    FunctionCall, HIRExpr, HIRRoot, HIRTypedBoundName, MonoType, NodeIndex, PolyType,
    TypeParameter, TypeTable, HIR,
};

struct MonomorphizationQueueItem {
    polymorphic_root: InternedString,
    positional_type_arguments: Vec<MonoType>,
    //the index in the original vec in polymorphic stage, used to return the result in the same order
    original_index: usize,
}

type CompilerError = ();

pub struct Monomorphizer<'compiler_state> {
    global_definitions: HashMap<InternedString, HIRRoot>,
    queue: Vec<MonomorphizationQueueItem>,
    type_db: &'compiler_state TypeConstructorDatabase,
    result: Vec<(HIRRoot, usize)>,
}

impl<'compiler_state> Monomorphizer<'compiler_state> {
    pub fn new(type_db: &'compiler_state TypeConstructorDatabase) -> Self {
        Self {
            global_definitions: HashMap::new(),
            queue: vec![],
            type_db,
            result: vec![],
        }
    }

    fn enqueue(&mut self, item: MonomorphizationQueueItem) {
        self.queue.push(item);
    }

    pub fn run(&mut self, all_roots: &[HIRRoot]) -> Result<(), ()> {
        for (i, hir_def) in all_roots.into_iter().enumerate() {
            match hir_def {
                HIRRoot::DeclareFunction {
                    ref function_name,
                    ref type_parameters,
                    ..
                } => {
                    println!("Declare function {function_name:?}");
                    if type_parameters.len() == 0 {
                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: *function_name,
                            positional_type_arguments: vec![],
                            original_index: i,
                        });
                    }

                    self.global_definitions
                        .insert(*function_name, hir_def.clone());
                }
                HIRRoot::StructDeclaration {
                    ref struct_name, ..
                } => {
                    self.global_definitions
                        .insert(*struct_name, hir_def.clone());
                }
                _ => {}
            }
        }

        //start consuming queue
        while let Some(queue_item) = self.queue.pop() {
            let result = self.monomorphize(
                queue_item.polymorphic_root,
                queue_item.positional_type_arguments,
                queue_item.original_index,
            )?;
            if let Some(hir) = result {
                self.result.push((hir, queue_item.original_index));
            }
        }
        Ok(())
    }

    pub fn get_result(mut self) -> Vec<HIRRoot> {
        self.result.sort_by(|a, b| a.1.cmp(&b.1));
        let mono_hir = self.result.into_iter().map(|(hir, _)| hir).collect();
        return mono_hir;
    }

    fn monomorphize(
        &mut self,
        polymorphic_root: InternedString,
        positional_type_arguments: Vec<MonoType>,
        original_index: usize,
    ) -> Result<Option<HIRRoot>, ()> {
        log!(
            "Monomorphizing the following element {polymorphic_root} {positional_type_arguments:#?}:"
        );

        log!(
            "Global definitions: {:#?}, {polymorphic_root:#?}",
            self.global_definitions.keys()
        );
        let hir = self
            .global_definitions
            .get(&polymorphic_root)
            .unwrap()
            .clone();

        let printed = HIRPrinter::new(false, &self.type_db).print_hir(&[hir.clone()]);

        log!("{printed}");

        let HIRRoot::DeclareFunction {
            function_name,
            type_parameters,
            parameters,
            mut body,
            return_type,
            is_intrinsic,
            is_varargs,
            is_external,
            type_table,
            ..
        } = hir
        else {
            return Ok(None);
        };

        let mut new_type_table = type_table.clone();

        let substitution = type_parameters
            .clone()
            .into_iter()
            .map(|x| TypeVariable(x.0))
            .zip(positional_type_arguments.clone().into_iter())
            .collect::<HashMap<TypeVariable, MonoType>>();

        log!("{polymorphic_root}: Applying substitution {substitution:#?}");
        new_type_table.apply_function_wide_substitution(&substitution);

        self.find_function_calls_hir(
            function_name,
            &mut body,
            original_index,
            &mut new_type_table,
        )?;

        if positional_type_arguments.len() > 0 {
            //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
            let new_function_name_suffix = positional_type_arguments
                .iter()
                .map(|x| x.print_name(&self.type_db))
                .collect::<Vec<_>>()
                .join(",");
            let old_function_name = function_name.to_string();
            let new_function_name = format!("{}[{}]", old_function_name, new_function_name_suffix);
            let interned_function_name = InternedString::new(&new_function_name);

            Ok(Some(HIRRoot::DeclareFunction {
                function_name: interned_function_name,
                type_parameters: vec![],
                parameters: parameters.clone(),
                body: body.clone(),
                return_type: return_type.clone(),
                is_intrinsic,
                is_varargs,
                is_external,
                method_of: None,
                type_table: new_type_table,
                has_been_monomorphized: true,
            }))
        } else {
            Ok(Some(HIRRoot::DeclareFunction {
                function_name,
                type_parameters: vec![],
                parameters: parameters.clone(),
                body: body.clone(),
                return_type: return_type.clone(),
                type_table: new_type_table,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of: None,
                has_been_monomorphized: true,
            }))
        }
    }

    fn find_function_calls_hir(
        &mut self,
        function_name: InternedString,
        body: &mut [HIR],
        original_index: usize,
        type_table: &mut TypeTable,
    ) -> Result<(), ()> {
        log!("Finding calls in body of {}", function_name.to_string());

        for hir_node in body.iter_mut() {
            match hir_node {
                HIR::Assign {
                    path,
                    expression,
                    location,
                } => {
                    log!("Monomorphizing assignment");
                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        path,
                        original_index,
                        type_table,
                    )?;
                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::Declare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    log!("Monomorphizing declaration of variable {}", var);

                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::SyntheticDeclare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::FunctionCall(fcall, location) => {
                    log!("Monomorphizing function call (HIR, not expr)");
                    self.monomorphize_fcall(
                        RootElementType::Function(function_name),
                        fcall,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::MethodCall(mcall, location) => {
                    log!("Ignoring monomorphizatio of method call (HIR, not expr) because not done yet");
                    /*let mono_mcall =
                        self.monomorphize_mcall(on_function, original_index, &mcall)?;
                    new_body.push(HIR::MethodCall(mono_mcall, location));*/
                }
                HIR::If(condition_expr, true_branch, false_branch, meta) => {
                    log!("Monomorphizing if statement");
                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        condition_expr,
                        original_index,
                        type_table,
                    )?;

                    self.find_function_calls_hir(
                        function_name,
                        true_branch,
                        original_index,
                        type_table,
                    )?;
                    self.find_function_calls_hir(
                        function_name,
                        false_branch,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::While(condition, body, meta) => {
                    log!("Monomorphizing while statement");
                    let mono_condition = self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        condition,
                        original_index,
                        type_table,
                    )?;
                    let mono_body = self.find_function_calls_hir(
                        function_name,
                        body,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::Return(expr, meta) => {
                    log!("Monomorphizing return statement");
                    self.find_function_calls_exprs(
                        RootElementType::Function(function_name),
                        expr,
                        original_index,
                        type_table,
                    )?;
                    log!("Monomorphizing return statement OK");
                }
                HIR::EmptyReturn(meta) => {}
            }
        }

        Ok(())
    }

    fn monomorphize_fcall(
        &mut self,
        on_function: RootElementType,
        call: &mut FunctionCall,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), ()> {
        let FunctionCall {
            function,
            //the types used in the call, i,e. if the user typed `foo<i32>(...)` then this would be [i32]
            //notice that the function can be called with foo()... but its definition might still be generic.
            //i.e. the compiler can infer the type arguments automatically.
            type_args,
            return_type,
            ..
        } = call;

        log!("Function returns type {return_type:#?}, Function data: {function:#?}");

        let positional_type_args = type_args
            .iter()
            .map(|x| type_table[x.resolved_type].mono.clone())
            .collect::<Vec<_>>();

        if positional_type_args.is_empty() {
            return Ok(());
        }

        match function {
            HIRExpr::Variable(name, _meta, ty) => {
                log!(
                    "monomorphize fcall {name} on function {func}, variable ty = {typ:#?}",
                    func = on_function.get_name(),
                    name = name,
                    typ = ty.print_name(type_table, &self.type_db)
                );

                let type_function = &type_table[ty];
                let type_function_mono = &type_function.mono;

                match type_function_mono {
                    MonoType::Application(_, args) => {
                        //add to the queue so that the function gets monomorphized with the passed type args
                        log!(
                            "Enqueueing monomorphization of {name} with {args:#?}",
                            args = args
                        );

                        //generate the new function name

                        let new_function_name_suffix = positional_type_args
                            .iter()
                            .map(|x| x.print_name(&self.type_db))
                            .collect::<Vec<_>>()
                            .join(",");

                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: *name,
                            positional_type_arguments: positional_type_args.clone(),
                            original_index,
                        });
                        let old_function_name = *name;
                        let new_function_name =
                            format!("{}[{}]", old_function_name, new_function_name_suffix);
                        let interned_function_name = InternedString::new(&new_function_name);

                        *name = interned_function_name;
                        //remove quantifiers from call
                        call.type_args = vec![];
                    }
                    _ => {}
                }
            }
            _ => {}
        };

        Ok(())
    }

    //returns a new HIRExpr with all polymorphic calls replaced with monomorphic calls
    fn find_function_calls_exprs(
        &mut self,
        on_function: RootElementType,
        expr: &mut HIRExpr,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match expr {
            HIRExpr::Cast(expr, user_type, poly_ty, meta) => {
                self.find_function_calls_exprs(on_function, expr, original_index, type_table)?
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, meta, ty) => {
                log!("Monomorphizing binop");
                self.find_function_calls_exprs(on_function, lhs, original_index, type_table)?;
                self.find_function_calls_exprs(on_function, rhs, original_index, type_table)?;
            }
            HIRExpr::MethodCall(mcall, node) => {
                //@TODO method calls also need to be monomorphized
                self.find_function_calls_exprs(
                    on_function,
                    &mut mcall.object,
                    original_index,
                    type_table,
                )?;
                for arg in mcall.args.iter_mut() {
                    self.find_function_calls_exprs(on_function, arg, original_index, type_table)?;
                }
            }
            HIRExpr::FunctionCall(call, node) => {
                log!("Monomorphizing function call {:#?}", call);

                self.monomorphize_fcall(on_function, call, original_index, type_table)?;
            }
            HIRExpr::StructInstantiate(name, hir_type, ty, meta) => {
                log!("Monomorphizing struct instantiation");
                /*Ok(HIRExpr::StructInstantiate(
                    *name,
                    hir_type.clone(),
                    self.construct_type(ty, vec![], typearg_map),
                    meta,
                ))*/
            }
            HIRExpr::Deref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");

                self.find_function_calls_exprs(
                    on_function,
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::Ref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");

                self.find_function_calls_exprs(
                    on_function,
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::UnaryExpression(op, expr, location, ty) => {
                log!("Monomorphizing unary exp");
                let mono_rhs =
                    self.find_function_calls_exprs(on_function, expr, original_index, type_table)?;
            }
            HIRExpr::MemberAccess(obj, member, ..) => {
                self.find_function_calls_exprs(on_function, obj, original_index, type_table)?;
            }
            HIRExpr::Array(items, expr_type, meta) => {
                log!("Monomorphizing array");
                for item in items {
                    self.find_function_calls_exprs(on_function, item, original_index, type_table)?;
                }
            }

            _ => {}
        }
        return Ok(());
    }
}
