use crate::types::diagnostics::RootElementType;
#[cfg(test)]
use crate::{
    interner::{InternedString, StringInterner},
    semantic::hir_printer::HIRPrinter,
};

#[cfg(test)]
use crate::{
    semantic::{hir::MetaTable, typer::Typer},
    types::{diagnostics::TypeErrorPrinter, type_constructor_db::TypeConstructorDatabase},
};

#[cfg(test)]
use super::context::test_utils;

#[cfg(test)]
use super::hir::ast_globals_to_hir;

#[cfg(test)]
use super::context::Source;

#[cfg(test)]
use super::hir::HIRRoot;
#[cfg(test)]
use super::hir::{MonoType, NodeIndex, TypeTable};

#[cfg(test)]
use super::hir::TypeVariable;
#[cfg(test)]
use crate::types::diagnostics::TypeErrors;
#[cfg(test)]
use std::collections::HashMap;

#[cfg(test)]
use super::typer::UnificationErrorStack;
#[cfg(test)]
use super::typer::UnificationMismatchingTypes;

#[cfg(test)]
fn check(
    ty: &TypeConstructorDatabase,
    unification_result: &Result<HashMap<TypeVariable, MonoType>, UnificationErrorStack>,
    expected_left: &str,
    expected_right: &str,
) {
    use core::panic;

    match unification_result {
        Ok(uni) => {
            let v = uni
                .get(&TypeVariable(expected_left.into()))
                .expect("Expected left type not found");
            assert!(v.print_name(ty) == expected_right);
        }
        Err(errors) => {
            panic!("Unification failed: {:#?}", errors);
        }
    }
}

#[test]
fn check_simple_variables_unify() {
    let mut db = TypeConstructorDatabase::new();
    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::variable("'t0"),
        &MonoType::variable("'t1"),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t0", "'t1");
}

#[test]
fn check_substitute_skolem_does_not_turn_skolem_into_variable() {
    let mut db = TypeConstructorDatabase::new();
    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::skolem("'t0"),
        &MonoType::variable("'t1"),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t1", "'t0");
}

#[test]
fn check_ptrs_of_different_skolem_types_do_not_unify() {
    let mut db = TypeConstructorDatabase::new();
    let ptr = db.common_types.ptr;
    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::Application(ptr, vec![MonoType::skolem("T")]),
        &MonoType::Application(ptr, vec![MonoType::skolem("U")]),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    assert!(unification_result.is_err());
}

#[test]
fn unify_ptr_i32_with_ptr_variable() {
    let mut db = TypeConstructorDatabase::new();
    let ctypes = db.common_types.clone();

    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::Application(ctypes.ptr, vec![MonoType::variable("'t0")]),
        &MonoType::Application(ctypes.ptr, vec![MonoType::simple(ctypes.i32)]),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t0", "i32");
}

//During monomorphization, we can encounter a scenario like this:
/*
def test<T>(x: ptr<T>) -> ptr<T>:
    x

but suppose we pass it a ptr<ptr<i32>>:

def main():
    x = 1
    test(&&x)

When we monomorphize test, the registered call is:
test: (ptr<ptr<i32>>) -> ptr<ptr<i32>>

So what is the type of T in this case? It should be ptr<i32>.

We do this by unifying the type of the argument with the type of the parameter:

       type of parameter x: ptr<T>
        v
unify(ptr<T>, ptr<ptr<i32>>) -> { T = ptr<i32> }
                ^
                type of argument to x: ptr<ptr<i32>>

One detail is that in ptr<T>, T is a skolem type which is rigid.
We have to turn it into a variable to allow it to be unified with the argument type.
*/
#[test]
fn you_can_use_unification_to_extract_the_inner_type_of_generics() {
    let mut db = TypeConstructorDatabase::new();
    let ctypes = db.common_types.clone();

    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::Application(ctypes.ptr, vec![MonoType::variable("'t0")]),
        &MonoType::Application(
            ctypes.ptr,
            vec![MonoType::Application(
                ctypes.ptr,
                vec![MonoType::simple(ctypes.i32)],
            )],
        ),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t0", "ptr<i32>");
}

#[test]
fn you_can_use_unification_to_extract_the_inner_type_of_generics_different_order() {
    let mut db = TypeConstructorDatabase::new();
    let ctypes = db.common_types.clone();

    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::Application(
            ctypes.ptr,
            vec![MonoType::Application(
                ctypes.ptr,
                vec![MonoType::simple(ctypes.i32)],
            )],
        ),
        &MonoType::Application(ctypes.ptr, vec![MonoType::variable("'t0")]),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t0", "ptr<i32>");
}

#[test]
fn you_cannot_use_unification_with_skolems_to_unpack_ptr_types() {
    let mut db = TypeConstructorDatabase::new();
    let ctypes = db.common_types.clone();

    let mut sut = Typer::new(&mut db);

    let unification_result = sut.unify(
        &MonoType::Application(ctypes.ptr, vec![MonoType::skolem("T")]),
        &MonoType::Application(
            ctypes.ptr,
            vec![MonoType::Application(
                ctypes.ptr,
                vec![MonoType::simple(ctypes.i32)],
            )],
        ),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );
    assert!(unification_result.is_err());
}

#[test]
fn you_can_use_unification_to_extract_multiple_types_at_once() {
    let mut db = TypeConstructorDatabase::new();
    let ctypes = db.common_types.clone();

    let mut sut = Typer::new(&mut db);

    //Note: this type does not actually exist.

    let unification_result = sut.unify(
        &MonoType::Application(
            ctypes.ptr,
            vec![MonoType::variable("'t0"), MonoType::variable("'t1")],
        ),
        &MonoType::Application(
            ctypes.ptr,
            vec![
                MonoType::Application(ctypes.ptr, vec![MonoType::simple(ctypes.i32)]),
                MonoType::Application(ctypes.ptr, vec![MonoType::simple(ctypes.f32)]),
            ],
        ),
        NodeIndex::none(),
        &RootElementType::Function("test".into()),
    );

    check(&db, &unification_result, "'t0", "ptr<i32>");
    check(&db, &unification_result, "'t1", "ptr<f32>");
}
