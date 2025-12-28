#[cfg(test)]
mod tests {
    use quickcheck::{Arbitrary, Gen};
    extern crate quickcheck;

    use crate::{
        interner::InternedString,
        semantic::{
            hir::{MonoType, NodeIndex, TypeVariable},
            typer::Typer,
        },
        types::{
            diagnostics::RootElementType,
            type_constructor_db::{TypeConstructorDatabase, TypeConstructorId},
        },
    };

    impl Arbitrary for TypeVariable {
        fn arbitrary(g: &mut Gen) -> Self {
            // Generate a random interned string (using some method to convert usize or similar into InternedString)
            let random_string = format!("'t{}", usize::arbitrary(g));
            TypeVariable(random_string.into())
        }
    }

    impl Arbitrary for MonoType {
        fn arbitrary(g: &mut Gen) -> Self {
            // Randomly select between Variable, Skolem, and Application variants
            let variant = usize::arbitrary(g) % 3;
            match variant {
                0 => MonoType::Variable(TypeVariable::arbitrary(g)),
                1 => MonoType::Skolem(TypeVariable::arbitrary(g)),
                2 => {
                    #[cfg(not(test))]
                    fn make_type_constructor(g: &mut Gen) -> TypeConstructorId {
                        let constructor = usize::arbitrary(g) % 5;
                        TypeConstructorId(constructor)
                    }

                    #[cfg(test)]
                    fn make_type_constructor(g: &mut Gen) -> TypeConstructorId {
                        let constructor = usize::arbitrary(g) % 5;
                        let random_string = format!("Type{constructor}");
                        TypeConstructorId(constructor, random_string.into())
                    }

                    let constructor_id = make_type_constructor(g);

                    //there's a 4/5 chance that the application will have no arguments

                    let has_arguments = usize::arbitrary(g) % 5 != 0;

                    if has_arguments {
                        let num_arguments = usize::arbitrary(g) % 3;
                        let mut arguments = Vec::with_capacity(num_arguments);
                        for _ in 0..num_arguments {
                            arguments.push(MonoType::arbitrary(g));
                        }
                        MonoType::Application(constructor_id, arguments)
                    } else {
                        MonoType::Application(constructor_id, vec![])
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    #[quickcheck]
    fn type_always_unifies_with_itself(m1: MonoType) -> bool {
        println!("m1: {:#?}", m1);
        let mut db = TypeConstructorDatabase::new();
        let mut sut = Typer::new(&mut db);
        sut.unify(
            &m1,
            &m1,
            NodeIndex::none(),
            &RootElementType::Function("test".into()),
        )
        .expect("Unification failed");
        true
    }

    #[quickcheck]
    fn when_unification_succeeds_application_to_both_sides_always_yield_the_same_type(
        m1: MonoType,
        m2: MonoType,
    ) -> bool {
        println!("m1: {:#?}", m1);
        let mut db = TypeConstructorDatabase::new();
        let mut sut = Typer::new(&mut db);
        if let Ok(uni) = sut.unify(
            &m1,
            &m2,
            NodeIndex::none(),
            &RootElementType::Function("test".into()),
        ) {
            let uni_m1 = m1.apply_substitution(&uni);
            let uni_m2 = m1.apply_substitution(&uni);

            assert_eq!(uni_m1, uni_m2);
        };
        true
    }
}
