New Type Inference Algorithm
============================


The file `type_inference.rs` right now contains 1.6k lines of code, and after I got away from this project for almost 10 months, I can't understand it anymore.

It's too complex. At every point, I have to handle the difference between a simple type like `i32`, `str`, etc and a composite, generic type
such as `List<i32>`, `ptr<List<i32>>`, etc. Not only that, I need to handle the difference between fully resolving and constructing a type in the first try,
and having some type parameters still "unresolved". Some of these types will only be fully resolved after monomorphization.

How about we fully embrace the possibility of types not being fully resolved? What if everything was potentially polymorphic? It kinda already is...

This is the end of this enum:

```rust
pub enum TypeInferenceResult {
    //Fully resolved
    Monomorphic(TypeInstanceId),
    //Partially resolved, i.e. a function with some arguments
    Polymorphic(TypeConstructParams),
}
```

Now, everything will be this TypeConstructParams:

```rust
pub enum TypeConstructParams {
    Given(TypeConstructorId),
    Generic(TypeParameter),
    FunctionSignature(FunctionSignature<TypeConstructParams>),
    Parameterized(Box<TypeConstructParams>, Vec<TypeConstructParams>),
}
```

But wait, why do we have `FunctionSignature`? If we have FunctionSignature, shouldn't we also have `Array`, `Pointer`, etc? No.
If arrays, pointers, etc all of these generic types are just type constructors, we can just use `Parameterized` for them.

This is what we're going to do: There will be specialized functions to *register* a new function type constructor according to
the function signature. But the returned type will just be a `TypeConstructParams::Parameterized`. type.

```rust
pub enum TypeConstructParams {
    Given(TypeConstructorId),
    Generic(TypeParameter),
    Parameterized(Box<TypeConstructParams>, Vec<TypeConstructParams>),
}
```

Now, why `Parameterized` has a `Box<TypeConstructParams>`? Would it make sense to have a T1<T2>? Hmm I don't know.
Maybe a List<T1><T2>? Well that doesn't seem to make much sense, this should be a simple type system and this is getting too complex.

So maybe we should have this, where parameterized types always have a TypeConstructorId as the base:

```rust
pub enum TypeConstructParams {
    Given(TypeConstructorId),
    Generic(TypeParameter),
    Parameterized(TypeConstructorId, Vec<TypeConstructParams>),
}
```

We can simplify it one more time, by making `Given` a `Parameterized` with no parameters. If it's a simple type, it's a parameterized type with 0 parameters.

```rust
pub enum TypeConstructParams {
    Parameterized(TypeConstructorId, Vec<TypeConstructParams>),
    Generic(TypeParameter),
}
```

Since the monomorphizer is currently able to deal with all of this, it should be lots of busy work but overall possible.

`TypeInferenceResult`
---------------------

There is an issue. The `TypeInferenceResult` enum is used in a lot of places, including the environment (i.e. which variables exist, what functions are in scope),
so there isn't a really good way to change it. I need to go with a hammer and destroy everything that uses it.

The main issue really is the environment. Maybe the environment should change according to the stage of the compiler?


Result of simplification
------------------------

About 300 lines got removed from the type inference algorithm. It didn't get all that much simpler to understand, but some polymorphic method call code that was completely
impossible to understand is now gone.

As for the environment, it's still an issue. After the first pass to get globals, I get all definitions that are not actually polymorphic and do a naive type construction,
i,e. functions that have no type parameters are easy to construct. The environment (`NameRegistry`) really does change types according to the stage of the compiler,
as it is generic over the type of the types it contains.

The monomorphizer isn't terribly simpler, but at least it's always expeecting only the `TypeConstructParams` type, instead of also expecting a TypeInstanceId.

Overall, ~300 lines of code less.

Complete rewrite
================

After that, it became clear that it wasn't just the data structures that weren't ideal.
The whole algorithm was just wrong.

I decided I couldn't just wing it anymore. Instead I decided to look at the literature and implemented
a subset of the Hindley-Milner type inference algorithm.

This is a work in progress, but it's promising.

Another change is that now I'm using a completely separate data structure for typing the HIR. Instead
of storing the types in the HIR, I'm storing "type indices" in the tree, and the actual types are stored
in a table separately. This way I can apply substitution to the types without having to modify the HIR.

Yes it relies on functions performing "side effects", functional people hate it ¯\_(ツ)_/¯.
