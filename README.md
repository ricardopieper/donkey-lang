Donkey lang
==========

Donkey is a Python-like statically typed programming language. It is a continuation of the work done in [my previous language called Horse](https://github.com/ricardopieper/horse) which was basically a Python clone, following Python's model almost closely. Donkey will not try to be Python, but rather a C-like Python with touches of Rust.

This is for study only.

Compiler Stages
---------------

Donkey has the following compiler stages:

 - Parsing (including lexer & parser)
 - HIR (High-level Intermediate Language): AST is lowered to HIR which is a simpler and more abstract form of the language. Array access syntax, for instance, is reduced to a method call.  
 - Typer: Infers types for every expression. Uses a type unification algorithm and a touch of Hindley-Milner.
 - Monomorphization: All generic HIR definitions are essentially copied and modified to work with the specific types it was used with. For instance, `List<T>` becomes `List[i32]`, `List[str]`, etc.
 - Uniformizer: After monomorphization, some type definitions still reference the generic, non-monomorphic versions. This makes it so that no generic types are referred to anymore.
 - Typer: This language uses ad-hoc polymorphism, like C++ typename templates. We need to re-check after monomorphization if the code in fact makes sense. You could be calling a method on a type that doesn't contain that method, because we only know the method exists after monomorphization.
 - Uniformizer: Running the typer again might cause some redefinitions, which are safe but they would refer to the generic types instead of monomorphized ones.
 - MIR (Middle-level Intermediate Language): Lowers all control flow to a more basic version where the CFG is explicit. Ifs and Whiles are reduced to blocks and gotos.
 - Type Check: Runs more type checking. This ensures every path in the function returns the correct type. 
 - Backend: Compiles to LLVM IR, but we used to have a bytecode interpreter and "lambda compiler", similar to https://blog.cloudflare.com/building-fast-interpreters-in-rust/

Why named Donkey?
-----------------

I asked a friend of mine to give me an animal name that's funny enough so that it turns off people from using it in production. I was going to use Pony but it already exists.

Standard library
----------------

The stdlib will be written in the language itself. Currently it is WIP.

Features
-------------------
Structs and impls, similar to Rust. 
Generic types on structs and functions, but not on methods. I intend to add generic types to methods in the future.

```
struct SomeStructGeneric<T>:
    field1: i32
    field2: i64
    field3: T

impl SomeStructGeneric<F>:
    def some_method(self, param: u32) -> F:
       return (*self).field3

def some_func<T>(param: T):
    x = 1
    y: i32 = 0
    return x + y + param

def main():
    my_struct = SomeStructGeneric<str>() //no initializers yet!
    my_struct.field1 = 10
    my_struct.field2 = 999
    my_struct.field3 = "Hello!"
    value = my_struct.some_method(123) //value is inferred as str
    some_func(10)

```

In this case, self is `ptr<SomeStructGeneric<F>>`, automatically inferred.


Pending features:
=================
 - low-level memory management functions
 - array methods
 - string methods (based on libc, can be done in stdlib)
 - import statements, prelude
 - ptr ergonomics (like -> or auto deref)
