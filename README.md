Donkey lang
==========

Donkey is a Python-like statically typed programming language. It is a continuation of the work done in [my previous language called Horse](https://github.com/ricardopieper/horse) which was basically a Python clone, following Python's model almost closely. Donkey will not try to be Python, but rather a C-like Python with touches of Rust.

This is more of a study on how compilers work rather than a serious programming language. This study is guided by personal interest in compiler subjects, rather than the necessities of a real production compiler. Obviously, don't use it in production.

Compiler Stages
---------------

Donkey has the following compiler stages:

 - Parsing: Converts textual source code to AST. It's basically python, but typed.
 - AST -> HIR: Lowers the AST to a format called HIR (High-level Intermediate Representation) that still retains most of the program's structure, but in a lower-level form. For instance, index operators (like `arr[i]`) are lowered to a method call (like `arr.__index__(i)`, like Python). For loops in collections would also be lowered to while loops.
 - Type Inference: Fills the HIR with type information. This also detects non-existing variables.
 - HIR -> MIR: Lowers the HIR to a format called MIR (Middle-level Intermediate Representation) that is composed of basic blocks, branches, return and gotos. Loops and ifs are lowered to Gotos and branches. Possible future language features, such as switch cases or pattern matching, would be automatically lowered to HIR IF statements, which then would be lowered to MIR, so that the code generator/interpreter does not need to be changed.
 - Type Checking: Checks the types of the program (call arguments, inference of array types, variable assignments, etc). Also checks that all paths in a method return the same type, and that all paths return.
 - Backend: This depends on the backend, which could be many.

Backends
--------

So far, Donkey has 3 very incomplete execution backends: 

 - donkey_vm: A bytecode and VM built from scratch to study how to make interpreters. It's not particularly fast. This repo includes an assembler for this bytecode too. So far it's the most complete, being able to call functions recursively.
 - lambda: A strategy of compiling from the MIR representation to closures. No bytecode, just closure functions generated from the code. I don't know how it would behave in large codebases, but it's a peculiar strategy that runs slightly faster than bytecode interpretation.
 - llvm: Uses inkwell, a high-level safe library to generate code with LLVM. This produces optimized code at the level of C, C++ and Rust.

Why named Donkey?
-----------------

Initially I wanted to name it pony, I created this repo with the name `pony-lang` but then I discovered there is already a programming language, a quite interesting one, called pony. I wonder if adoption of Pony is being held back by its unusual name. I wanted this name because my previous attempt at writing an interpreter was named Horse (whose origin was an internal joke, and is an animal name like python). 

After discovering pony already existed, I asked a friend of mine to give me an animal name that's funny enough so that it turns off people from using it in production. In the end he offered me the name Donkey and it kinda makes sense: Donkeys are smaller and more lightweight than horses, and this language will be more lightweight at runtime than horse, I hope. 


Standard library
----------------

In Horse, the stdlib was being written in Horse, which was cool. Maybe I'll do the same here. Types like `str` could be implemented using lower-level keywords yet to be designed.

Struct declarations
-------------------

Structs will be super simple:

```
struct SomeStruct:
    field1: i32
    field2: i64
    field3: str

struct SomeStructGeneric<T>:
    field1: i32
    field2: i64
    field3: T

```

Implementations
---------------

Impls look like this:

```
impl SomeStruct:

    def some_method(self, param: u32) -> u32:
        ...

```

In this case, self is ptr<SomeStruct>, automatically inferred.


Pending features:
 - low-level memory management functions
 - array methods
 - string methods (based on libc, can be done in stdlib)
 - import statements, prelude
