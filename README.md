Donkey lang
==========

Donkey is a Python-like statically typed programming language. It is a continuation of the work done in [my previous language called Horse](https://github.com/ricardopieper/horse) which was basically a Python clone, following Python's model almost closely. Donkey will not try to be Python, but rather a C-like Python with touches of Rust.

This is more of a study on how compilers work rather than a serious programming language. This study is guided by personal interest in compiler subjects, rather than the necessities of a real production compiler. Obviously, don't use it in production. That's why the user experience is not great, errors are not reported where they happen, and the compiler offers no diagnostics. This is an interesting topic that might be revisited later.

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

In Horse, the stdlib was being written in Horse, which was cool. Maybe I'll do the same here. Types like `str` could be implemented using lower-level keywords yet to be desgiend.

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

Traits and implementations
--------------------------

This could be the implementation of a jet fighter game, like Ace combat:


```
trait Aircraft:
    def name() -> str
    def throttle_up(rate: f32)
    def throttle_down(rate: f32)

trait FighterJet: Aircraft:
    def lock_missile(radar_lock_id: i32) -> bool

struct Su27Flanker:
    max_speed: f32
    current_speed: f32
    current_acceleration: f32
    current_missiles: i32
    radar: Radar

struct Boeing777:
    max_speed: f32
    current_speed: f32
    current_acceleration: f32
    passengers: u32

impl Su27Flanker:

    def init(radar: Radar) -> Su27Flanker:
    
        #Dictionary initialization, compiler will complain if you don't pass anything. 
        #You can only pass None (nullptr) to traits.
        return Su27Flanker(
            max_speed = 2000,
            current_speed = 0
            current_acceleration = 0
            current_missiles = 96
            radar = radar)

    def reduce_missile(): 
        self.current_missiles = self.current_missiles - 1;

impl Aircraft for Su27Flanker:

    def name(self) -> str:
        return "Su-27 Flanker" 

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration + rate

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration - rate

impl FighterJet for Su27Flanker

    def lock_missile(self, radar_target_id: i32) -> bool:
        
        #Here, type inference will be used, suppose get_entity_id returns i32
        entity_id = self.radar.get_entity_id(radar_target_id)

        #Explicitly define type, suppose GetEntity returns an Optional<Aircraft>.
        target: Optional<Aircraft> = EntityManager.GetEntity(entity_id)
  
        #We will allow static calls globals
        SoundManager.Play("assets/sounds/sidewinder-growl-tone.wav")
       
        #Match on trait types
        if target.is_some() && target.get() is FighterJet: # cannot check for concrete type here.
            UIManager.ChangeRadarLockColor(radar_target_id, 255, 0, 0)
        else:
            UIManager.ShowMessage("Warning: Civilian aircraft locked on radar, do not fire")
            UIManager.ChangeRadarLockColor(radar_target_id, 255, 255, 0)

impl Aircraft for Boeing777:

    def name(self) -> str:
        return "Boeing 777" 

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration + rate

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration - rate


```

Random notes / Idea dump
------------

How are we going to do dynamic dispatch and be able to check if a value implements a type?

For every interface impl we will generate a vtable for that type, and use fat pointers. When we need to pass a value using a trait object, we will pack the data into a fat pointer containing a pointer to the data, and a pointer to the required interface.

For instance, a method receiving an Aircraft would actually receive:

    -   Pointer to the instance data, that could be either stack or heap allocated
    -   Pointer to the vtable

If we pass a `Su27Flanker` instance, we will pass the pointer to it, and the pointer to the `Su27Flanker`'s specific `Aircraft` vtable pointer. The compiler will know the specific address to pass, and the code will know the offsets of the functions to be called. Notice that in this case the method won't be able to call the `init` function, the typechecker will prevent it.


To generate the code for the Su27Flanker methods, first we find all implementations and generate code for them separately, but try to put them as close as possible, so that we don't have too long jumps. Code implemented separately that calls onto the base might take a longer jump, but that's ok.

If we just receive the struct directly, then the compiler just won't resort to the vtable, it will resolve the call at compile time.

The `is` keyword is only able to check if a type has a trait by checking if has a vtable for 
that type. Casting to the type should be possible but syntax is yet to be defined. I like Jai's syntax for casting, where the cast seems to be an unary operator, like `cast(u8) expr`. Casting to a derived type is not allowed, as that would need to keep type information on the executable itself, and I don't want that for now.


Pending features:

 - low-level memory management functions
 - sizeof<T> function or operator (or T.size() or something)
 - array methods
 - string methods (based on libc, can be done in stdlib)
 - import statements, prelude