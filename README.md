pony
==========

Pony is a python-like, statically typed programming language.

Most of the parsing code was copied from my Horse interpreter at github.com/ricardopieper/horse repository.

Syntax will be based on python but the VM will be far more lightweight. Preferably in the future we won't have a VM. We wont have classes, just structs and impls like Rust. I will try to implement some trait-like system as well, so there will be syntax for that. While the syntax looks like python, it doesn't follow Python's object model or most of the language's built-in type naming. When it does it's coincidental

The features of the language will be implemented purely based on personal interest. Here are the things that, at the moment, I find particularly uninteresting:

  - Exceptions
  - Public/private members (everything will be public)
  - Inheritance

Here are the things that I do find interesting:

  - Metaprogramming, or some level of type information available at compile time (or runtime)
  - Trait system, like Rust
  - Functional programming in general
  - Generics

It doesn't mean I'll implement all of these things, but I would like to implement traits at least.

The goal is to be faster than the Horse interpreter but it won't be safe at all. You might need to 
allocate memory manually, take pointers, defend against None (nullptr), etc. I'll try to add some error detecting in the VM, but not much. The VM internally will probably use a ton of `mem::transmute`, and hope for the type checker to do it's job.

So yes, it's gonna be a statically typed, but interpreted language.

It's called pony because my previous attempt at writing an interpreter was named horse, and for the following reasons:

 - It will have a smaller VM, or someday even no VM at all (maybe i'll write llvm IR and compile that), just like a pony is, in general, smaller than a grown-up, adult, full-featured horse. Jokes aside, it's interesting to have a working interpreter to allow things like compile-time evaluation by just running the code.

 - Ponies stay closer to the ground, or in other words, lower in the air than horses, the same way pony will be a lower level, closer to the metal language. Doesn't mean it will be a low-level language though.

Having said that, it's probably gonna be a much larger effort than Horse.


Standard library
----------------

In Horse, implementing some of the standard library in python itself was a pretty cool thing. Maybe I'll do the same here. Types like `str` could be implemented using lower-level keywords yet to be designed.


Typing
--------

Python has support for explicit, gradual typing. We will be fully typed from the beginning. Every function has to declare the parameters and return types. Every struct will have to declare its field types.

However, we will have some type inference as well. 

Struct declarations
-------------------

Structs will be super simple:

```
struct SomeStruct:
    field1: i32
    field2: i64
    field3: str

# If generics are supported someday:

struct SomeStruct<T>:
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
    def send_missile(radar_lock_id: i32) -> bool

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
        #You can only pass None to traits.
        return Su27Flanker(
            max_speed = 2000,
            current_speed = 0
            current_acceleration = 0
            current_missiles = 96
            radar = radar)

    def reduce_missile(): #methods
        self.current_missiles = self.current_missiles - 1;

impl Aircraft for Su27Flanker:

    #These methods will be available for the value regardless if being accessed through interface (via dyamic dispatch) or statically

    # All methods in a trait impl must receive self first

    def name(self) -> str:
        return "Su-27 Flanker" 

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration + rate

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration - rate

# Traits must be implemented separately
impl FighterJet for Su27Flanker

    def lock_missile(self, radar_target_id: i32) -> bool:
        
        #Here, type inference will be used, suppose get_entity_radar returns i32
        entity_id = self.radar.get_entity_id(radar_target_id)

        #Explicitly define type, suppose GetEntity returns Aircraft.
        #Suppose this is not a high performance entity manager, it's just a big bucket of pointers
        target: Aircraft = EntityManager.GetEntity(entity)
  
        #We will allow globals
        SoundManager.Play("assets/sounds/sidewinder-growl-tone.wav")
       
        #Match on trait types
        if target.get() is FighterJet: # cannot check for concrete type here.
            UIManager.ChangeRadarLockColor(radar_target_id, 255, 0, 0)
        else:
            UIManager.ShowMessage("Warning: Civilian aircraft locked")
            UIManager.ChangeRadarLockColor(radar_target_id, 255, 255, 0)

impl Aircraft for Boeing777:

    def name(self) -> str:
        return "Boeing 777" 

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration + rate

    def throttle_up(self, rate: f32):
        self.current_acceleration = self.current_acceleration - rate


```


This begs the question: how are we going to do dynamic dispatch and be able to check if a value implements a type?

For every interface impl we will generate a vtable for that type, and use fat pointers. When we need to pass a value using a trait object, we will pack the data into a fat pointer containing a pointer to the data, and a pointer to the required interface.

For instance, a method receiving an Aircraft would actually receive:

    -   Pointer to the instance data, that could be either stack or heap allocated
    -   Pointer to the vtable

If we pass a `Su27Flanker` instance, we will pass the pointer to it, and the pointer to the `Su27Flanker`'s specific `Aircraft` vtable pointer. The compiler will know the specific address to pass, and the code will know the offsets of the functions to be called. Notice that in this case the method won't be able to call the `init` function, the typechecker will prevent it.


To generate the code for the Su27Flanker methods, first we find all implementations and generate code for them separately, but try to put them as close as possible, so that we don't have too long jumps. Code implemented separately that calls onto the base might take a longer jump, but that's ok.

If we just receive the struct directly, then the compiler just won't resort to the vtable, it will resolve the call at compile time.

The `is` keyword is only able to check if a type has a trait by checking if has a vtable for 
that type. Casting to the type should be possible but syntax is yet to be defined. I like Jai's syntax for casting, where the cast seems to be an unary operator, like `cast(u8) expr`. Casting to a derived type is not allowed, as that would need to keep type information on the executable itself, and I don't want that for now.









