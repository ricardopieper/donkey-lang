Monomorphization
================

I think there are several ways to implement monomorphization. The issue is that I didn't think this through in the beginning
and now my MIR does not support generics. Maybe it shouldn't?

The issue is that the later stages of HIR also kinda does not support generics, they require a `TypeInstanceId` 💀

Therefore we might just want to copy+paste with the different types?


Monomorphization Queue
======================


We will rely on a consistent mapping between a type usage and the monomorphized prefix it generates on structs and function names.

Suppose we have this function:


    def new_list<T>() -> List<T>:
        ls = List<T>()
        return ls


    def main():
        ls = new_list<i64>()
        print("it worked!")


We will loop through the global symbols of the code, and determine per-function:

 - Does it have generic parameters?
   - Put into a map of <symbol name, HIR node cloned. Yes, just clone it. And don't worry because it will likely be cloned even more!>
 - No, it doesnt:
   - Skip for now!

Iterate again:
 - Does it have generic parameters?
   - Skip!
 - No it doesn't
   - Iterate over HIR
   - For every generic type usage found:
     - Add into a queue of monomorphizations, where we pass the symbol name + positional generic arguments
     - Replace call by symbol_name + some well-defined prefix, for instance, {symbol_name}_i64 or something


What if the monomorphized function also calls into another function?


    def new_list<T>() -> List<T>:
        ls = List<T>()
        return ls

    def list_maker<T>() -> List<T>:
        return new_list<T>()

    def main() -> i64:
        ls = list_maker<i64>()
        print("it worked!")



Let's simulate:

Step 1: find all generics
State:

Definitions with type params: [
    "new_list", [original HIR node of new_list]
    "list_maker", [original HIR node of new_list]
]

Monomorphization queue: []

Step 2: Add to monomoprh queue:
Monomorphization queue: [
    "list_maker", i64
]

    def main() -> i64:
        ls = list_maker_i64()
        print("it worked!")

When we monomorphize "list_maker", we will have to run the step 2's process too! Why?

Because we are generating an list_maker_i64 function which has no generic arguments. This is as if list_maker_i64 was in the original user's code
that is calling onto a generic function. It's all the same for us.

Step 3: Run monomorphization queue:

Step 3.1 monomorphize list_maker, remember that T = i64
 - Run step 2 on list_maker
 - Iterate over AST
 - We will find new_list<T>()
 - We know that T = i64 in this case, so add to the monomorph queue:
   - ["new_list", i64]
 - Replace that "new_list" call by "new_list_i64"


The monomorphization process will be simple: look up the generic node, clone, replace all <T> by whatever the arg is. GG.




Ok more complicated example: Partial generics

    def add_item<K, V>(map: ptr<map<K, V>>, key: K, item: U):
        ...

    def create_string_keyed_map<V>(default_item: V) -> map<str, V>:
        my_map = map<str, V>()
        add_item(&my_map, "k", default_item);

    def make_a_map() -> map<str, i64>:
        return create_string_keyed_map<i64>(20)


    def main():
        x = make_a_map()
        println("worked")

First we iterate:

    [
        "add_item": {node},
        "create_string_keyed_map": {node},
    ]

Then again:

    make_a_map will generate in the queue:
    [
        "create_string_keyed_map", [i64]
    ]

Then we run monomorphization of create_string_keyed_map [i64] which generates:
    [
        map, [str, i64] (which means we have to replace the generic args, and then register an instantiation with all the arguments)
        add_item, [str, i64]
    ]


Failure scenarios:

1: Not passing the amount of generic arguments:

    def new_list<T>() -> List<T>:
        ls = List<T>()
        return ls


    def main():
        ls = new_list()
        print("type not passed!")

In this case we will look into the NameRegistry for the new_list, check if it is generic. If it is an we are passing a different amount of type arguments
than required, we throw a compilation error.

2: Non-existing types:

    def new_list<T>() -> List<T>:
        ls = List<U>() #????
        return ls


    def main():
        ls = new_list<i64>()
        print("success!")

In this case U simply does not exist. It's not in the generic args list and it's certainly not in the regular types list


# Plan

We will run this process after (maybe during) type inference, because we need to know the types.

Instead of name_registry returning 100% resolved types, it should maybe hold off that process for the type inference, i.e. don't return TypeInstanceId, just the original TypeUsage.



# Name Registry

Currently, our name registry only allows fully resolved types. This is sad. It does have a half-finished "partially_resolved_signatures" thing though.
We will use it and for now just call the appropriate functions.


# Why put it after inference?

 - Types are partially known, might be useful
 - If I want expression level type inference, I'd need some type inference to run before monomoprhization
 - Would retain most of current tests intact

# Why not put it after inference?

 - Type inference for binary operators don't work when types can't be inferred because things haven't been mmonomorphized, if x and y are generic, what is the type of x + y?
 - Would need to rerun type inference for these kinds of things, after monomorphization
  - Maybe binary ops logic can just be duplicated for now?

# C# w/o type bounds vs C++ approach

 - C#: just fails on binary operators
 - C++: accepts but fails for specific instantiations
 - Without type bounds, C# way becomes too restrictive
 - C++-like is the way to go
