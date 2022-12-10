Donkey's Type System
====================

The type system is actually kind of simplistic. We have support for generics in the type system but not much else besides basic checking.
It will get more complex in the future with traits though.

We store type information in the HIRExpr tree directly, but this proved to be a bad idea:

```
HIRExpr::Literal(
    LiteralHIRExpr::StringLiteral("some"),
    TypeInstance::Simple(type id of str)
)
```

What's the big problem?

The issue is that TypeInstance looks like this:

```
pub enum TypeInstance {
    Simple(TypeId),
    Generic(TypeId, Vec<TypeInstance>),
    Function(Vec<TypeInstance>, Box<TypeInstance>),
}
```

This is a pretty large type, around 40 bytes. This also leads to redundant allocations when we have multiple instantiations of the same types.
Also, most type instances are simple (non-generic), and we pay the price of 40 bytes for those... replicated everywhere in the tree. 

However, we also have another problem: when we have generics, we actually don't have all of the information we want at hand: to resolve a method call that returns generic
or to resolve a field with a generic typem we have to query the type database, find the type, replace with generics, etc etc etc. Feasible, but cumbersome.

We will instead refactor this to have 2 databases:

 - A type constructor database, which records every type declaration
 - A type instance database, which records the types used by the program

The HIR will continue storing type information, but we will only store a single ID:

```
HIRExpr::Literal(
    LiteralHIRExpr::StringLiteral("some"),
    TypeInstanceId(type id of str)
)

HIRExpr::Array(
    [HIRExpr::LiteralHIRExpr(
        LiteralHIRExpr::StringLiteral("some"),
        TypeInstanceId(type id of str)
    ]
    TypeInstanceId(type id of array<str>)
)
```

As a side effect, this table will also hold all generic types already replaced, so we can do monomorphization later. 

Functions are another problem. Suppose we have this:

```
def func(i: i32) -> i32:
    return i * 2

def main():
    some_func = func
    some_func(10)
```

In this case, we want the type of some_func to be inferred as `fn (i32) -> i32`. Currently we store in the `TypeInstance::Function` enum variant, but we won't have this anymore. This information will also live in the Type Instance database, as a record like anything else. 
