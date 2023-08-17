Suppose you have this code:

    def normal_function() -> i32:
        return 1


    def main():
        x = normal_function()

What's the type of x?

In this case, top_level_decls stage will type normal_function as a function type and register it as () -> i32. It's resolved, just take
the return type and slap it onto x. 


However, let's add just one level of complexity: one type parameter, but the normal_function still returns i32:

    def generic_function<T>() -> i32:
        return 1

    def main():
        x = generic_function<i64>()

Now what's the type of x?

In this case, top_level_decls stage will type generic_function as a "pending monomorphization", with a type signature of <T>() -> i32.
The type constructor algorithm gives up if any type parameters are present.

However, we still need to resolve what X can be. But this case is somewhat weird, let's setup a more useful case.


    def generic_function<T>() -> T:
        return 1

    def main():
        x = generic_function<i64>()

Of course this code shouldn't compile as 1 is not T, but suppose it does.

We have the following data:
    Callee: generic_function
    Type parameters: #0 = T
    Parameters: []
    Return type: T

    Call site: generic_function
    Type parameters: #0 = i64


More complicated case:

  def generic_function<T, U>() -> T:
        return 1

    def main():
        x = generic_function<i32, i64>()

In this case the type of X should be i32

  def generic_function<T, U>() -> U:
        return 1

    def main():
        x = generic_function<i32, i64>()

In this case, i64


  def generic_function<T, U>(length: T) -> List<U>:
        return 1

    def main():
        x = generic_function<i32, i64>(20)

In this case, List<i64> and length should be i32


In this case we would like to generate a new function signature like so:

    fn(length: i32) -> List<i64>

There are more complicated cases though, because we can pass type params coming from type params:

    def generic_function<T, U>(length: T) -> List<U>:
        return 1

    def some_function<T>():
        x = generic_function<i32, T>(20)

In this case the type of X is List<T>, but the T is in context of some_function and not generic_function


We have the following data:
    Callee: generic_function
    Type parameters: T = #0, U = #1
    Parameters: [length: T]
    Return type: List<U>

    Call site: generic_function
    Type parameters: 
        #0 = i32 (Resolved)
        #1 = T (Unresolved)


    Desired generated type signature for this call:

    fn(i32) -> List<T>

    In terms of the function signature in the call site, we could have:

    Callee: generic_function
    Type parameters: <i32, T>
    Parameters: [i32]
    Return type: List<T>



This is kind of a symbolic replacement.

One thing we know is that the function signature is self-contained, it provides the information necessary for callers to resolve,
so we can first build a substitution instruction tree: Given a list of parameters and a type usage, we should be able to resolve the type.

We have a function signature with "symbols", we can replace by a function signature with replacement instructions, like so:

    Callee: generic_function
    Type parameters: T = #0, U = #1
    Parameters: [length: T]
    Return type: List<U>

    Becomes (conceptually)

    Callee: generic_function
    Type parameters: T = #0, U = #1
    Parameters: [length: #0]
    Return type: List<#1>

Then it's just a matter of finding that index in the list of parameters.