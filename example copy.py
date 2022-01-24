struct Struct1:
    field1: i32
    field2: i64

def my_function2(arg1: i32, arg2: i32) -> i32:
    result1: i32 = my_function(arg1, arg2)
    result2 = pow(arg1, arg2)
    return my_function(result1, result2)

def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)