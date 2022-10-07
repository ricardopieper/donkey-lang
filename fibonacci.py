def fib(i: i32) -> i32:
    if i <= 1:
        return 1
    else:
        return fib(i - 1) + fib(i - 2)

def main():
    result: i32 = fib(30)