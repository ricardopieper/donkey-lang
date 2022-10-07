def fib(i):
    if i <= 1:
        return 1
    else:
        return fib(i - 1) + fib(i - 2)

def main():
    result = fib(30)
    print(result)

main()