def rec(i):
    if i <= 0:
        return 1
    else:
        return rec(i - 1)

def main():
    result = rec(10)
    print(result)

main()