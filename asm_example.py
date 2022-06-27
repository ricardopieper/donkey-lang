def half(num):
    return num // 2

def some_function(num, s):
    half_val = half(num) + s
    return num * half_val

def main():
    x  = 15
    y  = 3
    z  = x + y #18
    result = 5 + some_function(z, x)
    result = result + y 
    print(result)

main()