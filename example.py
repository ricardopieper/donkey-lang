def main() -> i32:
    x = 0
    mod5 = 0
    while x < 900000000:
        x = x + 1
        if x % 5 == 0:
            mod5 = mod5 + 1
    return mod5