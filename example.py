def main():
    x = 0
    y = 0
    mod5 = 0
    while x < 900000:
        y = y + 1
        x = x + 1
        if x % 5 == 0:
            mod5 = mod5 + 1