def search(a, b, val, func, eps):
    x = (a + b) / 2

    f_x = func(x)

    print(x, f_x)

    if abs(f_x - val) < eps:
        return x

    if f_x > val:
        return search(a, x, val, func, eps)
    else:
        return search(x, b, val, func, eps)
