from sys import stdin


def min(x, y, z):
    return (x, y if y < z else z) if x < y else (y, x if x < z else z)


def p1(i):
    t = 0

    for (l, w, h) in i:
        (m1, m2) = min(l, w, h)
        t += m1 * m2
        t += 2*l*w+2*w*h+2*h*l
    return t


def p2(i):
    t = 0

    for (l, w, h) in i:
        (m1, m2) = min(l, w, h)
        t += 2*(m1+m2)
        t += l*w*h
    return t


def p(i):
    i = [j.split("x") for j in i.split("\n")]
    i.pop()
    i = [(int(l), int(w), int(h)) for l, w, h in i]

    print(p1(i))
    print(p2(i))


p(stdin.read())
