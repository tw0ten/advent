from sys import stdin


def p1(i):
    l = 0
    for c in i:
        if c == '(':
            l = l + 1
        if c == ")":
            l = l - 1
    return l


def p2(i):
    l = 0
    n = 0
    for c in i:
        if c == '(':
            l = l + 1
        if c == ")":
            l = l - 1
        n = n + 1
        if l == -1:
            break
    return n


def p(i):
    print(p1(i))
    print(p2(i))


p(stdin.read())
