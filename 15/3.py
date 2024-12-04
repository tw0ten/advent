from sys import stdin


def p1(i):
    (x, y) = (0, 0)
    visited = [(x, y)]
    for c in i:
        if c == "^":
            y = y - 1
        if c == "v":
            y = y + 1
        if c == ">":
            x = x + 1
        if c == "<":
            x = x - 1
        if visited.count((x, y)) == 0:
            visited.append((x, y))
    return len(visited)


def p2(i):
    (x1, y1) = (0, 0)
    (x2, y2) = (0, 0)
    visited = [(0, 0)]
    j = 0
    for c in i:
        j = j + 1
        (x, y) = (0, 0)
        if c == "^":
            y = - 1
        if c == "v":
            y = + 1
        if c == ">":
            x = + 1
        if c == "<":
            x = - 1
        if j % 2 == 0:
            (x, y) = (x1, y1) = (x1 + x, y1 + y)
        else:
            (x, y) = (x2, y2) = (x2 + x, y2 + y)
        if visited.count((x, y)) == 0:
            visited.append((x, y))
    return len(visited)


def p(i):
    print(p1(i))
    print(p2(i))


p(stdin.read())
