import re
from collections import Counter


def points(xs: list[int]) -> list[tuple[int, int]]:
    [_, lft, top, w, t] = xs
    return [(lft + x, top + y) for x in range(w) for y in range(t)]


with open("../inputs/day03.txt") as f:
    inp = [[int(x) for x in re.findall(r"\d+", line)] for line in f.readlines()]

grid = Counter(p for line in inp for p in points(line))

one = len([x for x in grid.values() if x >= 2])
assert one == 100261
print(one)

two = 0
for line in inp:
    if all(x == 1 for x in [grid.get(x) for x in points(line)]):
        two = line[0]
        break
assert two == 251
print(two)
