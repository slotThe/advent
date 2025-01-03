from collections import Counter

with open("../inputs/day02.txt") as f:
    inp = [line.strip() for line in f.readlines()]

two = 0
three = 0
for c in map(Counter, inp):
    if 2 in c.values():
        two += 1
    if 3 in c.values():
        three += 1
assert 6200 == two * three
print(two * three)


def solve2(inp: list[str]) -> str:
    for l1 in inp:
        for l2 in inp:
            r = [a for (a, b) in zip(l1, l2) if a == b]
            if len(r) == len(l1) - 1:
                return "".join(r)
    return "The fabric is lost forever!"


assert "xpysnnkqrbuhefmcajodplyzw" == solve2(inp)
print(solve2(inp))
