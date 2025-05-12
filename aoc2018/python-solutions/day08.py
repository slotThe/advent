from functools import reduce

with open("../inputs/day08.txt", "r") as f:
    inp = [int(n) for n in f.readlines()[0].split(" ")]


def one(line: list[int]) -> int:
    def go(line: list[int], meta: int) -> tuple[list[int], int]:
        cn, me, *xs = line
        xs, meta = reduce(lambda rm, _: go(rm[0], rm[1]), range(cn), (xs, meta))
        return xs[me:], meta + sum(xs[:me])

    return go(line, 0)[1]


assert 45210 == one(inp)
print(one(inp))


def two(line: list[int]) -> int:
    def go(line: list[int]) -> tuple[list[int], int]:
        cn, me, *xs = line
        if cn == 0:
            return xs[me:], sum(xs[:me])
        children = []
        for _ in range(cn):
            r, m = go(xs)
            xs = r
            children.append((r, m))
        return xs[me:], sum(children[i - 1][1] for i in xs[:me] if i <= len(children))

    return go(line)[1]


assert 22793 == two(inp)
print(two(inp))
