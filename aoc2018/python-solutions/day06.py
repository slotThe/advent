import re
from sys import maxsize


def md(a: tuple[int, int], b: tuple[int, int]) -> int:
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def unique_min_ix(xs: list[int]) -> int | None:
    m = min(xs)
    if len([x for x in xs if x == m]) == 1:
        return xs.index(m)
    else:
        return None


with open("../inputs/day06.txt") as f:
    coords = list(
        map(
            lambda xs: (xs[0], xs[1]),
            [[int(x) for x in re.findall(r"-?\d+", line)] for line in f.readlines()],
        )
    )
    mx = max(c[0] for c in coords)  # max x
    my = max(c[1] for c in coords)  # max y

    assocs = [0 for _ in coords]
    two = 0
    for i in range(0, mx + 1):
        for j in range(0, my + 1):
            d = (i, j)
            ds = [md((i, j), c) for c in coords]
            if sum(ds) < 10000:
                two += 1
            k = unique_min_ix(ds)  # assocs is indexed in the same order as coords :]
            if k:
                if d[0] not in [0, mx] and d[1] not in [0, my]:  # edge → infinite area
                    assocs[k] += 1
                else:
                    assocs[k] = -maxsize
    one = max(assocs)
    assert one == 3604
    print(one)
    assert two == 46563
    print(two)
