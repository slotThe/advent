from collections import defaultdict
from itertools import takewhile
from functools import reduce
from util import INPUTS

type coord = tuple[int, int]


def neighbours8(i: int, j: int) -> list[coord]:
    return [
        (i, j - 1),
        (i, j + 1),
        (i + 1, j),
        (i + 1, j - 1),
        (i - 1, j + 1),
        (i - 1, j),
        (i - 1, j - 1),
        (i + 1, j + 1),
    ]


def get_digit(inp: dict[coord, str], nx: int, ny: int) -> int:
    all_digits = list(
        takewhile(
            lambda xy: inp.get((xy[0], xy[1]), ".").isdigit(),
            map(lambda off: (nx, ny + off), range(0, 10)),
        )
    )
    return int("".join(inp[xy] for xy in all_digits))


input: dict[coord, str] = {
    (i, j): c
    for i, line in enumerate(open(f"{INPUTS}/day03.txt", "r").readlines())
    for j, c in enumerate(line.strip())
}

association_map = defaultdict(set[coord])
for (i, j), el in input.items():
    if not (el.isdigit() or el == "."):
        for nx, ny in neighbours8(i, j):
            if input.get((nx, ny), ".").isdigit():
                nny = ny  # Get leftmost digit. Ugh.
                while input.get((nx, nny - 1), ".").isdigit():
                    nny -= 1
                association_map[i, j].add((nx, nny))

total = sum(get_digit(input, *n) for n in reduce(set.union, association_map.values()))
assert total == 532445

total2 = 0
for sym, neighs in association_map.items():
    if input[sym] == "*" and len(neighs) == 2:
        [n1, n2] = neighs
        total2 += get_digit(input, *n1) * get_digit(input, *n2)
assert total2 == 79842967

print(total)
print(total2)
