from util import INPUTS


def solve(readings, comp):
    def find_reflection(grid, mult, comp) -> int | None:
        for i in range(1, len(grid)):
            lefts = list(reversed(grid[:i]))
            rights = grid[i:]
            if comp(lefts, rights):
                return mult * i
        return None

    return sum(
        [
            # We assume that this definitely finds something, and erroring out
            # in case this is violated seems sensible.
            find_reflection(reading, 100, comp)
            or find_reflection(list(zip(*reading)), 1, comp)
            for reading in readings
        ]
    )


def compare_up_to_smudge(a: str, b: str) -> int:
    return sum(0 if l == r else 1 for (l, r) in zip(a, b))


readings = [
    grid.splitlines() for grid in open(f"{INPUTS}/day13.txt", "r").read().split("\n\n")
]
print(
    "Part 1: ",
    solve(readings, lambda lefts, rights: all(l == r for (l, r) in zip(lefts, rights))),
    "\nPart 2: ",
    solve(
        readings,
        lambda lefts, rights: sum(
            compare_up_to_smudge(l, r) for (l, r) in zip(lefts, rights)
        )
        == 1,
    ),
)
