from util import INPUTS, p_ints, prod
import re


def binary(pred, lo, hi):
    while lo != hi:
        mid = (lo + hi) // 2
        if pred(mid):
            hi = mid
        else:
            lo = mid + 1
    return lo


def solve(time, dist):
    mid = time // 2
    hi = binary(lambda t: t * (time - t) <= dist, mid, time)
    # Use the symmetry of the bell curve.
    return hi - (time - hi + 1)


input = open(f"{INPUTS}/day06.txt", "r").readlines()

part1 = prod(solve(time, dist) for time, dist in zip(*map(p_ints, input)))
assert part1 == 781200

part2 = solve(*[int("".join(re.findall(r"\d+", line))) for line in input])
assert part2 == 49240091

print("Part 1: ", part1, "\nPart 2: ", part2)
