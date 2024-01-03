from collections import defaultdict
from functools import reduce
from util import INPUTS
import re


def parse():
    with open(f"{INPUTS}/day02.txt", "r") as inp:
        return [
            [(int(m[0]), m[1]) for m in re.findall(r"([0-9]+) (green|blue|red)", line)]
            for line in inp.readlines()
        ]


def game_possible(val, color):
    match color:
        case "blue":
            return 14 >= val
        case "green":
            return 13 >= val
        case "red":
            return 12 >= val


def possible_games(xs):
    return sum(
        [
            i
            for (i, xs) in zip(range(1, len(xs) + 1), xs)
            if all(game_possible(v, c) for (v, c) in xs)
        ]
    )


def part2(cubes):
    res = defaultdict(int)  # Defaults to 0
    for val, color in cubes:
        res[color] = max(res[color], val)
    return reduce(lambda a, b: a * b, res.values())


def day02():
    input = parse()
    return possible_games(input), sum([part2(cubes) for cubes in input])


assert day02() == (2776, 68638)

if __name__ == "__main__":
    print(day02())
