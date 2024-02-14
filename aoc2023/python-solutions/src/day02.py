from collections import defaultdict
from functools import reduce
from util import INPUTS
import re

type game = list[tuple[int, str]]


def parse() -> list[game]:
    with open(f"{INPUTS}/day02.txt", "r") as inp:
        return [
            [(int(m[0]), m[1]) for m in re.findall(r"([0-9]+) (green|blue|red)", line)]
            for line in inp.readlines()
        ]


def game_possible(val: int, color: str) -> bool:
    match color:
        case "blue":
            return 14 >= val
        case "green":
            return 13 >= val
        case "red":
            return 12 >= val
        case _:
            return False


def possible_games(xs: list[game]) -> int:
    return sum(
        [
            i
            for (i, xs) in zip(range(1, len(xs) + 1), xs)
            if all(game_possible(v, c) for (v, c) in xs)
        ]
    )


def part2(cubes: game) -> int:
    res = defaultdict(int)  # Defaults to 0
    for val, color in cubes:
        res[color] = max(res[color], val)
    return reduce(lambda a, b: a * b, res.values())


def day02() -> tuple[int, int]:
    input = parse()
    return possible_games(input), sum([part2(cubes) for cubes in input])


assert day02() == (2776, 68638)

if __name__ == "__main__":
    print(day02())
