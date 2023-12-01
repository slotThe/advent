import re
from functools import reduce


def solve1(inp: str) -> int:
    matches = re.findall(r"[1-9]", inp)
    return int(matches[0]) * 10 + int(matches[-1])


replacements = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


def solve2(inp: str) -> int:
    def convert(a: str, b: str) -> int:
        if a != "":
            return int(a)
        else:
            return 1 + replacements.index(b)

    regexp = re.compile(
        "([1-9])|(?=(" + reduce((lambda acc, el: acc + "|" + el), replacements) + "))"
    )
    matches = re.findall(regexp, inp)
    return convert(*matches[0]) * 10 + convert(*matches[-1])


def day01():
    with open("../../inputs/day01.txt", "r") as inp:
        inp = inp.readlines()

        def solve(solve_with):
            print(sum(map(lambda x: solve_with(x.strip()), inp)))

        solve(solve1)
        solve(solve2)
