import re
from functools import reduce
from util import INPUTS


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


def parse() -> list[str]:
    with open(f"{INPUTS}/day01.txt", "r") as inp:
        return [x.strip() for x in inp.readlines()]


def day01() -> tuple[int, int]:
    inp = parse()
    return sum([solve1(x) for x in inp]), sum([solve2(x) for x in inp])


assert day01() == (54331, 54518)

if __name__ == "__main__":
    print(day01())
