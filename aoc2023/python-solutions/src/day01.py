import re


def solve1(inp: str) -> int:
    matches = re.findall(r"[1-9]", inp)
    return int(matches[0]) * 10 + int(matches[-1])


def solve2(inp: str) -> int:
    def convert(a: str, b: str) -> int:
        if a != "":
            return int(a)
        else:
            match b:
                case "one":
                    return 1
                case "two":
                    return 2
                case "three":
                    return 3
                case "four":
                    return 4
                case "five":
                    return 5
                case "six":
                    return 6
                case "seven":
                    return 7
                case "eight":
                    return 8
                case "nine":
                    return 9

    matches = re.findall(
        r"([1-9])|(?=(one|two|three|four|five|six|seven|eight|nine))", inp
    )
    return convert(*matches[0]) * 10 + convert(*matches[-1])


def day01():
    with open("../inputs/day01.txt", "r") as inp:
        inp = inp.readlines()

        def solve(solve_with):
            print(sum(map(lambda x: solve_with(x.strip()), inp)))

        solve(solve1)
        solve(solve2)
