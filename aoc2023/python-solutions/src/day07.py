from typing import Callable
from util import INPUTS

type Sorter = tuple[int, list[int]]
type SortOn = Callable[[str], Sorter]


def hand_val(hand: list[int]) -> int:
    match hand:
        case [5]:
            return 6
        case [4, 1]:
            return 5
        case [3, 2]:
            return 4
        case [3, 1, 1]:
            return 3
        case [2, 2, 1]:
            return 2
        case [2, 1, 1, 1]:
            return 1
        case _:
            return 0


def tie_breaker(order: str, hand: str):
    return [order.find(str(c)) for c in hand]


def part1(hand: str) -> Sorter:
    return (
        hand_val(sorted([hand.count(c) for c in "".join(set(hand))], reverse=True)),
        tie_breaker("23456789TJQKA", hand),
    )


def part2(hand: str) -> Sorter:
    tie_break = tie_breaker("J23456789TQKA", hand)
    match sorted([hand.count(c) for c in "".join(set(hand)) if c != "J"], reverse=True):
        case first, *other:
            return (
                hand_val([first + hand.count("J"), *other]),
                tie_break,
            )
        case _:  # Only jokers
            return (hand_val([5]), tie_break)


def solve(input: list[tuple[str, int]], sort_on: SortOn) -> int:
    return sum(
        i * s
        for i, (_, s) in enumerate(sorted(input, key=lambda x: sort_on(x[0])), start=1)
    )


input = []
for line in open(f"{INPUTS}/day07.txt", "r").readlines():
    hand, points = line.strip().split(" ")
    input.append((hand, int(points)))
print("Part 1: ", solve(input, part1), "\nPart 2: ", solve(input, part2))
