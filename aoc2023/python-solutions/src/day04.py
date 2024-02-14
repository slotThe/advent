from util import INPUTS, p_ints
from dataclasses import dataclass


@dataclass
class Hand:
    winning: int
    multiplier: int


input: dict[int, Hand] = {}
for line in open(f"{INPUTS}/day04.txt", "r").readlines():
    [win, my] = [p_ints(c) for c in line.strip().split("|")]
    input[win[0]] = Hand(len(set(win[1:]).intersection(set(my))), 1)

part1 = sum(2 ** (hand.winning - 1) for hand in input.values() if hand.winning != 0)
assert part1 == 25571

unfolded_cards = input
for i in range(1, max(unfolded_cards.keys()) + 1):
    cur = unfolded_cards[i]
    for j in range(0, cur.winning):
        unfolded_cards[j + i + 1].multiplier += cur.multiplier
part2 = sum(hand.multiplier for hand in unfolded_cards.values())
assert part2 == 8805731


print(part1)
print(part2)
