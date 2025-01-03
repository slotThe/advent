from itertools import accumulate, cycle
from operator import add

with open("../inputs/day01.txt") as f:
    ints = [int(x) for x in f.readlines()]

one = sum(ints)
assert 536 == one
print(one)

seen = set()
two = 0
for n in accumulate(cycle(ints), add):
    if n in seen:
        two = n
        break
    seen.add(n)
assert two == 75108
print(two)
