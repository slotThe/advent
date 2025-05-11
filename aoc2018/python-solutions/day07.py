from collections import defaultdict
from dataclasses import dataclass
from itertools import repeat
import re
from typing import Union

need = defaultdict(list[str])
have = defaultdict(list[str])
with open("../inputs/day07.txt", "r") as f:
    for s in f.readlines():
        res = re.fullmatch(
            r"Step (\w) must be finished before step (\w) can begin.", s.strip()
        )
        need[res[2]].append(res[1])
        have[res[1]].append(res[2])
starts = sorted([k for k in have.keys() if k not in need.keys()], reverse=True)

# One
queue = starts
final = []
seen = set()
while queue:
    step = queue.pop()
    if step not in seen:
        final.append(step)
    seen.add(step)
    for k in have[step]:
        if all(n in seen for n in need[k]):
            queue.append(k)
    queue.sort(reverse=True)
assert "GKRVWBESYAMZDPTIUCFXQJLHNO" == "".join(final)
print("".join(final))


# Two
@dataclass
class Empty:
    pass


@dataclass
class Working:
    char: str
    time: int


queue = starts
chars = [k for k in have.keys()]
final = []
work = list(repeat(Empty(), 5))
seen = set()
count = -1
while len(final) != len(chars):
    count += 1
    cur = []
    for i in range(len(work)):
        match work[i]:
            case Working(char=c, time=n):
                cur.append(c)
                if n <= 1:
                    work[i] = Empty()
                    if c not in seen:
                        final.append(c)
                        seen.add(c)
                else:
                    work[i] = Working(char=c, time=n - 1)
            case _:
                if queue:
                    c = queue.pop()
                    work[i] = Working(char=c, time=60 + ord(c) - 65)
                    cur.append(c)
    for c in chars:
        if all(n in seen for n in need[c]) and c not in cur + final:
            queue.append(c)
    queue.sort(reverse=True)
assert 903 == count
print(count)
