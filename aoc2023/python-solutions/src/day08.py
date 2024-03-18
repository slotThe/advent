import math
import re

from util import INPUTS


def solve(
    starts: list[str],
    goals: list[str],
    graph: dict[str, tuple[str, str]],
    ins: str,
) -> int:
    singles = []
    for start in starts:
        pos = start
        ix = 0
        count = 0
        while pos not in goals:
            pos = graph[pos][0 if ins[ix] == "L" else 1]
            count += 1
            ix = (ix + 1) % len(ins)
        singles.append(count)
    return math.lcm(*singles)


ins, _, *nodes = map(lambda x: x.strip(), open(f"{INPUTS}/day08.txt", "r").readlines())
graph = {p: (l, r) for node in nodes for [p, l, r] in [re.findall("[A-Z]+", node)]}
print(
    "Part 1: ",
    solve(["AAA"], ["ZZZ"], graph, ins),
    "\nPart 2: ",
    solve(
        [n for n in graph.keys() if n.endswith("A")],
        [n for n in graph.keys() if n.endswith("Z")],
        graph,
        ins,
    ),
)
