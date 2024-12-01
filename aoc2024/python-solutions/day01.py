import numpy as np
from collections import Counter

with open("../inputs/day01.txt") as f:
    inp = [int(x) for x in f.read().split()]
    l, r = np.asarray(sorted(inp[::2])), np.asarray(sorted(inp[1::2]))  # noqa: E741
    print(sum(abs(l - r)))
    freqs = Counter(r)
    print(sum(x * freqs.get(x, 0) for x in l))
