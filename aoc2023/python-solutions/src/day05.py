from itertools import batched
from util import INPUTS, p_ints


def find_match(
    seeds: list[tuple[int, int]], conv_map: list[list[int]]
) -> list[tuple[int, int]]:
    done = []
    while seeds != []:
        s, e = seeds.pop()
        for [dest, src, rnge] in conv_map:
            # Calculate overlap
            over_s = max(s, src)
            over_e = min(e, src + rnge)
            if over_s < over_e:  # any intersection happening at all?
                done.append((over_s - src + dest, over_e - src + dest))
                #   ______
                #  ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅
                if over_s > s and over_e < e:
                    seeds.append((s, over_s))
                    seeds.append((over_e, e))
                #       ______
                #  ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅
                elif over_s > s:
                    seeds.append((s, over_s))
                # ______
                #    ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅ ̅
                elif over_e < e:
                    seeds.append((over_e, e))
                break
        else:
            done.append((s, e))
    return done


def min_seed(seeds: list[tuple[int, int]], maps: list[list[list[int]]]) -> int:
    for mp in maps:
        seeds = find_match(seeds, mp)
    return min(a for a, _ in seeds)


[seeds], *input = list(
    map(
        lambda m: [x for x in map(p_ints, m.splitlines()) if x != []],
        open(f"{INPUTS}/day05.txt", "r").read().split("\n\n"),
    ),
)
print(
    "Part 1: ",
    min_seed([(s, s + 1) for s in seeds], input),
    "\nPart 2: ",
    min_seed([(s, s + r) for [s, r] in batched(seeds, 2)], input),
)
