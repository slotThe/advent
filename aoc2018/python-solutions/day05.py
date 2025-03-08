def solve(inp: str) -> int:
    res = []
    for y in inp:
        if res == [] or y.swapcase() != res[-1]:
            res.append(y)
        else:  # Aa aA
            res.pop()
    return len(res)


with open("../inputs/day05.txt") as f:
    inp = f.readlines()[0].strip()
    print(solve(inp))
    print(
        min(
            map(
                solve,
                [
                    "".join(x for x in inp if x.lower() != u)
                    for u in set([x for x in inp if x.islower()])
                ],
            )
        )
    )
