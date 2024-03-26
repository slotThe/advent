import operator
from itertools import takewhile
from typing import Callable
from typing_extensions import Generator
from util import INPUTS, p_ints


def iterate[T](func: Callable[[T], T], start: T) -> Generator[T, None, None]:
    while True:
        yield start
        start = func(start)


def solve(reading: list[int]) -> int:
    return sum(
        [
            xs[-1]
            for xs in takewhile(
                lambda xs: any(x != 0 for x in xs),
                iterate(lambda xs: list(map(operator.sub, list(xs)[1:], xs)), reading),
            )
        ],
    )


readings = [p_ints(line) for line in open(f"{INPUTS}/day09.txt", "r").readlines()]
print(
    "Part 1: ",
    sum(map(solve, readings)),
    "\nPart 2: ",
    sum(map(lambda x: solve(x[::-1]), readings)),
)
