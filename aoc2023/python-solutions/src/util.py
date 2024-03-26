from functools import reduce
from numbers import Integral
import os
import re
from typing import Iterable

INPUTS = os.path.join(os.environ["PROJECT_ROOT"], "../inputs")


def p_ints(s: str) -> list[int]:
    """Parse a string into a list of ints."""
    return list(map(int, re.findall(r"-?\d+", s)))


def prod(xs: Iterable[Integral]) -> Integral:
    """Return the product of the given list of numbers."""
    return reduce(lambda a, b: a * b, xs)
