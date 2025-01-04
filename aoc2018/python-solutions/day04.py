from collections import defaultdict, Counter
import re
from datetime import datetime, timedelta


def parse() -> list[tuple[int, list[datetime]]]:
    def p_line(s: str) -> datetime:
        (t, _) = s.split("]", maxsplit=1)
        return datetime.strptime(t, "[%Y-%m-%d %H:%M")

    with open("../inputs/day04.txt") as inp:
        chunks = []
        c = (0, [])  # Dummy default value, will be overwritten immediately.
        for line in sorted(ln for ln in inp.readlines()):
            if "begins shift" in line:
                id_ = int(re.findall(r"#(\d+)", line)[0])
                chunks.append(c)
                c = (id_, [p_line(line)])
            else:
                c[1].append(p_line(line))
        chunks.append(c)  # Don't forget to append the last chunk.
    return chunks


def asleep_during(cs: list[datetime]) -> list[int]:
    """Get all the minutes that a guard is asleep between 0000 and 0059. The
    idea is that the logs are sensible (= nice) and only follow the simple
    protocol of "guard begins, guard falls asleep, guard awakes, guard falls
    asleep, …". This means we don't have to keep track of the actual
    instructions; instead, we create a sliding window of size two and take
    all the odd indices."""
    asleep = []
    for start, stop in list(zip(cs, cs[1:]))[1::2]:
        for m in range(int((stop - start).seconds / 60)):
            asleep.append((start + timedelta(minutes=m)).minute)
    return asleep


asleep = defaultdict(list)
for i, ms in parse():
    asleep[i].extend(asleep_during(ms))
asleep = {i: Counter(ms) for (i, ms) in asleep.items()}

id_, mins = max(asleep.items(), key=lambda id_ms: sum(id_ms[1].values()))
one = id_ * mins.most_common(1)[0][0]  # id * most common minute
assert one == 99759
print(one)

id_, (min_, _) = max(
    [
        (i, max(mins.items() or [(0, 0)], key=lambda ma: ma[1]))  # max by amnt
        for (i, mins) in asleep.items()
    ],
    key=lambda id_min_amnt: id_min_amnt[1][1],  # max by amnt
)
two = id_ * min_
assert two == 97884
print(id_ * min_)
