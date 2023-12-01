from .day01 import day01


def day(n, one, two):
    print("DAY %s", n)
    print("Part one: %s", one)
    print("Part two: %s", two)


def main():
    day(1, *day01())


if __name__ == "__main__":
    main()
