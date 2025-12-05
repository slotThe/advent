use itertools::Itertools;
use rust_aoc_util::check;

fn main() {
    let inp = std::fs::read_to_string("../inputs/day05.txt").unwrap();
    let (rs, ids): (&str, &str) = inp.split("\n\n").next_tuple().unwrap();
    let ids: Vec<u64> = ids.lines().map(|x| x.parse().unwrap()).collect();
    let rs: Vec<(u64, u64)> = rs
        .lines()
        .map(|l| {
            l.split("-")
                .map(|x| x.parse().unwrap())
                .next_tuple()
                .unwrap()
        })
        .collect();

    let one = ids.iter().fold(0, |a, id| {
        a + if rs.iter().any(|(x, y)| x <= id && id <= y) {
            1
        } else {
            0
        }
    });
    check!(one, 874);

    let (c, _) = rs.iter().sorted().fold((0, 0), |(c, n), (a, b)| {
        (
            c + if *a <= n { n.max(*b) - n } else { b - a + 1 },
            n.max(*b),
        )
    });
    check!(c, 348548952146313);
}
