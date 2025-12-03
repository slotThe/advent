use itertools::concat;
use rust_aoc_util::check;

fn jolts(xs: &[isize], n: usize) -> isize {
    xs.iter().fold(vec![0; n], |p, k| {
        p.iter()
            .cloned()
            .zip(concat([vec![0], p.clone()]))
            .map(|(a, b)| a.max(b * 10 + k))
            .collect()
    })[n - 1]
}

fn main() {
    let inp: Vec<Vec<isize>> = std::fs::read_to_string("../inputs/day03.txt")
        .unwrap()
        .trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as isize)
                .collect()
        })
        .collect();
    check!(inp.iter().map(|l| jolts(l, 2)).sum::<isize>(), 17158);
    check!(
        inp.iter().map(|l| jolts(l, 12)).sum::<isize>(),
        170449335646486
    );
}
