use itertools::{concat, Itertools};
use rust_aoc_util::check;

fn jolts(xs: &[isize], n: usize) -> isize {
    let mut r = 0;
    (0..n).rev().fold(xs, |ys, m| {
        let m = ys.iter().dropping_back(m).max().unwrap();
        r = m + r * 10;
        &ys[ys.iter().position(|x| x == m).unwrap() + 1..]
    });
    r
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
