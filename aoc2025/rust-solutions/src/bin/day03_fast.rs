use itertools::Itertools;
use rust_aoc_util::check;

fn jolts(mut xs: Vec<isize>, n: usize) -> isize {
    let mut m = vec![-1; xs.len()]; // mask
    let mut b = xs.len() - 1; // bound
    let mut f = vec![0; xs.len()]; // from
    let mut fi = 0; // from index
    let mut i = 0; // iterations
    while i < n {
        while f[fi] >= b {
            fi = 1.max(fi) - 1; // rewind
        }
        let p = xs.len() - 1 - xs[f[fi]..].iter().rev().position_max().unwrap();
        if m[p] != -1 {
            b = m.iter().rposition(|x| *x == -1).unwrap(); // already full -> rewind
            continue;
        }
        m[p] = xs[p];
        xs[p] = -1;
        fi += 1;
        f[fi] = p;
        i += 1;
    }
    m.iter().filter(|x| **x != -1).fold(0, |a, x| 10 * a + x)
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
    check!(
        inp.iter().map(|l| jolts(l.to_vec(), 2)).sum::<isize>(),
        17158
    );
    check!(
        inp.iter().map(|l| jolts(l.to_vec(), 12)).sum::<isize>(),
        170449335646486
    );
}
