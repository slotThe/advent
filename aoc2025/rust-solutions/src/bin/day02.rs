use std::collections::HashSet;

use itertools::*;
use rust_aoc_util::check;

fn gen_nums(s: &str, e: &str, n: usize) -> HashSet<usize> {
    let sl = s.len();
    let el = e.len();
    if el > sl {
        let mut h1 = gen_nums(s, &"9".repeat(sl), n);
        h1.extend(gen_nums(&("1".to_string() + &"0".repeat(el - 1)), e, n));
        return h1;
    }
    if !sl.is_multiple_of(n) {
        return HashSet::new();
    }
    let np = sl / n; // number of partitions
    let sv = s.parse::<usize>().unwrap();
    let ev = e.parse::<usize>().unwrap();
    // The repeater r: ∀k the number r*k is the number k repeated n times.
    let r = (0..n).map(|i| 10usize.pow((np * i) as u32)).sum();
    let st = s[0..np].to_string().parse::<usize>().unwrap() * r; // skip to start
    HashSet::from_iter((st..ev + 1).step_by(r).filter(|x| *x >= sv))
}

fn main() {
    let inp: Vec<(String, String)> = std::fs::read_to_string("../inputs/day02.txt")
        .unwrap()
        .trim()
        .split(",")
        .map(|x| x.split("-").map(|s| s.to_string()).next_tuple().unwrap())
        .collect();
    check!(
        inp.iter()
            .map(|(s, e)| gen_nums(s, e, 2).iter().sum::<usize>())
            .sum::<usize>(),
        54234399924
    );
    check!(
        inp.iter()
            .map(|(s, e)| {
                let mut h = HashSet::new();
                (2..e.len() + 1).for_each(|i| h.extend(gen_nums(s, e, i)));
                h.iter().sum::<usize>()
            })
            .sum::<usize>(),
        70187097315
    )
}
