use crate::util::*;
use std::collections::HashSet;

pub fn day6(p: Part) -> usize {
    match p {
        Part::One => solve(4),
        Part::Two => solve(14),
    }
}

fn solve(n: usize) -> usize {
    let inp: Vec<char> = std::fs::read_to_string("./input/day6.txt")
        .unwrap()
        .chars()
        .collect();
    n + inp
        .windows(n)
        .take_while(|win| {
            let mut hs = HashSet::new();
            win.iter().any(|x| !(hs.insert(x)))
        })
        .count()
}
