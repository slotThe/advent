use std::collections::HashSet;

pub fn day6() -> (usize, usize) { (solve(4), solve(14)) }

fn solve(n: usize) -> usize {
  let inp: Vec<char> = std::fs::read_to_string("../inputs/day6.txt")
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
