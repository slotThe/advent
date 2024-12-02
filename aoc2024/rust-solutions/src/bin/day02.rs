use std::cmp::Ordering;

use itertools::Itertools;

fn one(xs: &[usize]) -> bool {
  let mut o = Ordering::Equal;
  xs.iter().tuple_windows().all(|(l, r)| {
    let c = l.cmp(r);
    o = o.then(c);
    c == o && (1..=3).contains(&l.abs_diff(*r))
  })
}

fn main() {
  let inp = std::fs::read_to_string("../inputs/day02.txt").unwrap();
  let inp: Vec<Vec<usize>> = inp
    .trim()
    .split("\n")
    .map(|l| l.split(" ").map(|s| s.parse::<usize>().unwrap()).collect())
    .collect();

  println!("{}", inp.iter().filter(|xs| one(xs)).count());

  println!(
    "{}",
    inp
      .iter()
      .filter(|xs| (0..xs.len())
        .map(|i| [&xs[..i], &xs[i + 1..]].concat())
        .any(|xs| one(&xs)))
      .count()
  );
}
