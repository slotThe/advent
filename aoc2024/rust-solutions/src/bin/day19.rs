#![feature(pattern)]
use std::{collections::HashMap, str::pattern::Pattern};

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::print_day;

fn arrangements(towels: &[&str], ptns: &[&str]) -> Vec<u64> {
  fn go<'a>(towels: &[&str], ptn: &'a str, cache: &mut HashMap<&'a str, u64>) -> u64 {
    match cache.get(&ptn) {
      Some(n) => *n,
      None => towels
        .iter()
        .filter(|s| s.is_prefix_of(&ptn))
        .map(|s| {
          let p = &ptn[s.len()..];
          let r = go(towels, p, cache);
          cache.insert(p, r);
          r
        })
        .sum(),
    }
  }
  let mut start = HashMap::from([("", 1)]);
  ptns.iter().map(|p| go(towels, p, &mut start)).collect()
}

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day19.txt")?;
  let arrangements = inp.split_once("\n\n").map_or(vec![], |(t, p)| {
    arrangements(&t.split(", ").collect_vec(), &p.lines().collect_vec())
  });
  print_day(
    19,
    (
      arrangements.iter().filter(|n| n > &&0).count(),
      arrangements.iter().sum::<u64>(),
    ),
  );
  Ok(())
}
