#![feature(iter_map_windows)]
use std::collections::HashMap;

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::slurp_nums;

fn sim(mut n: u64) -> Vec<u64> {
  let mut res = vec![n];
  for _ in 0..2000 {
    n = ((n << 6) ^ n) & 16777215;
    n = ((n >> 5) ^ n) & 16777215;
    n = ((n << 11) ^ n) & 16777215;
    res.push(n);
  }
  res
}

type W4 = (i32, i32, i32, i32);

fn main() -> Result<()> {
  let sims: Vec<Vec<u64>> = slurp_nums(&std::fs::read_to_string("../inputs/day22.txt")?)?
    .iter()
    .map(|n: &i32| sim(*n as u64))
    .collect_vec();

  let one: u64 = sims.iter().map(|v| v[2000]).sum();
  assert_eq!(13584398738, one);
  println!("{one}");

  let diffs: Vec<HashMap<W4, i32>> = sims
    .iter()
    .map(|v| v.iter().map(|n| n.rem_euclid(10) as i32).collect_vec())
    .map(|v| {
      v.iter()
        .map_windows(|&[a, b]| *b - *a)
        .tuple_windows::<W4>()
        .enumerate()
        .map(|(i, w)| (w, v[i + 4]))
        .unique_by(|(w, _)| *w)       // first window wins
        .collect()
    })
    .collect_vec();
  let two = *diffs
    .iter()
    .fold(HashMap::new(), |mut acc, h| {
      h.iter().for_each(|(w, &v)| {
        acc.entry(w).and_modify(|ov| *ov += v).or_insert(v);
      });
      acc
    })
    .values()
    .max()
    .unwrap();
  assert_eq!(1612, two);
  println!("{two}");

  Ok(())
}
