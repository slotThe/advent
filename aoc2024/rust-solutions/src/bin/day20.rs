use std::collections::{HashMap, HashSet};

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::{coord::{self, Coord}, print_day};

fn build_path(path: &HashSet<Coord>, start: Coord, end: Coord) -> HashMap<Coord, i32> {
  let mut p = end;
  let mut res = HashMap::new();
  for k in 0.. {
    res.insert(p, k);
    if p == start {
      return res;
    }
    p = p
      .clone()
      .neighbours4()
      .find(|n| path.contains(n) && !res.contains_key(n)) // Unique neighbour
      .unwrap();
  }
  res
}

fn solve(path: &HashMap<Coord, i32>, max_dist: i32) -> Vec<i32> {
  // Not strictly necessary, but improves runtime by about 2x.
  let stencil = (-max_dist..=max_dist)
    .flat_map(|x| (-max_dist..=max_dist).map(move |y| coord::from_pair((x, y))))
    .filter(|p| coord::from_pair((0, 0)).manhattan(*p) <= max_dist as u32)
    .collect_vec();
  path
    .iter()
    .flat_map(|(&p, _)| {
      let np = path.get(&p).unwrap();
      stencil.iter().filter_map(move |&s| {
        let q = s + p; // p added to default stencil
        let d = p.manhattan(q) as i32;
        path
          .get(&q)                                          // in path?
          .and_then(|nq| (nq - np - d >= 100).then_some(d)) // high enough score?
      })
    })
    .collect()
}

fn main() -> Result<()> {
  let g = coord::map_from_grid(&std::fs::read_to_string("../inputs/day20.txt")?);
  let start = *g.iter().find(|(_, c)| c == &&'S').map(|(p, _)| p).unwrap();
  let end = *g.iter().find(|(_, c)| c == &&'E').map(|(p, _)| p).unwrap();

  let path = build_path(
    &g.iter()
      .filter(|(_, c)| c != &&'#')
      .map(|(p, _)| *p)
      .collect(),
    start,
    end,
  );

  let r = solve(&path, 20);
  print_day(20, (r.iter().filter(|d| **d == 2).count(), r.len()));
  Ok(())
}
