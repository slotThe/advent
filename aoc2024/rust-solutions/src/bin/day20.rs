use std::collections::{HashMap, HashSet};

use anyhow::Result;
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

fn solve(inp: &HashMap<Coord, char>, start: Coord, end: Coord, max_dist: u32) -> usize {
  let free: HashSet<Coord> = inp
    .iter()
    .filter(|(_, c)| c != &&'#')
    .map(|(p, _)| *p)
    .collect();
  let path: HashMap<Coord, i32> = build_path(&free, start, end);
  let mut res = 0;
  for p in &free {
    let np = path.get(&p).unwrap();
    res += free
      .iter()
      .filter(|q| p.manhattan(**q) <= max_dist)
      .filter(|&q| path.get(q).unwrap() - np - p.manhattan(*q) as i32 >= 100)
      .count();
  }
  res
}

fn main() -> Result<()> {
  let g = coord::map_from_grid(&std::fs::read_to_string("../inputs/day20.txt")?);
  let start = *g.iter().find(|(_, c)| c == &&'S').map(|(p, _)| p).unwrap();
  let end = *g.iter().find(|(_, c)| c == &&'E').map(|(p, _)| p).unwrap();
  print_day(18, (solve(&g, start, end, 2), solve(&g, start, end, 20)));
  Ok(())
}
