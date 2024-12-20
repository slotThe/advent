use std::collections::HashMap;

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::{coord::{self, Coord}, print_day};

type Grid = HashMap<Coord, char>;

fn solve(grid: &Grid, get_points: impl Fn(&Grid, Coord, Coord) -> Vec<Coord>) -> usize {
  grid
    .values()
    .unique()
    .filter(|&&a| a != '.')
    .flat_map(|antenna| {
      let ps = grid
        .iter()
        .filter_map(|(p, a)| if a == antenna { Some(p) } else { None })
        .collect_vec();
      ps.iter()
        .cartesian_product(&ps)
        .filter(|(p, np)| p != np) // off-diagonal
        .flat_map(|(&&p, &&np)| get_points(grid, p, np))
        .filter(|p| grid.contains_key(p))
        .collect_vec()
    })
    .unique()
    .count()
}

fn n_dist(n: i32, p: Coord, q: Coord) -> Coord { p - (p - q).cmul(n) }

fn main() -> Result<()> {
  let inp: Grid = coord::map_from_grid(&std::fs::read_to_string("../inputs/day08.txt")?);
  let one = solve(&inp, |_, p, np| vec![n_dist(2, p, np)]);
  let two = solve(&inp, |g, p, np| {
    (1..)
      .map(|i| n_dist(i, p, np))
      .take_while(|p| g.contains_key(p))
      .collect()
  });
  assert_eq!(one, 299);
  assert_eq!(two, 1032);
  print_day(8, (one, two));
  Ok(())
}
