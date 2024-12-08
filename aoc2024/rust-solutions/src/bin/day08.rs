use std::collections::{HashMap, HashSet};

use anyhow::Result;
use itertools::Itertools;

type Point = (isize, isize);
type Grid = HashMap<Point, char>;

fn solve(grid: &Grid, get_points: impl Fn(&Grid, Point, Point) -> Vec<Point>) -> usize {
  grid
    .values()
    .unique()
    .filter(|&&a| a != '.')
    .flat_map(|antenna| {
      let ps: Vec<_> = grid
        .iter()
        .filter_map(move |(p, a)| if a == antenna { Some(p) } else { None })
        .collect();
      ps.iter()
        .cartesian_product(&ps)
        .filter(|(p, np)| p != np)
        .flat_map(|(&&p, &&np)| get_points(grid, p, np))
        .filter(|p| grid.contains_key(p))
        .collect::<Vec<_>>()
    })
    .collect::<HashSet<_>>()
    .len()
}

fn n_dist(n: isize, (px, py): Point, (npx, npy): Point) -> Point {
  (px - n * (px - npx), py - n * (py - npy))
}

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day08.txt")?;
  let inp: HashMap<(isize, isize), char> = inp
    .lines()
    .enumerate()
    .flat_map(|(i, l)| {
      l.chars()
        .enumerate()
        .map(move |(j, c)| ((i as isize, j as isize), c))
        .collect::<Vec<_>>()
    })
    .collect();
  println!("{}", solve(&inp, |g, p, np| vec![n_dist(2, p, np)]));
  println!(
    "{}",
    solve(&inp, |g, p, np| (1..)
      .map(|i| n_dist(i, p, np))
      .take_while(|p| g.contains_key(&p))
      .collect())
  );
  Ok(())
}
