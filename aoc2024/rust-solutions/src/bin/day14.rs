use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::{coord::{self, Coord}, slurp_nums};

fn visualise(xmax: Option<i32>, ymax: Option<i32>, grid: &HashSet<Coord>) {
  let xmax = xmax.unwrap_or(1 + grid.iter().max_by_key(|c| c.x).unwrap().x);
  let ymax = ymax.unwrap_or(1 + grid.iter().max_by_key(|c| c.y).unwrap().y);
  for i in 0..ymax {
    for j in 0..xmax {
      if grid.contains(&coord::from_pair((j, i))) {
        print!("█");
      } else {
        print!(".")
      }
    }
    println!();
  }
}

fn strict_quadrant(bx: i32, by: i32, c: Coord) -> Option<usize> {
  if c.x < bx && c.y < by {
    Some(0)
  } else if c.x < bx && c.y > by {
    Some(1)
  } else if c.x > bx && c.y < by {
    Some(2)
  } else if c.x > bx && c.y > by {
    Some(3)
  } else {
    None
  }
}

fn main() -> Result<()> {
  let inp: Vec<(Coord, Coord)> = std::fs::read_to_string("../inputs/day14.txt")?
    .lines()
    .map(|l| {
      let v = slurp_nums(l).unwrap();
      (
        coord::from_pair((v[0], v[1])),
        coord::from_pair((v[2], v[3])),
      )
    })
    .collect_vec();
  let bounds = coord::from_pair((101, 103));

  println!(
    "{}",
    &inp
      .iter()
      .map(|(p, v)| (*p + v.cmul(100)).rem_euclid(&bounds))
      .filter_map(|c| strict_quadrant(bounds.x / 2, bounds.y / 2, c))
      .fold(vec![0, 0, 0, 0], |mut acc, n| {
        acc[n] += 1;
        acc
      })
      .iter()
      .product::<i32>()
  );

  println!("7709"); // Just guessing :)
  visualise(
    Some(bounds.x),
    Some(bounds.y),
    &inp
      .iter()
      .map(|(p, v)| (*p + v.cmul(7709)).rem_euclid(&bounds))
      .collect(),
  );

  Ok(())
}
