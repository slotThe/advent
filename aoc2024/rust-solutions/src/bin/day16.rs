use std::collections::HashMap;

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::{coord::{self, Coord, Dir}, dijkstra};

fn get_val(grid: &HashMap<Coord, char>, v: char) -> Coord {
  *grid
    .iter()
    .filter_map(|(p, v2)| if *v2 == v { Some(p) } else { None })
    .next()
    .unwrap()
}

fn neighs(p: Coord, d: Dir, grid: &HashMap<Coord, char>) -> Vec<((Coord, Dir), usize)> {
  [Dir::North, Dir::East, Dir::South, Dir::West]
    .into_iter()
    .map(|d2| {
      if d == d2 {
        ((p.move_in(d), d), 1)
      } else {
        ((p, d2), 1000)
      }
    })
    .filter(|((p, _), _)| grid.contains_key(p))
    .collect()
}

fn main() -> Result<()> {
  let inp: HashMap<Coord, char> = std::fs::read_to_string("../inputs/day16.txt")?
    .lines()
    .enumerate()
    .flat_map(|(i, l)| {
      l.chars()
        .enumerate()
        .filter_map(|(j, c)| {
          if c == '#' {
            None // Don't care about walls at all
          } else {
            Some((coord::from_pair((j as i32, i as i32)), c))
          }
        })
        .collect_vec()
    })
    .collect();

  let d_fw = dijkstra(&[(get_val(&inp, 'S'), Dir::East)], |&(p, d)| {
    neighs(p, d, &inp)
  });
  let end = get_val(&inp, 'E');
  let min = *d_fw
    .iter()
    .filter_map(|((p, _), n)| if *p == end { Some(n) } else { None })
    .min()
    .unwrap();
  assert_eq!(min, 105508);
  println!("{min}");

  let d_bw = dijkstra(
    &d_fw // valid endings
      .iter()
      .filter_map(|(&(p, d), &n)| {
        if p == end && n == min {
          Some((p, d))
        } else {
          None
        }
      })
      .collect_vec(),
    |&(p, d)| neighs(p, d, &inp),
  );

  let two = d_fw
    .iter()
    .flat_map(|(&(p, d), &v)| {
      neighs(p, d, &inp)
        .into_iter()
        .filter(|((np, nd), w)| v + d_bw.get(&(*np, *nd)).unwrap() - w == min)
        .collect_vec()
    })
    .unique_by(|((p, _), _)| *p)
    .count();
  assert_eq!(two, 548);
  println!("{two}");

  Ok(())
}
