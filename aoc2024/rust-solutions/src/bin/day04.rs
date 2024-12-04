use std::collections::HashMap;

use rust_aoc_util::print_day;

#[rustfmt::skip]
fn dirs() -> impl Iterator<Item = (isize, isize)> {
  [ (1, 0), (0, 1), (-1, 0), (0, -1), (-1, -1), (1, 1), (-1, 1), (1, -1) ].into_iter()
}

#[rustfmt::skip]
fn star() -> impl Iterator<Item = (isize, isize)> {
  [ (1, -1), (1, 1), (0, 0), (-1, -1), (-1, 1) ].into_iter()
}

fn one(grid: &HashMap<(isize, isize), char>) -> usize {
  grid.keys().fold(0, |acc, (x, y)| {
    acc
      + dirs()
        .filter(|(dx, dy)| {
          "XMAS"
            == (0..=3)
              .flat_map(|i| grid.get(&(x + i * dx, y + i * dy)))
              .collect::<String>()
        })
        .count()
  })
}

fn two(grid: &HashMap<(isize, isize), char>) -> usize {
  grid
    .keys()
    .filter(|(x, y)| {
      ["MSAMS", "SSAMM", "MMASS", "SMASM"]
        .map(String::from)
        .contains(
          &star()
            .flat_map(|(dx, dy)| grid.get(&(x + dx, y + dy)))
            .collect::<String>(),
        )
    })
    .count()
}

fn main() {
  let inp: HashMap<(isize, isize), char> = std::fs::read_to_string("../inputs/day04.txt")
    .unwrap()
    .lines()
    .enumerate()
    .flat_map(|(i, l)| {
      l.chars()
        .enumerate()
        .map(|(j, c)| ((i as isize, j as isize), c))
        .collect::<Vec<((isize, isize), char)>>()
    })
    .collect();
  let one = one(&inp);
  let two = two(&inp);
  assert!(one == 2493);
  assert!(two == 1890);
  print_day(4, (one, two));
}
