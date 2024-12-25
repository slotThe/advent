#![feature(ascii_char)]
use anyhow::Result;
use itertools::{Either, Itertools};
use rust_aoc_util::print_day;

fn transpose_count(xs: &[&str]) -> Vec<usize> {
  (0..xs[0].len())
    .map(|i| {
      xs.iter()
        .map(|&row| row.as_ascii().unwrap()[i])
        .filter(|c| c == &'#'.as_ascii().unwrap())
        .count()
    })
    .collect()
}

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day25.txt")?;
  let (keys, locks): (Vec<Vec<usize>>, Vec<Vec<usize>>) = inp
    .split("\n\n")
    .partition_map(|g| match &g.lines().collect_vec()[..] {
      ["#####", xs @ ..] => Either::Right(transpose_count(xs)),
      xs => Either::Left(transpose_count(xs.split_last().unwrap().1)),
    });
  let two = keys
    .iter()
    .map(|k| {
      locks
        .iter()
        .filter(|l| l.iter().zip(k).map(|(a, b)| a + b).all(|n| n <= 5))
        .count()
    })
    .sum::<usize>();
  assert_eq!(3287, two);
  print_day(25, (two, "Merry Christmas!"));
  Ok(())
}
