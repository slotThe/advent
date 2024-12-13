use anyhow::Result;
use itertools::Itertools;
use nom::{self, branch::alt, character::{self, complete::none_of}, combinator::map, multi::many1};
use rust_aoc_util::parse;

fn p_ints(s: &str) -> Result<Vec<f64>, nom::error::Error<&str>> {
  parse(s, |s| {
    many1(alt((
      map(character::complete::i32, |i| Some(i as f64)),
      map(many1(none_of("-0123456789")), |_| None),
    )))(s)
  })
  .map(|v| v.into_iter().flatten().collect())
}

type Point = (f64, f64);
type Mat = (f64, f64, f64, f64); // 2×2

fn solve((a, b, c, d): Mat, (gx, gy): Point) -> u64 {
  let det = a * d - b * c;
  if det.abs() < 0.01 {
    0
  } else {
    let (a, b) = ((d * gx - c * gy) / det, (a * gy - b * gx) / det);
    if a.fract() < 0.01 && b.fract() < 0.01 {
      (3.0 * a + b) as u64
    } else {
      0
    }
  }
}

const C: f64 = 10_000_000_000_000.0;

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day13.txt")?;
  let inp = inp
    .split("\n\n")
    .map(|l| {
      let v = p_ints(l).unwrap();
      ((v[0], v[1], v[2], v[3]), (v[4], v[5]))
    })
    .collect_vec();

  println!("{}", inp.iter().map(|(ab, g)| solve(*ab, *g)).sum::<u64>());
  println!(
    "{}",
    inp
      .iter()
      .map(|(ab, (gx, gy))| solve(*ab, (gx + C, gy + C)))
      .sum::<u64>()
  );

  Ok(())
}
