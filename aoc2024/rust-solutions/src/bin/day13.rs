use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::slurp_nums;

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
      let v = slurp_nums(l).unwrap();
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
