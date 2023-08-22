use anyhow::Result;
use itertools::{iterate, Itertools};
use rust_aoc_util::parse_single_line;

pub fn day10() -> Result<(usize, usize)> {
  let inp = parse_single_line("../inputs/day10.txt")?;
  let res: Vec<usize> = iterate(inp, |i| play(i.clone()))
    .map(|v| v.len())
    .take(51)
    .collect();
  Ok((res[40], res[50]))
}

fn play(inp: String) -> String {
  let mut res: Vec<(char, String)> = vec![];
  for (c, s) in &inp.chars().group_by(|x| *x) {
    res.push((c, s.collect()))
  }
  res
    .iter()
    .map(|(c, s)| format!("{}", s.len()) + &c.to_string())
    .collect()
}
