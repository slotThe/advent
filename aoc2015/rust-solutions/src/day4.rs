use anyhow::Result;
use std::str::pattern::Pattern;

pub fn day4() -> Result<(usize, usize)> {
  let inp = std::fs::read_to_string("../inputs/day4.txt")?;
  Ok((solve("00000", inp.trim()), (solve("000000", inp.trim()))))
}

fn solve(pfx: &str, inp: &str) -> usize {
  for i in 0.. {
    let digest: String = format!("{:x}", md5::compute(inp.to_string() + &i.to_string()));
    if pfx.is_prefix_of(&digest) {
      return i;
    }
  }
  panic!("2015/day4::solve: Unreachable")
}

