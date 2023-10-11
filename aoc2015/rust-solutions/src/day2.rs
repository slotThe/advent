use std::{cmp::min, num::ParseIntError, str::FromStr};

use anyhow::Result;

#[derive(Debug)]
pub struct Crate {
  l: usize,
  w: usize,
  h: usize,
}

impl FromStr for Crate {
  type Err = ParseIntError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let lwh: Vec<usize> = s.split('x').map(|c| c.parse()).collect::<Result<_, _>>()?;
    Ok(Crate {
      l: lwh[0],
      w: lwh[1],
      h: lwh[2],
    })
  }
}

impl Crate {
  fn vol(&self) -> usize { self.l * self.w * self.h }

  fn min_perim(&self) -> usize {
    vec![
      2 * self.l + 2 * self.w,
      2 * self.h + 2 * self.w,
      2 * self.h + 2 * self.l,
    ]
    .into_iter()
    .min()
    .unwrap()
  }

  fn paper(&self) -> usize {
    let a = 2 * self.l * self.w;
    let b = 2 * self.w * self.h;
    let c = 2 * self.h * self.l;
    a + b + c + min(a, min(b, c)) / 2
  }
}

pub fn day2() -> Result<(usize, usize)> {
  let crates: Vec<Crate> = std::fs::read_to_string("../inputs/day2.txt")?
    .lines()
    .flat_map(|s| s.parse::<Crate>())
    .collect();
  Ok((
    crates.iter().map(|s| s.paper()).sum(),
    crates.iter().map(|s| s.vol() + s.min_perim()).sum(),
  ))
}
