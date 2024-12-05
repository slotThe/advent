use std::cmp::Ordering;

use anyhow::Result;

fn not_isect((a, b): &(u32, u32), xs: &[u32]) -> bool {
  xs.iter().position(|x| x == a).map_or(true, |ia| {
    xs.iter().position(|x| x == b).map_or(true, |ib| ia < ib)
  })
}

fn compare(ords: &[(u32, u32)], a: &u32, b: &u32) -> Ordering {
  // (x, y) ∈ ords means that x must come before y.
  for (x, y) in ords {
    if (a, b) == (x, y) {
      return Ordering::Less;
    }
    if (a, b) == (y, x) {
      return Ordering::Greater;
    }
  }
  Ordering::Equal
}

fn main() -> Result<()> {
  let (ord, upd): (Vec<(u32, u32)>, Vec<Vec<u32>>) =
    std::fs::read_to_string("../inputs/day05.txt")?
      .trim()
      .split_once("\n\n")
      .map(|(ord, upd)| {
        (
          ord
            .split("\n")
            .map(|o| {
              let v: Vec<u32> = o.split("|").flat_map(|n| n.parse()).collect();
              (v[0], v[1])
            })
            .collect(),
          upd
            .split("\n")
            .map(|o| o.split(",").flat_map(|n| n.parse()).collect())
            .collect(),
        )
      })
      .unwrap();

  let (good, mut bad): (Vec<Vec<u32>>, Vec<Vec<u32>>) = upd
    .into_iter()
    .partition(|u| ord.iter().all(|o| not_isect(o, u)));

  let one: u32 = good.iter().map(|xs| xs[xs.len() / 2]).sum();
  assert!(one == 4578);
  println!("{one}");

  for u in &mut bad {
    u.sort_by(|a, b| compare(&ord, a, b));
  }
  let two: u32 = bad.iter().map(|xs| xs[xs.len() / 2]).sum();
  assert!(two == 6179);
  println!("{two}");

  Ok(())
}
