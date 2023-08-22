use std::collections::HashSet;

use anyhow::Result;
use itertools::{iterate, Itertools};
use rust_aoc_util::parse_single_line;

pub fn day11() -> Result<(String, String)> {
  let inp = parse_single_line("../inputs/day11.txt")?;
  let res: Vec<String> = iterate(inp, inc_num)
    // We already check for forbidden words during string generation,
    // so no need to do that here.
    .filter(|s| two_pairs(s) && increasing3(s))
    .take(2)
    .collect();
  Ok((res[0].clone(), res[1].clone()))
}

fn two_pairs(s: &str) -> bool {
  let mut seen: HashSet<char> = HashSet::new();
  s.chars().tuple_windows().for_each(|(a, b)| {
    if a == b && !seen.contains(&a) {
      seen.insert(a);
    }
  });
  seen.len() >= 2
}

fn increasing3(s: &str) -> bool {
  let following = |c1, c2| 1 == c2 as i8 - c1 as i8;
  s.chars()
    .tuple_windows()
    .any(|(a, b, c)| following(a, b) && following(b, c))
}

// *sad immutability noises*
fn inc_num(s: &String) -> String {
  let forbidden = |c| "iol".contains(c as char);
  let mut res = s.as_bytes().to_vec();
  let mut changed = true;
  for i in (0..res.len()).rev() {
    let c = res[i];
    if !changed {
      break;
    } else if c == b'z' {
      res[i] = b'a';
    } else {
      changed = false;
      let next = if forbidden(c + 1) { c + 2 } else { c + 1 };
      res[i] = next;
    }
  }
  res.iter().map(|c| *c as char).collect()
}
