use std::collections::HashMap;

use anyhow::Result;
use itertools::Itertools;

pub fn day5() -> Result<(usize, usize)> {
  let inp = std::fs::read_to_string("../inputs/day5.txt")?;
  Ok((
    inp.lines().filter(is_nice).count(),
    inp.lines().filter(is_nice2).count(),
  ))
}

fn is_nice(name: &&str) -> bool {
  // At least three vowels.
  name.chars().filter(|&c| "aeiou".contains(c)).count() >= 3
       // One letter that appears twice in a row.
    && name.chars().tuple_windows().any(|(c1, c2)| c1 == c2)
       // Doesn't contain ab, cd, pq, or xy.
    && name
      .chars()
      .tuple_windows()
      .all(|(c1, c2)| !["ab", "cd", "pq", "xy"].contains(&format!("{c1}{c2}").as_str()))
}

fn is_nice2(name: &&str) -> bool {
  // Some: no duplicate found yet (with index); None: duplicate found.
  let mut pairs: HashMap<String, Option<usize>> = HashMap::new();
  for (i, (c1, c2)) in name.chars().tuple_windows().enumerate() {
    pairs
      .entry(format!("{c1}{c2}"))
      .and_modify(|v| *v = v.and_then(|j| if i - j > 1 { None } else { Some(i) }))
      .or_insert(Some(i));
  }

  // Duplicates with one letter between them.
  name.chars().tuple_windows::<(_, _, _)>().any(|(c1, _, c3)| c1 == c3)
       // Non-overlapping duplicates.
    && pairs.into_values().any(|v| v.is_none())
}
