use std::str::pattern::Pattern;

pub fn day01() -> (usize, usize) {
  let solve = |repls: &[(&str, usize)]| {
    std::fs::read_to_string("../inputs/day01.txt")
      .unwrap()
      .lines()
      .map(|s| get_extremes(repls, s))
      .sum()
  };
  (
    solve(&REPLACEMENTS1),
    solve(
      &[
        REPLACEMENTS1,
        [
          ("one", 1),
          ("two", 2),
          ("three", 3),
          ("four", 4),
          ("five", 5),
          ("six", 6),
          ("seven", 7),
          ("eight", 8),
          ("nine", 9),
        ],
      ]
      .concat(),
    ),
  )
}

fn get_extremes(repls: &[(&str, usize)], inp: &str) -> usize {
  let find_first = {
    let mut s = inp.to_string();
    loop {
      match repls.iter().find(|(a, _)| a.is_prefix_of(&s)) {
        None => s.remove(0),
        Some((_, b)) => break *b,
      };
    }
  };

  let find_last = {
    let mut s = inp.to_string();
    loop {
      match repls.iter().find(|(a, _)| a.is_suffix_of(&s)) {
        None => s.pop(),
        Some((_, b)) => break *b,
      };
    }
  };

  find_first * 10 + find_last
}

static REPLACEMENTS1: [(&str, usize); 9] = [
  ("1", 1),
  ("2", 2),
  ("3", 3),
  ("4", 4),
  ("5", 5),
  ("6", 6),
  ("7", 7),
  ("8", 8),
  ("9", 9),
];
