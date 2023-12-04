use std::collections::HashSet;

pub fn day04() -> (i32, i32) {
  let inp = std::fs::read_to_string("../inputs/day04.txt").unwrap();
  let inp: Vec<_> = inp.lines().collect();
  let mult_wins = (1..=inp.len())
    .map(|n| (1, p_input(inp[n - 1])))
    .collect::<Vec<(i32, usize)>>();
  (
    mult_wins
      .iter()
      .map(|(_, n)| 2_f32.powi((n - 1) as i32) as i32)
      .sum(),
    part2(mult_wins),
  )
}

fn part2(inp: Vec<(i32, usize)>) -> i32 {
  let mut res = inp;
  for i in 0..res.len() {
    let (mult, correct_guesses) = res[i];
    for win in (i + 1..).take(correct_guesses) {
      res[win].0 += mult;
    }
  }
  res.iter().map(|(m, _)| m).sum()
}

fn p_input(inp: &str) -> usize {
  let p_integers = |xs: &str| {
    xs.split(' ')
      .flat_map(|s| s.parse())
      .collect::<HashSet<i32>>()
  };
  let inp: Vec<&str> = inp.split('|').collect();
  p_integers(inp[0]).intersection(&p_integers(inp[1])).count()
}
