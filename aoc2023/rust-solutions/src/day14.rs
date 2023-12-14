use rust_aoc_util::{detect_cycle, transpose, Cycle};

pub fn day14() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day14.txt").unwrap();
  let inp: Vec<Vec<_>> = inp.lines().map(|l| l.chars().collect()).collect();

  let (mut cyc, Cycle { pre, len }) = detect_cycle(|v| roll(v.as_ref()), inp.clone());
  let diff = (1000000000 - pre) % (len - pre);
  for _ in 0..diff {
    cyc = roll(&cyc);
  }

  (calc_score(roll_north(&inp)), calc_score(cyc))
}

// Express all rolls in terms of stones rolling north.
fn roll(inp: &[Vec<char>]) -> Vec<Vec<char>> {
  let north = roll_north(inp);
  let west = transpose(roll_north(&transpose(north)));
  let south: Vec<_> = roll_north(west.into_iter().rev().collect::<Vec<_>>().as_ref())
    .into_iter()
    .rev()
    .collect();
  let east = transpose(roll_north(&transpose(
    south
      .iter()
      .map(|l| l.iter().rev().copied().collect::<Vec<_>>())
      .collect(),
  )));
  east
    .iter()
    .map(|l| l.iter().rev().copied().collect::<Vec<_>>())
    .collect()
}

fn roll_north(inp: &[Vec<char>]) -> Vec<Vec<char>> {
  let mut res = inp.to_owned();
  for i in 0..res.len() {
    for j in 0..res[i].len() {
      if res[i][j] == 'O' {
        match (0..i).rev().take_while(|&ix| res[ix][j] == '.').last() {
          None => {},
          Some(ix) => {
            res[ix][j] = 'O';
            res[i][j] = '.';
          },
        }
      }
    }
  }
  res
}

fn calc_score(inp: Vec<Vec<char>>) -> usize {
  inp
    .iter()
    .rev()
    .enumerate()
    .flat_map(|(i, l)| l.iter().map(move |s| if 'O'.eq(s) { i + 1 } else { 0 }))
    .sum()
}
