use itertools::Itertools;
use rust_aoc_util::iter_to_num;

pub fn day03() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day03.txt").unwrap();
  let char_map: Vec<(char, Vec<usize>)> =
    solve(inp.lines().map(|l| l.chars().collect()).collect());

  (
    char_map
      .iter()
      .map(|(_, v)| v.iter().sum::<usize>())
      .sum::<usize>(),
    char_map
      .iter()
      .filter_map(|(s, v)| {
        if *s == '*' && v.len() == 2 {
          Some(v.iter().product::<usize>())
        } else {
          None
        }
      })
      .sum::<usize>(),
  )
}

fn solve(inp: Vec<Vec<char>>) -> Vec<(char, Vec<usize>)> {
  let width = inp.len();
  let height = inp[0].len();
  let is_digit = |i: usize, j: usize| -> bool {
    inp
      .get(i)
      .and_then(|row| row.get(j))
      .is_some_and(|c| c.is_ascii_digit())
  };
  let mut res: Vec<(char, Vec<usize>)> = vec![];

  (0..width)
    .cartesian_product(0..height)
    .filter(|&(i, j)| !(inp[i][j] == '.' || is_digit(i, j))) // is_symbol
    .for_each(|(i, j)| {
      let mut sym_nums: Vec<Vec<(usize, usize)>> = vec![];
      neighbours8(i, j)
        .filter(|&(k, l)| is_digit(k, l))
        .for_each(|(k, l)| {
          // Find start (leftmost position) of number.
          let start_of_num = (0..)
            .take_while(|offset| is_digit(k, l - offset))
            .last()
            .unwrap();
          // Save *coordinates of* the number.
          sym_nums.push(
            (0..)
              .map(|offset| (k, l - start_of_num + offset))
              .take_while(|&(x, y)| is_digit(x, y))
              .collect(),
          );
        });
      // Dedup the list only for a single symbol. The same symbol might
      // be connected to two different "versions" of the same number,
      // which we would be unable to see in the completed list.
      sym_nums.dedup();
      res.push((
        inp[i][j], // symbol
        sym_nums // value of number
          .iter()
          .map(|v| iter_to_num(v.iter().map(|&(x, y)| inp[x][y])))
          .collect(),
      ));
    });

  res
}

fn neighbours8(i: usize, j: usize) -> impl Iterator<Item = (usize, usize)> {
  vec![
    (i, j + 1),
    (i, j - 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1),
    (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
  ]
  .into_iter()
}
