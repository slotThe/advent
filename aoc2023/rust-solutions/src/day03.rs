use itertools::Itertools;
use rust_aoc_util::iter_to_num;

pub fn day03() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day03.txt").unwrap();
  let inp: Vec<(char, Vec<usize>)> =
    solve(inp.lines().map(|l| l.chars().collect()).collect());

  (
    inp
      .iter()
      .map(|(_, v)| v.iter().sum::<usize>())
      .sum::<usize>(),
    inp
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
  let mut res: Vec<(char, Vec<usize>)> = vec![];
  (0..width)
    .cartesian_product(0..height)
    .filter(|(i, j)| !(inp[*i][*j] == '.' || inp[*i][*j].is_ascii_digit()))
    .for_each(|(i, j)| {
      let mut sym_nums: Vec<Vec<(usize, usize)>> = vec![];
      neighbours8(i, j)
        .into_iter()
        .filter(|(k, l)| inp[*k][*l].is_ascii_digit())
        .for_each(|(k, l)| {
          // Find start of number.
          let start_of_num = (0..)
            .take_while(|off| is_digit(&inp, k, l - off))
            .last()
            .unwrap();
          // Save *coordinates of* the number.
          sym_nums.push(
            (0..)
              .map(|off| (k, l - start_of_num + off))
              .take_while(|(rk, rl)| is_digit(&inp, *rk, *rl))
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
          .map(|v| iter_to_num(v.iter().map(|(x, y)| inp[*x][*y])))
          .collect(),
      ));
    });
  res
}

fn neighbours8(i: usize, j: usize) -> Vec<(usize, usize)> {
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
}

fn is_digit(v: &[Vec<char>], i: usize, j: usize) -> bool {
  v.get(i)
    .and_then(|v2| v2.get(j))
    .is_some_and(|d| d.is_ascii_digit())
}
