use itertools::iterate;

pub fn day09() -> (isize, isize) {
  std::fs::read_to_string("../inputs/day09.txt")
    .unwrap()
    .lines()
    .map(|l| {
      l.split(' ')
        .flat_map(|d| d.parse::<isize>())
        .collect::<Vec<_>>()
    })
    .fold((0, 0), |(acc1, acc2), v| {
      let (a, b) = solve(&v);
      (acc1 + a, acc2 + b)
    })
}

fn solve(reading: &[isize]) -> (isize, isize) {
  mk_sequences(reading.to_vec())
    .iter()
    .rev()
    .fold((0, 0), |(a1, b1), (a2, b2)| (a1 + a2, b2 - b1))
}

fn mk_sequences(reading: Vec<isize>) -> Vec<(isize, isize)> {
  iterate(reading, |r| {
    r.windows(2).map(|ch| ch[1] - ch[0]).collect::<Vec<_>>()
  })
  .take_while(|r| !r.iter().all(|&d| d == 0))
  .map(|v| (v[v.len() - 1], v[0]))
  .collect()
}
