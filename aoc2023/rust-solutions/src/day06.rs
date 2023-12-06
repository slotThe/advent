use itertools::Itertools;

pub fn day06() -> (isize, isize) {
  let inp = std::fs::read_to_string("../inputs/day06.txt").unwrap();
  let inp: Vec<_> = inp.lines().collect();
  (
    parse1(&inp).into_iter().map(solve).product(),
    solve(parse2(&inp)),
  )
}

fn solve((time, distance): (isize, isize)) -> isize {
  let new_best = |t| t * (time - t) > distance;
  let mid = time.div_floor(2);
  binary_search(|t| !new_best(t), mid, time) - binary_search(new_best, 0, mid)
}

fn binary_search(pred: impl Fn(isize) -> bool, low: isize, high: isize) -> isize {
  let mut l = low;
  let mut r = high;
  loop {
    let mid = (r + l).div_floor(2);
    if l == r {
      return l;
    } else if pred(mid) {
      r = mid;
    } else {
      l = mid + 1;
    }
  }
}

fn parse1(inp: &[&str]) -> Vec<(isize, isize)> {
  let res: Vec<Vec<_>> = inp
    .iter()
    .map(|s| s.split(' ').flat_map(|w| w.parse()).collect())
    .collect();
  res[0]
    .iter()
    .zip(res[1].iter())
    .map(|(&a, &b)| (a, b))
    .collect()
}

fn parse2(inp: &[&str]) -> (isize, isize) {
  let res: Vec<_> = inp
    .iter()
    .flat_map(|s| s.split(' ').dropping(1).collect::<String>().parse())
    .collect();
  (res[0], res[1])
}
