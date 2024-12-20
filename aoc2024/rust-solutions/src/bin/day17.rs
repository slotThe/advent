#![feature(iter_intersperse)]
use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::slurp_nums;

fn combo(a: usize, b: usize, c: usize, i: usize) -> usize {
  match i {
    4 => a,
    5 => b,
    6 => c,
    i => i,
  }
}

fn sim(
  (mut a, mut b, mut c): (usize, usize, usize),
  ops: &[(&usize, &usize)],
) -> Vec<usize> {
  let mut res = vec![];
  let mut p = 0;
  let mut o = 1;
  while p < ops.len() {
    match ops[p] {
      (0, i) => a >>= combo(a, b, c, *i),
      (1, i) => b ^= i,
      (2, i) => b = combo(a, b, c, *i) & 7,
      (3, i) => {
        if a != 0 {
          o = *i;
        };
      },
      (4, _) => b ^= c,
      (5, i) => res.push(combo(a, b, c, *i) & 7),
      (6, i) => b = a >> combo(a, b, c, *i),
      (7, i) => c = a >> combo(a, b, c, *i),
      _ => panic!("unreachable"),
    }
    p = o;
    o = p + 1;
  }
  res
}

fn find_quine(mut prog: Vec<usize>) -> usize {
  let mut prevs = vec![0];
  loop {
    if prog.is_empty() {
      return *prevs.iter().min().unwrap();
    }
    prevs = prevs
      .iter()
      .flat_map(|prev| {
        (0..8)
          .filter_map(|i| {
            // Shift to next three bits.
            let now = prev << 3 | i;
            // Hand-optimised from input, but I guess one could also run one
            // step of `sim`.
            if prog[prog.len() - 1] == (now & 7 ^ 1) ^ (now >> (now & 7 ^ 2)) & 7 {
              Some(now)
            } else {
              None
            }
          })
          .collect_vec()
      })
      .collect_vec();
    prog.pop();
  }
}

fn main() -> Result<()> {
  let inp: Vec<usize> =
    slurp_nums::<i32>(&std::fs::read_to_string("../inputs/day17.txt")?)
      .unwrap()
      .iter()
      .map(|i| *i as usize)
      .collect();

  let one: String = Iterator::intersperse(
    sim(
      (inp[0], inp[1], inp[2]),
      &inp[3..].iter().tuple_windows().step_by(2).collect_vec(),
    )
    .iter()
    .map(|x| format!("{x}")),
    ",".to_string(),
  )
  .collect();
  assert_eq!(one, "7,1,5,2,4,0,7,6,1");
  println!("{one}");

  let two = find_quine(inp[3..].to_vec());
  assert_eq!(two, 37222273957364);
  println!("{two}");
  Ok(())
}
