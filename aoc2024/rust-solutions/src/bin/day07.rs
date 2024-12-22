#![feature(try_blocks)]
use anyhow::Result;
use rayon::prelude::*;

fn eval<F>(goal: u64, xs: &[u64], ops: &[F]) -> bool
where
  F: Fn(u64, u64) -> u64,
{
  xs[1..]
    .iter()
    .fold(vec![xs[0]], |acc, x| {
      acc
        .iter()
        .flat_map(|y| ops.iter().map(|f| f(*x, *y)).collect::<Vec<_>>())
        .filter(|&x| x <= goal)
        .collect()
    })
    .contains(&goal)
}

fn main() -> Result<()> {
  let inp: Vec<(u64, Vec<u64>)> = std::fs::read_to_string("../inputs/day07.txt")?
    .lines()
    .map(|l| {
      let (a, b) = l.split_once(":").unwrap();
      (
        a.parse().unwrap(),
        b.trim().split(" ").flat_map(|x| x.parse()).collect(),
      )
    })
    .collect();

  let e1 = |(g, xs): &&(u64, Vec<u64>)| -> bool {
    eval(*g, xs, [std::ops::Add::add, std::ops::Mul::mul].as_ref())
  };
  println!("{}", inp.par_iter().filter(e1).map(|(g, _)| g).sum::<u64>());

  let e2 = |(g, xs): &&(u64, Vec<u64>)| {
    eval(
      *g,
      xs,
      [std::ops::Add::add, std::ops::Mul::mul, |x: u64, y: u64| {
        y * 10u64.pow(x.ilog10() + 1) + x
      }]
      .as_ref(),
    )
  };
  println!("{}", inp.par_iter().filter(e2).map(|(g, _)| g).sum::<u64>());
  Ok(())
}
