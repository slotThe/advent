use std::collections::{HashMap, HashSet};

use anyhow::Result;
use itertools::Itertools;
use nom::{bytes::complete::{tag, take_till}, combinator::map, multi::separated_list0, IResult};
use rust_aoc_util::parse;

pub fn day9() -> Result<(usize, usize)> {
  let mut hm: HashMap<(&str, &str), usize> = HashMap::new();
  let inp = std::fs::read_to_string("../inputs/day9.txt")?;
  inp
    .lines()
    .map(|l| parse(l, p_line).unwrap())
    .for_each(|((s, t), dist)| {
      hm.insert((s, t), dist);
      hm.insert((t, s), dist);
    });

  let names: HashSet<&str> = hm
    .clone()
    .into_keys()
    .map(|(a, b)| HashSet::from([a, b]))
    .concat();

  Ok(
    solve(names, hm)
      .iter()
      .minmax()
      .into_option()
      .map(|(&a, &b)| (a, b))
      .unwrap(),
  )
}

fn solve(inp: HashSet<&str>, hm: HashMap<(&str, &str), usize>) -> Vec<usize> {
  let mut res = vec![];
  inp.iter().permutations(inp.len()).for_each(|perm| {
    res.push(
      perm
        .iter()
        .tuple_windows()
        .fold(0, |acc, (&&a, &&b)| acc + hm[&(a, b)]),
    )
  });
  res
}

fn p_line(inp: &str) -> IResult<&str, ((&str, &str), usize)> {
  map(
    separated_list0(tag(" "), take_till(|c| c == ' ')),
    |xs: Vec<&str>| ((xs[0], xs[2]), xs[4].parse().unwrap()),
  )(inp)
}
