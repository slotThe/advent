pub mod coord;
pub mod fun;
pub mod interval;

use anyhow::Result;
use nom::{bytes::complete::tag, character::complete::multispace0, combinator::all_consuming, sequence::delimited, Finish, IResult};
use std::hash::Hash;
use num::Num;
use priority_queue::DoublePriorityQueue;
use std::{collections::{HashMap, HashSet}};

///////////////////////////////////////////////////////////////////////
// Parsing

pub fn parse_single_line(fp: &str) -> std::io::Result<String> {
  std::fs::read_to_string(fp).map(|s| s.trim_end().to_string())
}

pub fn parse<'a, R, F>(inp: &'a str, parser: F) -> Result<R, nom::error::Error<&'a str>>
where
  F: Fn(&'a str) -> nom::IResult<&'a str, R>,
{
  all_consuming(parser)(inp).finish().map(|(_, r)| r)
}

// Lift a value into the parser context; like `pure` in Haskell.
pub fn pure<'a, T: 'a + Clone>(t: T) -> impl Fn(&'a str) -> IResult<&'a str, T> {
  move |i| Ok((i, t.clone()))
}

pub fn wtag<'a>(s: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
  delimited(multispace0, tag(s), multispace0)
}

///////////////////////////////////////////////////////////////////////
// Matrix util

pub fn inp_to_grid<T: Clone>(parse: impl Fn(char) -> Option<T>, inp: &str) -> Vec<Vec<T>>{
  transpose(
    inp
      .lines()
      .map(|l| {
        l.chars()
          .flat_map(|c| parse(c))
          .collect()
      })
      .collect()
  )
}

pub fn transpose<El: Clone>(xxs: Vec<Vec<El>>) -> Vec<Vec<El>> {
  transpose_with(|v| v, xxs)
}

pub fn transpose_with<I: Clone, O, F>(f: F, xxs: Vec<Vec<I>>) -> Vec<Vec<O>>
where
  F: Fn(Vec<I>) -> Vec<O>,
{
  (0..xxs[0].len())
    .map(|i| xxs.iter().map(|row| row[i].clone()).collect())
    .map(f)
    .collect()
}

pub fn grid_get<T>(grid: &[Vec<T>], x: usize, y: usize) -> Option<&T> {
  grid.get(x).and_then(|v| v.get(y))
}

///////////////////////////////////////////////////////////////////////
// Conversions

pub fn iter_to_num(v: impl IntoIterator<Item = char>) -> usize {
  v.into_iter().collect::<String>().parse().unwrap()
}

///////////////////////////////////////////////////////////////////////
// Algorithms

pub fn gcd<N: Num + Copy>(a: N, b: N) -> N {
  let mut a = a;
  let mut b = b;
  loop {
    if b == N::zero() {
      return a;
    } else {
      let tmp = a;
      a = b;
      b = tmp % b;
    }
  }
}

pub fn lcm<N: Num + Copy>(a: N, b: N) -> N {
  b * (a.div(gcd(a, b)))
}

pub struct Cycle {
  pub pre: usize,
  pub len: usize,
}

pub fn detect_cycle<T: Clone + Eq + Hash>(
  f: impl Fn(&T) -> T,
  inp: T,
) -> (T, Cycle) {
  let mut cyc = inp;
  let mut i = 0;
  let mut seen: HashMap<T, usize> = HashMap::from([(cyc.clone(), i)]);
  loop {
    i += 1;
    cyc = f(&cyc);
    if let Some(&pre) = seen.get(&cyc) {
      return (cyc, Cycle{pre, len: i});
    } else {
      seen.insert(cyc.clone(), i);
    }
  }
}

pub fn dijkstra<N, P>(starts: &[P], more: impl Fn(&P) -> Vec<(P, N)>) -> HashMap<P, N>
where
  P: Ord + Hash + Copy,
  N: Num + Ord + Copy,
{
  let mut all_costs = HashMap::new();
  let mut seen = HashSet::new();
  let mut queue = DoublePriorityQueue::new();
  for start in starts {
    queue.push(*start, N::zero());
  }
  loop {
    if let Some((u, cost_u)) = queue.pop_min() {
      if !seen.contains(&u) {
        all_costs.insert(u, cost_u); // final, minimal, cost of a point
        seen.insert(u);
        for &(n, cost_n) in more(&u).iter() {
          if let Some(&p) = queue.get_priority(&n) {
            queue.push(n, p.min(cost_u + cost_n));
          } else {
            queue.push(n, cost_u + cost_n);
          }
        }
      }
    } else {
      return all_costs;
    }
  }
}

///////////////////////////////////////////////////////////////////////
// Pretty printing

pub fn print_day<A, B>(num: usize, solutions: (A, B))
where
  A: AdventString,
  B: AdventString,
{
  println!("!!! Day {num} !!!");
  println!("First  task: {}", solutions.0.pp());
  println!("Second task: {}\n", solutions.1.pp());
}

pub trait AdventString {
  fn pp(&self) -> String;
}

impl<A: ToString> AdventString for Option<A> {
  fn pp(&self) -> String {
    match self {
      None => "None".to_string(),
      Some(x) => x.to_string(),
    }
  }
}

impl<A: ToString> AdventString for Result<A> {
  fn pp(&self) -> String {
    match self {
      Err(e) => e.to_string(),
      Ok(x) => x.to_string(),
    }
  }
}

// Yup, this is really necessary, because apparently
//
//     impl<A: ToString> AdventString for A
//
// doesn't work and triggers E0119. Yikes.

impl AdventString for String {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for i32 {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for u16 {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for u32 {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for u128 {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for usize {
  fn pp(&self) -> String { self.to_string() }
}

impl AdventString for isize {
  fn pp(&self) -> String { self.to_string() }
}
