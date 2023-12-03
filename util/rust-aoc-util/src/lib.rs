pub mod coord;
pub mod fun;

use anyhow::Result;
use nom::{bytes::complete::tag, character::complete::multispace0, combinator::all_consuming, sequence::delimited, Finish, IResult};

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

///////////////////////////////////////////////////////////////////////
// Conversions

pub fn iter_to_num(v: impl IntoIterator<Item = char>) -> usize {
  v.into_iter().collect::<String>().parse().unwrap()
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
