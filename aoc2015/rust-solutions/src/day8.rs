use anyhow::Result;
use nom::{branch::alt, bytes::complete::tag, character::complete::{alpha1, anychar}, combinator::map, multi::{count, many0}, sequence::{delimited, terminated}, IResult};
use rust_aoc_util::parse;

pub fn day8() -> Result<(usize, usize)> {
  Ok(
    std::fs::read_to_string("../inputs/day8.txt")?
      .lines()
      .map(|x| {
        (
          x.len() - parse(x, p_string).unwrap(),
          x.escape_default().count() + 2 - x.len(),
        )
      })
      .fold((0, 0), |(a, b), (x, y)| (a + x, b + y)),
  )
}

fn p_special(inp: &str) -> IResult<&str, usize> {
  map(
    alt((
      tag("\\\\"),
      tag("\\\""),
      terminated(tag("\\x"), count(anychar, 2)),
    )),
    |_| 1,
  )(inp)
}

fn p_string(inp: &str) -> IResult<&str, usize> {
  delimited(
    tag("\""),
    map(
      many0(alt((p_special, map(alpha1, |x: &str| x.len())))),
      |vs| vs.into_iter().sum(),
    ),
    tag("\""),
  )(inp)
}
