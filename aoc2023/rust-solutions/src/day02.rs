use itertools::any;
use lazy_static::lazy_static;
use regex::Regex;

pub fn day02() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day02.txt").unwrap();
  let inp = inp.lines().map(|s| parse(s));
  (part1(inp.clone()), part2(inp))
}

fn parse(inp: &str) -> Vec<(&str, usize)> {
  lazy_static! {
    static ref RE: Regex = Regex::new(r"(?:(\d+) (\w+))").unwrap();
  }
  RE.captures_iter(inp)
    .map(|cap| {
      (
        cap.get(2).unwrap().as_str(),
        cap.get(1).unwrap().as_str().parse::<usize>().unwrap(),
      )
    })
    .collect()
}

fn part1<'a>(inp: impl Iterator<Item = Vec<(&'a str, usize)>>) -> usize {
  let impossible = |(colour, amount)| match colour {
    "red" => amount > 12,
    "green" => amount > 13,
    "blue" => amount > 14,
    _ => true,
  };
  inp
    .zip(1..)
    .filter_map(|(cs, i)| if any(cs, impossible) { None } else { Some(i) })
    .sum()
}

fn part2<'a>(inp: impl Iterator<Item = Vec<(&'a str, usize)>>) -> usize {
  let max_col = |colour: &str, colours: &[(&str, usize)]| {
    colours
      .iter()
      .filter(|(s, _)| colour.eq(*s))
      .max()
      .unwrap()
      .1
  };
  inp
    .map(|cs| max_col("red", &cs) * max_col("green", &cs) * max_col("blue", &cs))
    .sum()
}
