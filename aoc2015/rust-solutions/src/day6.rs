use anyhow::Result;
use nom::{branch::alt,
          bytes::complete::tag,
          character,
          combinator::map,
          sequence::{preceded, separated_pair},
          IResult};
use rust_aoc_util::{coord::{from_pair, range, Coord},
                    parse, pure};
use std::cmp::max;

pub fn day6() -> Result<(i32, i32)> {
  let input: Vec<Action> = std::fs::read_to_string("../inputs/day6.txt")?
    .lines()
    .map(|l| parse(l, p_action).ok().unwrap())
    .collect();
  Ok((simulate(input.clone(), Light::On), simulate(input, 0)))
}

fn simulate<T: IsLight + Copy>(inp: Vec<Action>, start: T) -> i32 {
  let mut grid: Vec<Vec<T>> = vec![vec![start; 1000]; 1000];
  for Action { cmd, c1, c2 } in inp {
    for (i, j) in range(c1, c2) {
      grid[i][j].flick_mut(cmd);
    }
  }
  grid
    .into_iter()
    .map(|v| v.into_iter().map(|l| l.points()).sum::<i32>())
    .sum()
}

#[derive(Debug, Clone, Copy)]
enum Light {
  On,
  Off,
}

#[derive(Debug, Clone, Copy)]
enum Cmd {
  TurnOff,
  TurnOn,
  Toggle,
}

trait IsLight {
  fn points(self) -> i32;
  fn flick_mut(&mut self, cmd: Cmd);
}

impl IsLight for i32 {
  fn points(self) -> i32 { self }

  fn flick_mut(&mut self, cmd: Cmd) {
    match cmd {
      Cmd::TurnOff => *self = max(*self - 1, 0),
      Cmd::TurnOn => *self += 1,
      Cmd::Toggle => *self += 2,
    }
  }
}

impl IsLight for Light {
  fn points(self) -> i32 {
    match self {
      Light::On => 1,
      Light::Off => 0,
    }
  }

  fn flick_mut(&mut self, cmd: Cmd) {
    match cmd {
      Cmd::TurnOff => *self = Light::Off,
      Cmd::TurnOn => *self = Light::On,
      Cmd::Toggle => match self {
        Light::On => *self = Light::Off,
        Light::Off => *self = Light::On,
      },
    }
  }
}

#[derive(Debug, Clone)]
pub struct Action {
  cmd: Cmd,
  c1:  Coord,
  c2:  Coord,
}

fn p_action(inp: &str) -> IResult<&str, Action> {
  let parse_pair = |inp| {
    map(
      separated_pair(character::complete::i32, tag(","), character::complete::i32),
      from_pair,
    )(inp)
  };

  let (inp, cmd) = alt((
    preceded(tag("turn on "), pure(Cmd::TurnOn)),
    preceded(tag("turn off "), pure(Cmd::TurnOff)),
    preceded(tag("toggle "), pure(Cmd::Toggle)),
  ))(inp)?;
  let (inp, c1) = parse_pair(inp)?;
  let (inp, _) = tag(" through ")(inp)?;
  let (inp, c2) = parse_pair(inp)?;

  Ok((inp, Action { cmd, c1, c2 }))
}
