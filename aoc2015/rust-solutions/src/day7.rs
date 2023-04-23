use anyhow::Result;
use itertools::Itertools;
use nom::{branch::alt,
          character::{self, complete::alpha1},
          combinator::map,
          sequence::tuple,
          IResult};
use rust_aoc_util::{parse, wtag};
use std::collections::{HashMap, VecDeque};

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Operand {
  Lit(String),
  Num(u16),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Op<T> {
  LSHIFT(T, u16),
  RSHIFT(T, u16),
  OR(T, T),
  AND(T, T),
  NOT(T),
  ASS(T),
}

pub fn day7() -> Result<(u16, u16)> {
  let input = std::fs::read_to_string("../inputs/day7.txt")?;
  let mut wiring: HashMap<&str, Op<Operand>> =
    HashMap::from_iter(input.lines().map(|x| parse(x, p_wiring).unwrap()));
  let one = solve("a", &wiring);
  wiring.insert("b", Op::ASS(Operand::Num(one)));
  Ok((one, solve("a", &wiring)))
}

// This would be so much easier with lazy dynamic programming :(
fn solve(start: &str, hm: &HashMap<&str, Op<Operand>>) -> u16 {
  let mut seen: HashMap<String, u16> = HashMap::new();
  let mut pile: VecDeque<String> = VecDeque::from([start.to_string()]);
  loop {
    pile = pile.into_iter().unique().collect();
    if pile.is_empty() {
      break;
    } else {
      let head = pile.pop_front().unwrap();
      match hm.get(head.as_str()).unwrap().eval(&seen) {
        Ok(i) => {
          seen.insert(head, i);
        },
        Err(v) => {
          pile.push_back(v);
          pile.push_back(head);
        },
      }
    }
  }
  *seen.get(start).unwrap()
}

impl Op<u16> {
  fn eval(self) -> u16 {
    use Op::*;
    match self {
      NOT(i) => !i,
      ASS(i) => i,
      LSHIFT(i, l) => i << l,
      RSHIFT(i, r) => i >> r,
      AND(l, r) => l & r,
      OR(l, r) => l | r,
    }
  }
}

impl Op<Operand> {
  fn eval(&self, seen: &HashMap<String, u16>) -> Result<u16, String> {
    use Op::*;
    // There *has* to be a better way…
    match self {
      LSHIFT(a, i) => {
        let a2 = lookup(a, seen)?;
        Ok(LSHIFT(a2, *i).eval())
      },
      RSHIFT(a, i) => {
        let a2 = lookup(a, seen)?;
        Ok(RSHIFT(a2, *i).eval())
      },
      NOT(a) => {
        let a2 = lookup(a, seen)?;
        Ok(NOT(a2).eval())
      },
      ASS(a) => {
        let a2 = lookup(a, seen)?;
        Ok(ASS(a2).eval())
      },
      OR(a, b) => {
        let a2 = lookup(a, seen)?;
        let b2 = lookup(b, seen)?;
        Ok(OR(a2, b2).eval())
      },
      AND(a, b) => {
        let a2 = lookup(a, seen)?;
        let b2 = lookup(b, seen)?;
        Ok(AND(a2, b2).eval())
      },
    }
  }
}

fn lookup<'a>(o: &'a Operand, seen: &'a HashMap<String, u16>) -> Result<u16, String> {
  match o {
    Operand::Num(i) => Ok(*i),
    Operand::Lit(s) => match seen.get(s) {
      None => Err(s.to_string()),
      Some(i) => Ok(*i),
    },
  }
}

///////////////////////////////////////////////////////////////////////
// Parsing

fn p_operand(inp: &str) -> IResult<&str, Operand> {
  alt((
    map(character::complete::u16, Operand::Num),
    map(alpha1, |x: &str| Operand::Lit(x.to_string())),
  ))(inp)
}

fn p_op(inp: &str) -> IResult<&str, Op<Operand>> {
  alt((
    map(
      tuple((p_operand, wtag("LSHIFT"), character::complete::u16)),
      |(l, _, r)| Op::LSHIFT(l, r),
    ),
    map(
      tuple((p_operand, wtag("RSHIFT"), character::complete::u16)),
      |(l, _, r)| Op::RSHIFT(l, r),
    ),
    map(tuple((p_operand, wtag("OR"), p_operand)), |(l, _, r)| {
      Op::OR(l, r)
    }),
    map(tuple((p_operand, wtag("AND"), p_operand)), |(l, _, r)| {
      Op::AND(l, r)
    }),
    map(tuple((wtag("NOT"), p_operand)), |(_, r)| Op::NOT(r)),
    map(p_operand, Op::ASS),
  ))(inp)
}

fn p_wiring(inp: &str) -> IResult<&str, (&str, Op<Operand>)> {
  map(tuple((p_op, wtag("->"), alpha1)), |(lhs, _, rhs)| {
    (rhs, lhs)
  })(inp)
}
