#![feature(iter_map_windows)]
use std::{collections::HashMap, sync::LazyLock};

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::{coord::{self, Coord, EAST, NORTH, SOUTH, WEST}, print_day};

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day21.txt")?;
  let inp = inp.lines().collect_vec();
  let sol = |n| {
    inp
      .iter()
      .map(|i| {
        i.chars()
          .dropping_back(1)
          .collect::<String>()
          .parse::<usize>()
          .map_or(0, |k| k * solve(n, i))
      })
      .sum::<usize>()
  };
  let one = sol(2);
  assert_eq!(one, 246990);
  let two = sol(25);
  assert_eq!(two, 306335137543664);
  print_day(21, (one, two));
  Ok(())
}

type Cache = HashMap<((char, char), usize), usize>;

fn solve(lvl: usize, s: &str) -> usize {
  fn go(m: Mode, n: usize, s: &str, memo: &mut Cache) -> usize {
    use Mode::*;
    ("A".to_string() + s)
      .chars()
      .map_windows(|&[x, y]| match memo.get(&((x, y), n)) {
        Some(r) => *r,
        None => {
          let paths = dirs(m, x, y);
          let ret = if n == 0 {
            paths.iter().map(|p| p.len()).min().unwrap()
          } else {
            paths.iter().map(|p| go(Dir, n - 1, p, memo)).min().unwrap()
          };
          memo.insert(((x, y), n), ret);
          ret
        },
      })
      .sum()
  }
  go(Mode::Key, lvl, s, &mut HashMap::new()) // Key is only used once
}

fn dirs(m: Mode, b: char, e: char) -> Vec<String> {
  let hm = match m {
    Mode::Key => &KEYPAD,
    Mode::Dir => &DIRPAD,
  };
  let mut res: Vec<(Coord, Vec<Coord>)> = vec![(*hm.get(&b).unwrap(), vec![])];
  let end = *hm.get(&e).unwrap();
  loop {
    res = res
      .iter()
      .flat_map(|(p, path)| {
        [NORTH, SOUTH, EAST, WEST]
          .iter()
          .filter_map(|&m| {
            let q = *p + m;
            if q.manhattan(end) < p.manhattan(end) && q != coord::from_pair((-2, -1)) {
              Some((q, [path.clone(), vec![m]].concat()))
            } else {
              None
            }
          })
          .collect_vec()
      })
      .collect();
    match res.first() {
      None => {
        return vec!["A".to_string()];
      },
      Some((c, _)) => {
        if c == &end {
          return res // Transform into human-readable directions
            .iter()
            .map(|(_, p)| p.iter().map(|c| c.to_dir()).collect::<String>() + "A")
            .max_set_by_key(|p| {
              p.chars().tuple_windows().filter(|(x, y)| x == y).count()
            });
        }
      },
    }
  }
}

fn fp(x: i32, y: i32) -> Coord { coord::from_pair((x, y)) }

#[rustfmt::skip]
static KEYPAD: LazyLock<HashMap<char, Coord>> = LazyLock::new(|| {
    [ ('7', fp(-2, -3)), ('8', fp(-1, -3)), ('9', fp(0, -3))
    , ('4', fp(-2, -2)), ('5', fp(-1, -2)), ('6', fp(0, -2))
    , ('1', fp(-2, -1)), ('2', fp(-1, -1)), ('3', fp(0, -1))
    ,                    ('0', fp(-1,  0)), ('A', fp(0,  0))
    ].into_iter().collect()
});

#[rustfmt::skip]
static DIRPAD: LazyLock<HashMap<char, Coord>> = LazyLock::new(|| {
    [                   ('^', fp(-1,  -1)), ('A', fp(0, -1))
    , ('<', fp(-2, 0)), ('v', fp(-1,   0)), ('>', fp(0,  0))
    ].into_iter().collect()
});

#[derive(Clone, Copy)]
enum Mode {
  Key,
  Dir,
}
