use std::{collections::HashMap, ops::{Add, Mul, Sub}};

use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum Dir {
  North,
  East,
  South,
  West,
}

const CARDINAL: [Dir; 4] = [Dir::North, Dir::West, Dir::South, Dir::East];

impl Dir {
  pub fn opposed(d: Dir) -> (Dir, Dir) {
    use Dir::*;
    match d {
      North | South => (East, West),
      East | West => (North, South),
    }
  }

  pub fn is_opposed(self, d: Dir) -> bool {
    let (a, b) = Self::opposed(self);
    d == a || d == b
  }

  pub fn is_reversed(self, d: Dir) -> bool {
    use Dir::*;
    match self {
      North => d == South,
      South => d == North,
      East => d == West,
      West => d == East,
    }
  }

  pub fn turn_right(self) -> Dir {
    match self {
      Dir::North => Dir::East,
      Dir::East => Dir::South,
      Dir::South => Dir::West,
      Dir::West => Dir::North,
    }
  }
}

pub fn neighbours4_iter(x: usize, y: usize) -> impl Iterator<Item = (usize, usize)> {
  neighbours4_dir_iter(x, y).map(|(a, b, _)| (a, b))
}

pub fn neighbours4_dir_iter(
  x: usize,
  y: usize,
) -> impl Iterator<Item = (usize, usize, Dir)> {
  [
    (x + 1, y, Dir::East),
    (x, y + 1, Dir::South),
    (x - 1, y, Dir::West),
    (x, y - 1, Dir::North),
  ]
  .into_iter()
}

pub fn char_to_dir(c: char) -> Option<Dir> {
  match c {
    '^' => Some(Dir::North),
    '>' => Some(Dir::East),
    'v' => Some(Dir::South),
    '<' => Some(Dir::West),
    _ => None,
  }
}

// 2d coordinates: x grows to the *right* and y grows *down*.

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Coord {
  pub x: i32,
  pub y: i32,
}

impl Coord {
  pub fn move_in(self, d: Dir) -> Self {
    match d {
      Dir::North => self + NORTH,
      Dir::West => self + WEST,
      Dir::East => self + EAST,
      Dir::South => self + SOUTH,
    }
  }

  pub fn move_dir_mut(&mut self, d: Dir) {
    match d {
      Dir::North => self.y -= 1,
      Dir::West => self.x -= 1,
      Dir::East => self.x += 1,
      Dir::South => self.y += 1,
    }
  }

  pub fn cmul(&self, c: i32) -> Self {
    Coord {
      x: self.x * c,
      y: self.y * c,
    }
  }

  pub fn rem_euclid(&self, rhs: &Self) -> Self {
    Coord {
      x: self.x.rem_euclid(rhs.x),
      y: self.y.rem_euclid(rhs.y),
    }
  }

  pub fn neighbours4(&self) -> impl Iterator<Item = Self> + use<'_> {
    CARDINAL.iter().map(|d| self.move_in(*d))
  }

  pub fn manhattan(&self, p: Self) -> u32 { self.x.abs_diff(p.x) + self.y.abs_diff(p.y) }
}

pub fn from_pair(p: (i32, i32)) -> Coord { Coord { x: p.0, y: p.1 } }

pub fn map_from_grid(s: &str) -> HashMap<Coord, char> {
  s.lines()
    .enumerate()
    .flat_map(|(i, l)| {
      l.chars()
        .enumerate()
        .map(|(j, c)| (from_pair((j as i32, i as i32)), c))
        .collect_vec()
    })
    .collect()
}

macro_rules! coord_instances {
  ($($inst:ident $name:ident),+) => {
    $(
      impl $inst for Coord {
        type Output = Self;
        fn $name(self, rhs: Self) -> Self::Output {
          Coord {
            x: self.x.$name(rhs.x),
            y: self.y.$name(rhs.y),
          }
        }
      }
    )+
  };
}
coord_instances!(Add add, Sub sub, Mul mul);

/// 2D coordinate indexing. Coordinates are required to be non-negative;
/// this is not checked.
pub fn range(c1: Coord, c2: Coord) -> impl Iterator<Item = (usize, usize)> {
  (c1.x as usize..=c2.x as usize).cartesian_product(c1.y as usize..=c2.y as usize)
}

pub const ORIGIN: Coord = Coord { x: 0, y: 0 };
pub const NORTH: Coord = Coord { x: 0, y: -1 };
pub const SOUTH: Coord = Coord { x: 0, y: 1 };
pub const EAST: Coord = Coord { x: 1, y: 0 };
pub const WEST: Coord = Coord { x: -1, y: 0 };
