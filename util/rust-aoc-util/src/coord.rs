use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum Dir {
  North,
  East,
  South,
  West,
}

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Coord {
  x: i32,
  y: i32,
}

impl Coord {
  pub fn move_dir_mut(&mut self, d: Dir) {
    match d {
      Dir::North => self.y -= 1,
      Dir::West => self.x -= 1,
      Dir::East => self.x += 1,
      Dir::South => self.y += 1,
    }
  }
}

pub fn from_pair(p: (i32, i32)) -> Coord { Coord { x: p.0, y: p.1 } }

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
