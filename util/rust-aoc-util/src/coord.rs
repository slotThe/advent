use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Dir {
  North,
  East,
  South,
  West,
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
