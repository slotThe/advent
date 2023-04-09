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

pub const ORIGIN: Coord = Coord { x: 0, y: 0 };
pub const NORTH: Coord = Coord { x: 0, y: -1 };
pub const SOUTH: Coord = Coord { x: 0, y: 1 };
pub const EAST: Coord = Coord { x: 1, y: 0 };
pub const WEST: Coord = Coord { x: -1, y: 0 };
