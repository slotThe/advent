use anyhow::Result;
use itertools::Itertools;
use std::{
    borrow::{Borrow, BorrowMut},
    cmp::min,
    collections::HashMap,
    num::ParseIntError,
    str::FromStr,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Dir2D {
    North,
    East,
    South,
    West,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn move_to_mut(&mut self, d: Dir2D) {
        use Dir2D::*;
        match d {
            North => self.y += 1,
            East => self.x += 1,
            South => self.y -= 1,
            West => self.x -= 1,
        }
    }
}

fn char_to_dir2d(c: char) -> Option<Dir2D> {
    use Dir2D::*;
    match c {
        '^' => Some(North),
        '>' => Some(East),
        'v' => Some(South),
        '<' => Some(West),
        _ => None,
    }
}

pub fn day3() -> Result<(i32, i32)> {
    let moves: Vec<Dir2D> = std::fs::read_to_string("../inputs/day3.txt")?
        .chars()
        .flat_map(char_to_dir2d)
        .collect();
    Ok((solve(moves.clone(), 1), solve(moves, 2)))
}

fn solve(moves: Vec<Dir2D>, n: usize) -> i32 {
    let origin = Coord { x: 0, y: 0 };
    let mut workers: Vec<Coord> = (0..n).map(|_| origin).collect();
    let mut hm = HashMap::from([(origin, n)]);
    moves.into_iter().chunks(n).into_iter().for_each(|ch| {
        ch.into_iter().enumerate().for_each(|(i, x)| {
            workers[i].move_to_mut(x);
            let times_visited = hm.entry(workers[i]).or_insert(0);
            *times_visited += 1;
        })
    });
    hm.into_keys().len() as i32
}
