use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::coord::{char_to_dir, Coord, Dir, ORIGIN};
use std::collections::HashSet;

pub fn day3() -> Result<(usize, usize)> {
    let moves: Vec<Dir> = std::fs::read_to_string("../inputs/day3.txt")?
        .chars()
        .flat_map(char_to_dir)
        .collect();
    Ok((solve(moves.clone(), 1), solve(moves, 2)))
}

fn solve(moves: Vec<Dir>, n: usize) -> usize {
    let mut workers: Vec<Coord> = (0..n).map(|_| ORIGIN).collect();
    let mut visited = HashSet::from([ORIGIN]);
    moves.into_iter().chunks(n).into_iter().for_each(|ch| {
        ch.into_iter().enumerate().for_each(|(i, x)| {
            workers[i].move_dir_mut(x);
            visited.insert(workers[i]);
        })
    });
    visited.len()
}
