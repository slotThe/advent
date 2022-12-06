use crate::util::*;
use lazy_static::lazy_static;
use regex::Regex;
use std::{num::ParseIntError, str::FromStr};

struct RangePair {
    elf1: (usize, usize),
    elf2: (usize, usize),
}

impl FromStr for RangePair {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)$").unwrap();
        }
        let caps: Vec<usize> = RE
            .captures(s)
            .unwrap()
            .iter()
            .skip(1) // Skip whole match
            .map(|c| c.unwrap().as_str().parse().unwrap())
            .collect();
        Ok(RangePair {
            elf1: (caps[0], caps[1]),
            elf2: (caps[2], caps[3]),
        })
    }
}

impl RangePair {
    fn completely_overlap(&self) -> bool {
        self.elf1.0 >= self.elf2.0 && self.elf1.1 <= self.elf2.1        // 1 contained in 2
            || self.elf1.0 <= self.elf2.0 && self.elf1.1 >= self.elf2.1 // 2 contained in 1
    }
    fn overlap(&self) -> bool {
        !(self.elf1.1 < self.elf2.0 || self.elf2.1 < self.elf1.0) // not disjoint
    }
}

pub fn day4(p: Part) -> usize {
    match p {
        Part::One => solve(|rp| rp.completely_overlap()),
        Part::Two => solve(|rp| rp.overlap()),
    }
}

fn solve<F>(filter_by: F) -> usize
where
    F: Fn(&RangePair) -> bool,
{
    std::fs::read_to_string("../inputs/day4.txt").map_or(0, |f| {
        f.lines()
            .map(|s| s.parse().expect("parse line"))
            .filter(filter_by)
            .count()
    })
}
