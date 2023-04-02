use std::collections::HashMap;

use crate::util::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, rest},
    sequence::{preceded, terminated},
    IResult,
};

#[derive(Debug)]
pub enum Ins {
    Cd(String),
    File(u128),
    Ignore,
}

pub fn day7(p: Part) -> Option<u128> {
    let fs = std::fs::read_to_string("../inputs/day7.txt")
        .ok()
        .map(|res| simulate(res.lines().flat_map(|s| parse(s, p_input)).collect()))?;
    match p {
        Part::One => Some(fs.into_values().filter(|v| *v <= 100_000).sum()),
        Part::Two => {
            let need = 30_000_000
                - (70_000_000
                    - fs.get(&vec!["/".to_string()])
                        .expect("The root directory should be indexed."));
            Some(fs.into_values().filter(|v| *v >= need).min().unwrap())
        }
    }
}

fn p_input(inp: &str) -> IResult<&str, Ins> {
    alt((
        map(preceded(tag("$ cd "), rest), |d: &str| {
            Ins::Cd(d.to_string())
        }),
        map(terminated(nom::character::complete::u128, rest), Ins::File),
        map(rest, |_| Ins::Ignore),
    ))(inp)
}

fn simulate(instructions: Vec<Ins>) -> HashMap<Vec<String>, u128> {
    // Dude mutability lmao
    let mut path: Vec<String> = vec!["/".to_string()];
    let mut hm: HashMap<Vec<String>, u128> = HashMap::new();
    for ins in instructions {
        match ins {
            // Can't match on Cd("..") :(
            Ins::Cd(pth) => {
                if pth == ".." {
                    path.pop();
                } else {
                    path.push(pth);
                }
            }
            Ins::File(size) => {
                let mut p = path.clone();
                while !p.is_empty() {
                    hm.entry(p.clone())
                        .and_modify(|v| *v += size)
                        .or_insert(size);
                    p.pop();
                }
            }
            Ins::Ignore => (),
        }
    }
    hm
}
