use crate::util::*;
use std::str::FromStr;

#[derive(Debug)]
pub enum Ins {
    Noop,
    Add(i32),
}

impl FromStr for Ins {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "noop" {
            Ok(Ins::Noop)
        } else if s.contains("addx ") {
            Ok(Ins::Add(s[5..].parse().unwrap()))
        } else {
            Err(())
        }
    }
}

fn simulate(inp: Vec<Ins>) -> Vec<i32> {
    let mut res: Vec<i32> = vec![1];
    for ins in inp {
        let val = res[res.len() - 1];
        match ins {
            Ins::Noop => res.push(val),
            Ins::Add(a) => {
                res.push(val);
                res.push(val + a);
            }
        }
    }
    res
}

pub fn day10(p: Part) {
    let v: Vec<i32> = std::fs::read_to_string("../inputs/day10.txt")
        .ok()
        .unwrap()
        .lines()
        .map(|s| s.parse().ok())
        .collect::<Option<_>>()
        .map(simulate)
        .unwrap();
    match p {
        Part::One => println!(
            "{}",
            (20..221)
                .step_by(40)
                .fold(0, |acc, ix| acc + v[ix - 1] * ix as i32),
        ),
        Part::Two => {
            let res: Vec<&str> = v
                .iter()
                .enumerate()
                .map(|(i, val)| {
                    let pos = (i % 40) as i32;
                    if val - 1 <= pos && pos <= val + 1 {
                        "█"
                    } else {
                        " "
                    }
                })
                .collect();
            // This will have to do for now.
            for i in 0..(res.len() / 40) {
                for j in 0..39 {
                    print!("{} ", res[j + 40 * i]);
                }
                println!();
            }
        }
    }
}
