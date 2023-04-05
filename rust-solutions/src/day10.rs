use std::{fmt::Debug, str::FromStr};

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

pub fn day10() -> (i32, String) {
    let steps: Vec<i32> = simulate(
        std::fs::read_to_string("../inputs/day10.txt")
            .unwrap()
            .lines()
            .map(|s| s.parse().unwrap())
            .collect(),
    );
    (
        (20..=220)
            .step_by(40)
            .fold(0, |acc, ix| acc + steps[ix - 1] * ix as i32),
        part2(steps),
    )
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

fn part2(inp: Vec<i32>) -> String {
    let res: Vec<&str> = inp
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
    "\n".to_string()
        + (0..res.len() / 40)
            .map(|i| (0..39).map(|j| res[j + 40 * i].to_string()).collect())
            .intersperse("\n".to_string())
            .collect::<String>()
            .as_ref()
}
