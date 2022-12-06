use anyhow::{Context, Result};

pub fn get_nth_most_wanted(n: usize) -> i64 {
    let mut input = get_input().unwrap();
    input.sort_by(|a, b| b.cmp(a));
    input.into_iter().take(n).sum()
}

fn get_input() -> Result<Vec<i64>> {
    input_from_lines(std::fs::read_to_string("../inputs/day1.txt")?)
}

fn input_from_lines(input: String) -> Result<Vec<i64>> {
    input
        .split("\n\n")
        .into_iter()
        .map(|elf| {
            elf.lines()
                .map(|l| l.parse::<i64>().context("parsing calories"))
                .sum()
        })
        .collect()
}
