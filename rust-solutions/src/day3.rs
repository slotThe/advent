use crate::util::*;
use std::collections::HashSet;

pub fn day3(p: Part) -> usize {
    match p {
        Part::One => solve(parse1),
        Part::Two => solve(parse2),
    }
}

fn get_input() -> Vec<String> {
    std::fs::read_to_string("./input/day3.txt")
        .unwrap()
        .lines()
        .map(|s| s.to_string())
        .collect()
}

fn solve<F>(parse: F) -> usize
where
    F: Fn() -> Vec<Vec<String>>,
{
    parse()
        .iter()
        .map(|v| intersect_hashsets(v.iter().map(|s| s.chars().collect())))
        .map(|hs| value(*hs.iter().next().unwrap())) // ????
        .sum()
}

fn parse1() -> Vec<Vec<String>> {
    get_input()
        .iter()
        .map(|s| {
            let split = s.len() / 2;
            vec![s[0..split].to_string(), s[split..].to_string()]
        })
        .collect()
}

fn parse2() -> Vec<Vec<String>> {
    get_input()
        .chunks(3)
        .map(|x| x.iter().map(|s| s.to_string()).collect())
        .collect()
}

// How is this so hard/not built into the HashSet module?
fn intersect_hashsets<I>(hash_sets: I) -> HashSet<char>
where
    I: Iterator<Item = HashSet<char>>,
{
    hash_sets
        .reduce(|acc, el| acc.intersection(&el).copied().collect()) // uff²
        .unwrap()
}

fn value(c: char) -> usize {
    match c {
        'a'..='z' => c as usize - 'a' as usize + 1,
        'A'..='Z' => c as usize - 'A' as usize + 27,
        _ => unreachable!(),
    }
}
