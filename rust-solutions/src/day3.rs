use crate::util::*;
use std::collections::HashSet;

pub fn day3(p: Part) -> usize {
    match p {
        Part::One => solve(parse1),
        Part::Two => solve(parse2),
    }
}

fn get_input() -> String {
    std::fs::read_to_string("./input/day3.txt").unwrap()
}

fn solve<F>(parse: F) -> usize
where
    F: Fn() -> Vec<Vec<String>>,
{
    let string_to_set = |s: &String| -> HashSet<char> {
        let mut hs = HashSet::new();
        s.chars().for_each(|c| {
            hs.insert(c);
        });
        hs
    };

    parse()
        .iter()
        .map(|v| intersect_hashsets(v.iter().map(string_to_set)))
        .map(|hs| value(*hs.iter().next().unwrap())) // ????
        .sum()
}

fn parse1() -> Vec<Vec<String>> {
    get_input()
        .lines()
        .map(|s| {
            let split = s.len() / 2;
            vec![s[0..split].to_string(), s[split..].to_string()]
        })
        .collect()
}

fn parse2() -> Vec<Vec<String>> {
    get_input()
        .lines()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .chunks(3)
        .map(|x| x.to_vec())
        .collect()
}

// How is this so hard/not built into the HashSet module?
fn intersect_hashsets<I>(mut hash_sets: I) -> HashSet<char>
where
    I: Iterator<Item = HashSet<char>>,
{
    if let Some(first) = hash_sets.next() {
        hash_sets.fold(first, |acc, el| {
            acc.intersection(&el).copied().collect() // uff²
        })
    } else {
        HashSet::new()
    }
}

fn value(c: char) -> usize {
    let ascii = c as usize;
    if (97..=122).contains(&ascii) {
        ascii - 96
    } else {
        ascii - 38
    }
}
