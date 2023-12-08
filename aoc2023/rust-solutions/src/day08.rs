use std::collections::{HashMap, HashSet};

use lazy_static::lazy_static;
use regex::Regex;
use rust_aoc_util::lcm;

type Tree<'a> = HashMap<&'a str, (&'a str, &'a str)>;

pub fn day08() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day08.txt").unwrap();
  let inp = inp.split("\n\n").collect::<Vec<_>>();
  let (lr, tree) = (inp[0].chars().collect::<Vec<_>>(), p_tree(inp[1]));
  (
    go_down("AAA", &HashSet::from(["ZZZ"]), &lr, &tree),
    solve2(&lr, &tree),
  )
}

fn solve2(lr: &[char], tree: &Tree) -> usize {
  let nodes: Vec<&str> = tree.keys().copied().collect();
  let ends: HashSet<&str> = nodes.iter().filter(|s| s.ends_with('Z')).copied().collect();
  nodes
    .iter()
    .filter(|s| s.ends_with('A'))
    .map(|s| go_down(s, &ends, lr, tree))
    .reduce(lcm)
    .unwrap()
}

fn go_down(start: &str, ends: &HashSet<&str>, lr: &[char], tree: &Tree) -> usize {
  let mut n = 0;
  let mut ix = 0;
  let mut node = start;
  loop {
    if ends.contains(node) {
      return n;
    } else {
      let (l, r) = tree[node];
      node = if let 'L' = lr[ix] { l } else { r };
      ix = (ix + 1) % lr.len();
      n += 1;
    }
  }
}

fn p_tree(inp: &str) -> Tree {
  lazy_static! {
    static ref RE: Regex = Regex::new(r"[A-Z]{3}").unwrap();
  }
  inp
    .lines()
    .map(|s| {
      let ws: Vec<&str> = RE
        .captures_iter(s)
        .map(|cap| cap.get(0).unwrap().as_str())
        .collect();
      (ws[0], (ws[1], ws[2]))
    })
    .collect()
}
