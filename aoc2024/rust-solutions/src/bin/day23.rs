#![feature(iter_intersperse)]
use std::collections::{HashMap, HashSet};

use anyhow::Result;
use itertools::Itertools;
use rust_aoc_util::print_day;

fn main() -> Result<()> {
  let inp = std::fs::read_to_string("../inputs/day23.txt")?;
  let inp: HashMap<&str, HashSet<&str>> = inp
    .lines()
    .flat_map(|l| {
      let [a, b] = l.split("-").collect_vec()[..] else {
        unimplemented!()
      };
      [(a, b), (b, a)]
    })
    .fold(HashMap::new(), |mut acc, (k, v)| {
      acc
        .entry(k)
        .and_modify(|vs| {
          vs.insert(v);
        })
        .or_insert(HashSet::from([v]));
      acc
    });

  let one = c3(&inp);
  assert_eq!(926, one);
  let two =
    Iterator::intersperse(max_clique(&inp).into_iter().sorted(), ",").collect::<String>();
  assert_eq!("az,ed,hz,it,ld,nh,pc,td,ty,ux,wc,yg,zz", two);
  print_day(23, (one, two));
  Ok(())
}

fn c3(graph: &HashMap<&str, HashSet<&str>>) -> usize {
  graph
    .keys()
    .filter(|g| g.starts_with("t"))
    .flat_map(|a| {
      graph.get(a).unwrap().iter().flat_map(move |&b| {
        graph
          .get(b)
          .unwrap()
          .iter()
          .filter(|&&c| graph.get(c).unwrap().contains(a))
          .map(move |c| [a, b, c].into_iter().sorted().collect_vec())
      })
    })
    .unique()
    .count()
}

fn max_clique<'a>(graph: &'a HashMap<&'a str, HashSet<&'a str>>) -> HashSet<&'a str> {
  let mut max_clique = HashSet::new();
  for n in graph.keys() {
    let mut clique = HashSet::from([*n]);
    for n2 in graph.keys() {
      if clique.is_subset(graph.get(n2).unwrap()) {
        clique.insert(n2);
      }
    }
    max_clique = std::cmp::max_by_key(max_clique, clique, HashSet::len);
  }
  max_clique
}
