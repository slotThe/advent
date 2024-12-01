use std::collections::HashMap;

use rust_aoc_util::transpose_with;

fn main() {
  let inp = std::fs::read_to_string("../inputs/day01.txt").unwrap();
  let inp: Vec<Vec<u32>> = transpose_with(
    |mut row| {
      row.sort();
      row
    },
    inp
      .trim()
      .split("\n")
      .map(|l| l.split("   ").map(|s| s.parse::<u32>().unwrap()).collect())
      .collect(),
  );

  let one: u32 = inp[0]
    .iter()
    .zip(inp[1].clone())
    .map(|(x, y)| x.abs_diff(y))
    .sum();
  assert!(1579939 == one);
  println!("{one}");

  // MUTABILITY BOIIIIS
  let mut m = HashMap::new();
  for x in &inp[1] {
    *m.entry(x).or_insert(0) += 1
  }
  let mut two = 0;
  for x in &inp[0] {
    two += x * m.get(&x).unwrap_or(&0);
  }
  assert!(20351745 == two);
  println!("{two}");
}
