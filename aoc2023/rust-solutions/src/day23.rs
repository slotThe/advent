use std::collections::{HashMap, HashSet};

use rust_aoc_util::{coord, grid_get, transpose};

type UCoord = (usize, usize);

pub fn day23() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day23.txt").unwrap();
  let inp = transpose(inp.lines().map(|l| l.chars().collect()).collect());
  (solve(&inp, neighbours1), solve(&inp, neighbours2))
}

fn neighbours1(inp: &[Vec<char>], (x, y): UCoord) -> Vec<UCoord> {
  match inp[x][y] {
    '>' => vec![(x + 1, y)],
    '<' => vec![(x - 1, y)],
    'v' => vec![(x, y + 1)],
    '^' => vec![(x, y - 1)],
    _ => neighbours2(inp, (x, y)),
  }
}

fn neighbours2(inp: &[Vec<char>], (x, y): UCoord) -> Vec<UCoord> {
  coord::neighbours4_iter(x, y)
    .filter(|&(a, b)| grid_get(inp, a, b).is_some_and(|&c| c != '#'))
    .collect()
}

fn solve(
  inp: &[Vec<char>],
  neighbours: impl Fn(&[Vec<char>], UCoord) -> Vec<UCoord>,
) -> usize {
  let start = (1, 0);
  let goal = (inp.len() - 2, inp.len() - 1);
  let graph = build_graph(inp, neighbours);
  let mut queue = vec![(start, 0, HashSet::from([start]))];
  let mut max_goal = 0;

  while let Some((v, c, seen)) = queue.pop() {
    if v == goal {
      max_goal = max_goal.max(c);
    }
    for (neigh, cost_n) in &graph[&v] {
      if !seen.contains(neigh) {
        let mut seen_n = seen.clone();
        seen_n.insert(*neigh);
        queue.push((*neigh, c + cost_n, seen_n));
      }
    }
  }

  max_goal
}

// There is a smaller graph hiding in the bigger graph! Basically,
// instead of looking at the big grid, build an undirected graph out of
// all crossings (i.e., points with more than two neighbours), as well
// as the two end points. This is a *much* smaller graph, so that one
// can semi-bruteforce the longest path in `solve'.
fn build_graph(
  inp: &[Vec<char>],
  neighbours: impl Fn(&[Vec<char>], UCoord) -> Vec<UCoord>,
) -> HashMap<UCoord, HashMap<UCoord, usize>> {
  let mut vertices = HashSet::from([(1, 0), (inp.len() - 2, inp.len() - 1)]);
  for i in 0..inp.len() {
    for j in 0..inp.len() {
      if inp[i][j] != '#' && 2 < neighbours(inp, (i, j)).len() {
        vertices.insert((i, j));
      }
    }
  }
  vertices
    .iter()
    .map(|&v| (v, build_paths(inp, &vertices, v, &neighbours)))
    .collect()
}

fn build_paths(
  inp: &[Vec<char>],
  nodes: &HashSet<UCoord>,
  v: UCoord,
  neighbours: impl Fn(&[Vec<char>], UCoord) -> Vec<UCoord>,
) -> HashMap<UCoord, usize> {
  let mut queue: Vec<_> = neighbours(inp, v).into_iter().map(|n| (n, 1)).collect();
  let mut visited = HashSet::from([v]);
  let mut res = HashMap::new();
  while let Some((pt, cost)) = queue.pop() {
    visited.insert(pt);
    if nodes.contains(&pt) {
      res.insert(pt, cost);
    } else {
      for neigh in neighbours(inp, pt) {
        if !visited.contains(&neigh) {
          queue.push((neigh, cost + 1));
          visited.insert(neigh);
        }
      }
    }
  }
  res
}
