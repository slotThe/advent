use rust_aoc_util::transpose;

type CMatRef<'a> = &'a [Vec<char>];
type CMatComp<'a> = &'a [(&'a [char], &'a [char])]; // Yikes!

pub fn day13() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day13.txt").unwrap();
  let inp: Vec<Vec<Vec<char>>> = inp
    .split("\n\n")
    .map(|r| r.lines().map(|l| l.chars().collect()).collect())
    .map(transpose)
    .collect();
  (
    inp
      .iter()
      .map(|s| find_reflection(|inp| inp.iter().all(|(x, y)| x == y), s))
      .sum(),
    inp
      .iter()
      .map(|s| find_reflection(equal_up_to_smudge, s))
      .sum(),
  )
}

fn equal_up_to_smudge(inp: CMatComp) -> bool {
  1 == inp
    .iter()
    .flat_map(|(r1, r2)| r1.iter().zip(*r2).map(|(a, b)| if a == b { 0 } else { 1 }))
    .sum::<usize>()
}

fn find_reflection(eq: impl Fn(CMatComp) -> bool, ground: CMatRef) -> usize {
  (1..ground.len())
    .find(|&n| {
      eq(
        ground[..n]
          .iter()
          .rev()
          .zip(&ground[n..])
          .map(|(a, b)| (a.as_ref(), b.as_ref()))
          .collect::<Vec<_>>()
          .as_ref(),
      )
    })
    .unwrap_or_else(|| 100 * find_reflection(eq, &transpose(ground.to_vec())))
}
