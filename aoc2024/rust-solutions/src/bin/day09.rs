use itertools::Itertools;

fn main() {
  let inp = std::fs::read_to_string("../inputs/day09.txt").unwrap();
  println!("{}", solve1(&inp.trim()));
  println!("{}", solve2(&inp.trim()));
}

///////////////////////////////////////////////////////////////////////
// One

#[derive(Clone, Copy)]
enum Block {
  Free,
  File(usize),
}

impl Block {
  fn is_file(&self) -> bool { matches!(self, Block::File(_)) }

  fn is_free(&self) -> bool { !self.is_file() }
}

fn expand(s: &str) -> Vec<Block> {
  s.chars()
    .enumerate()
    .flat_map(|(id, c)| {
      let n = c.to_digit(10).unwrap() as usize;
      if id % 2 == 0 {
        std::iter::repeat_n(Block::File(id / 2), n).collect_vec() // file
      } else {
        std::iter::repeat_n(Block::Free, n).collect_vec() // free real estate
      }
    })
    .collect()
}

fn move_baby(v: &mut [Block]) {
  let n = v.len() - 1;
  loop {
    if v.iter().skip_while(|b| b.is_file()).all(|b| b.is_free()) {
      return;
    } else {
      v.swap(
        v.iter().position(|b| b.is_free()).unwrap(),
        n - v.iter().rev().position(|b| b.is_file()).unwrap(),
      );
    }
  }
}

fn solve1(inp: &str) -> usize {
  let mut v = expand(inp);
  move_baby(&mut v);
  v.iter()
    .filter_map(|b| match b {
      Block::Free => None,
      Block::File(n) => Some(n),
    })
    .enumerate()
    .map(|(i, n)| i * n)
    .sum()
}

///////////////////////////////////////////////////////////////////////
// Two

/// input → files[subtotal index, size], frees[subtotal index, size]
fn split(s: &str) -> (Vec<(usize, usize)>, Vec<(usize, usize)>) {
  let (file, free): (Vec<(usize, (usize, usize))>, Vec<(usize, (usize, usize))>) = s
    .trim()
    .chars()
    .map(|c| c.to_digit(10).unwrap() as usize)
    .scan(0, |acc, amnt| {
      let oldacc = *acc;
      *acc += amnt;
      Some((oldacc, amnt))
    })
    .enumerate()
    .partition(|(i, _)| i % 2 == 0);
  (
    file.into_iter().map(|(_, x)| x).collect(),
    free.into_iter().map(|(_, x)| x).collect(),
  )
}

fn solve2(inp: &str) -> usize {
  let (files, mut frees) = split(inp);
  (0..files.len())
    .rev()
    .map(|id| {
      let (mut subt, size) = files[id];
      match frees.iter_mut().take(id).find(|(_, fsize)| size <= *fsize) {
        None => {},
        // Found free space, so switch and change subtotal.
        Some((foff, fsize)) => {
          subt = *foff;
          *foff += size;
          *fsize -= size;
        },
      }
      id * (2 * subt + size - 1) * size / 2 // size range-sum from subt
    })
    .sum()
}
