use rust_aoc_util::{coord::{neighbours4_dir_iter, Dir}, dijkstra, grid_get, inp_to_grid};

pub fn day17() -> (u32, u32) {
  let inp = std::fs::read_to_string("../inputs/day17.txt").unwrap();
  let inp: Vec<Vec<u32>> = inp_to_grid(|c| c.to_digit(10), &inp);
  (solve(&inp, 0, 3), solve(&inp, 4, 10))
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
struct Point {
  x:            usize,
  y:            usize,
  dir:          Dir,
  straight_for: usize,
}

fn solve(inp: &[Vec<u32>], max_straight: usize, turn_after: usize) -> u32 {
  fn filter_neighbours(
    inp: &[Vec<u32>],
    u: Point,
    filter_before: impl Fn(Point, Dir) -> bool,
    mk_straight_for: impl Fn(Point, Dir) -> usize,
  ) -> Vec<(Point, u32)> {
    neighbours4_dir_iter(u.x, u.y)
      .flat_map(|(x, y, dir)| {
        filter_before(u, dir).then(|| {
          grid_get(inp, x, y).map(|c| {
            (
              Point {
                x,
                y,
                dir,
                straight_for: mk_straight_for(u, dir),
              },
              *c,
            )
          })
        })?
      })
      .collect()
  }

  let shortest_paths = dijkstra(
    &[
      Point {
        x:            0,
        y:            0,
        dir:          Dir::East,
        straight_for: 0,
      },
      Point {
        x:            0,
        y:            0,
        dir:          Dir::South,
        straight_for: 0,
      },
    ],
    |&u| {
      if u.straight_for >= turn_after {
        filter_neighbours(inp, u, |u, dir| u.dir.is_opposed(dir), |_, _| 1)
      } else if u.straight_for < max_straight {
        filter_neighbours(inp, u, |u, dir| u.dir == dir, |u, _| u.straight_for + 1)
      } else {
        filter_neighbours(
          inp,
          u,
          |u, dir| !u.dir.is_reversed(dir),
          |u, dir| if dir == u.dir { u.straight_for + 1 } else { 1 },
        )
      }
    },
  );

  let hi_x = inp.len() - 1;
  let hi_y = inp[hi_x].len() - 1;
  shortest_paths
    .into_iter()
    .filter_map(|(u, d)| {
      if u.x == hi_x && u.y == hi_y && max_straight <= u.straight_for {
        Some(d)
      } else {
        None
      }
    })
    .min()
    .unwrap()
}
