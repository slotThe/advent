use anyhow::Result;
use rust_aoc_util::converge;

fn not_isect((a, b): &(u32, u32), xs: &[u32]) -> bool {
  match (
    xs.iter().position(|x| x == a),
    xs.iter().position(|x| x == b),
  ) {
    (Some(ia), Some(ib)) => ia < ib,
    _ => true,
  }
}

fn reorder((a, b): &(u32, u32), xs: &mut [u32]) {
  xs.swap(
    xs.iter().position(|x| x == a).unwrap(),
    xs.iter().position(|x| x == b).unwrap(),
  );
}

fn main() -> Result<()> {
  let (ord, upd): (Vec<(u32, u32)>, Vec<Vec<u32>>) =
    std::fs::read_to_string("../inputs/day05.txt")?
      .trim()
      .split_once("\n\n")
      .map(|(ord, upd)| {
        (
          ord
            .split("\n")
            .map(|o| {
              let v: Vec<u32> = o.split("|").flat_map(|n| n.parse()).collect();
              (v[0], v[1])
            })
            .collect(),
          upd
            .split("\n")
            .map(|o| o.split(",").flat_map(|n| n.parse()).collect())
            .collect(),
        )
      })
      .unwrap();

  let (good, bad): (Vec<Vec<u32>>, Vec<Vec<u32>>) = upd
    .into_iter()
    .partition(|u| ord.iter().all(|o| not_isect(o, u)));

  println!("{}", good.iter().map(|xs| xs[xs.len() / 2]).sum::<u32>());

  println!(
    "{}",
    bad.into_iter().fold(0, |acc, u| {
      let xs = converge(u, |u| {
        let mut uu = u.clone();
        for o in &ord {
          if !not_isect(o, &uu) {
            reorder(o, &mut uu);
          }
        }
        uu
      });
      acc + xs[xs.len() / 2]
    })
  );

  Ok(())
}
