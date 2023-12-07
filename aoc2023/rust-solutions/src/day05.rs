use itertools::Itertools;
use rust_aoc_util::interval::Interval;

type AlmanacMap = (isize, isize, isize);

pub fn day05() -> (isize, isize) {
  let inp = std::fs::read_to_string("../inputs/day05.txt").unwrap();
  let inp: Vec<Vec<_>> = inp.split("\n\n").map(|s| s.lines().collect()).collect();
  let (seeds, almanac_maps): (Vec<isize>, Vec<Vec<AlmanacMap>>) = (
    p_seeds(inp[0][0]),
    inp.iter().dropping(1).map(|m| p_maps(m)).collect(),
  );

  (
    solve(
      seeds.iter().map(|&s| Interval::from_len(s, 1)).collect(),
      &almanac_maps,
    ),
    solve(
      seeds
        .chunks(2)
        .map(|ch| Interval::from_len(ch[0], ch[1]))
        .collect(),
      &almanac_maps,
    ),
  )
}

fn solve(seedvals: Vec<Interval>, almanac_maps: &[Vec<AlmanacMap>]) -> isize {
  almanac_maps
    .iter()
    .fold(seedvals, |seeds, maps| {
      seeds
        .into_iter()
        .flat_map(|x| calc_intersections(x, maps))
        .collect()
    })
    .into_iter()
    .map(Interval::beg)
    .min()
    .unwrap()
}

fn calc_intersections(ival: Interval, maps: &[AlmanacMap]) -> Vec<Interval> {
  let mut res = vec![];
  let mut source_ival = Some(ival);
  for &(dest, source, len) in maps {
    match source_ival {
      None => return res,
      Some(iv) => {
        let ival2 = Interval::from_len(source, len);
        if let Some(Interval { beg, end }) = iv.intersect(&ival2) {
          res.push(Interval::from_len(beg + (dest - source), 1 + end - beg));
          source_ival = iv.without(&ival2);
        }
      },
    }
  }
  // If there is some interval left, verbatim copy it.
  source_ival.into_iter().for_each(|iv| res.push(iv));
  res
}

///////////////////////////////////////////////////////////////////////
// Parsing

fn p_seeds(seeds: &str) -> Vec<isize> {
  seeds.split(' ').flat_map(|w| w.parse::<isize>()).collect()
}

fn p_maps(maps: &[&str]) -> Vec<AlmanacMap> {
  maps
    .iter()
    .filter_map(|l| {
      let v: Vec<_> = l.split(' ').flat_map(|w| w.parse::<isize>()).collect();
      (!v.is_empty()).then(|| (v[0], v[1], v[2]))
    })
    .collect()
}
