use itertools::Itertools;

type AlmanacMap = (usize, usize, usize);
type Interval = (usize, usize);

pub fn day05() -> (usize, usize) {
  let inp = std::fs::read_to_string("../inputs/day05.txt").unwrap();
  let inp: Vec<Vec<_>> = inp.split("\n\n").map(|s| s.lines().collect()).collect();
  let (seeds, almanac_maps): (Vec<usize>, Vec<Vec<AlmanacMap>>) = (
    p_seeds(inp[0][0]),
    inp.iter().dropping(1).map(|m| p_maps(m)).collect(),
  );

  (
    solve(
      seeds.iter().map(|&s| into_interval(s, 1)).collect(),
      &almanac_maps,
    ),
    solve(
      seeds
        .iter()
        .chunks(2)
        .into_iter()
        .map(|mut ch| into_interval(*ch.next().unwrap(), *ch.next().unwrap()))
        .collect(),
      &almanac_maps,
    ),
  )
}

fn solve(seedvals: Vec<Interval>, almanac_maps: &[Vec<AlmanacMap>]) -> usize {
  almanac_maps
    .iter()
    .fold(seedvals, |seeds, maps| {
      seeds
        .into_iter()
        .flat_map(|x| calc_intersections(x, maps))
        .collect()
    })
    .into_iter()
    .map(|(a, _)| a)
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
        let ival2 = into_interval(source, len);
        if let Some((s, e)) = intersect(iv, ival2) {
          res.push(into_interval(s + (dest - source), 1 + e - s));
          source_ival = subtract(iv, ival2);
        }
      },
    }
  }
  // If there is some interval left, verbatim copy it.
  source_ival.into_iter().for_each(|iv| res.push(iv));
  res
}

///////////////////////////////////////////////////////////////////////
// Interval util

fn into_interval(s: usize, e: usize) -> Interval { (s, s + e - 1) }

fn intersect((s1, e1): Interval, (s2, e2): Interval) -> Option<Interval> {
  (s1.max(s2) <= e1.min(e2)).then_some((s1.max(s2), e1.min(e2)))
}

fn subtract((s1, e1): Interval, (s2, e2): Interval) -> Option<Interval> {
  ((s2 > s1) || (e1 > e2)).then(|| {
    let (s, e) = if s1 < s2 {
      (s1, (s2 - 1).min(e1))
    } else {
      (s1.max(e2 + 1), e1)
    };
    (e >= s).then_some((s, e))
  })?
}

///////////////////////////////////////////////////////////////////////
// Parsing

fn p_seeds(seeds: &str) -> Vec<usize> {
  seeds.split(' ').flat_map(|w| w.parse::<usize>()).collect()
}

fn p_maps(maps: &[&str]) -> Vec<AlmanacMap> {
  maps
    .iter()
    .filter_map(|l| {
      let v: Vec<_> = l.split(' ').flat_map(|w| w.parse::<usize>()).collect();
      (!v.is_empty()).then(|| (v[0], v[1], v[2]))
    })
    .collect()
}
