use std::collections::HashMap;

use anyhow::Result;

fn blink(hm: &mut HashMap<isize, isize>) {
  let mut v = vec![];
  hm.iter().for_each(|(&x, &count)| {
    v.push((x, -count)); // EXTERMINATE
    let s = format!("{x}");
    if x == 0 {
      v.push((1, count));
    } else if s.len() % 2 == 0 {
      let (a, b) = s.split_at(s.len() / 2);
      v.push((a.parse().unwrap(), count));
      v.push((b.parse().unwrap(), count));
    } else {
      v.push((x * 2024, count));
    }
  });
  v.into_iter().for_each(|(y, count)| {
    hm.entry(y)
      .and_modify(|c| {
        *c += count;
      })
      .or_insert(count);
  });
}

fn main() -> Result<()> {
  let mut inp: HashMap<isize, isize> = std::fs::read_to_string("../inputs/day11.txt")?
    .trim()
    .split(" ")
    .map(|x| (x.parse().unwrap(), 1))
    .collect();

  (0..25).for_each(|_| blink(&mut inp));
  let one = inp.values().sum::<isize>();
  assert_eq!(one, 239714);
  println!("{one}");

  (25..75).for_each(|_| blink(&mut inp));
  let two = inp.values().sum::<isize>();
  assert_eq!(284973560658514, two);
  println!("{two}");

  Ok(())
}
