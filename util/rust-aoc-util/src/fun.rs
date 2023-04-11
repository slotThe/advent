pub fn zip_with<R, U, C>(combine: C, left: U, right: U) -> impl Iterator<Item = R>
where
  U: Iterator,
  C: Fn(U::Item, U::Item) -> R,
{
  left.zip(right).map(move |(l, r)| combine(l, r))
}
