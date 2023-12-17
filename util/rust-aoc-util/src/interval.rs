#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Interval {
  pub beg: isize,
  pub end: isize,
}

impl Interval {
  pub fn beg(self) -> isize { self.beg }

  pub fn end(self) -> isize { self.end }

  /// Create an interval from a starting point, and a length.
  pub fn from_len(beg: isize, l: isize) -> Interval {
    Interval {
      beg,
      end: beg + l - 1,
    }
  }

  /// Intersect two intervals.
  pub fn intersect(self, i2: &Interval) -> Option<Interval> {
    let beg = self.beg.max(i2.beg);
    let end = self.end.min(i2.end);
    (beg <= end).then_some(Interval { beg, end })
  }

  /// Return self without the interval i2.
  pub fn without(self, i2: &Interval) -> Option<Interval> {
    match self.intersect(i2) {
      None => Some(self), // Disjoint, so return self.
      Some(_) => {
        let (beg, end) = if self.beg < i2.beg {
          (self.beg, (i2.beg - 1).min(self.end))
        } else {
          (self.beg.max(i2.end + 1), self.end)
        };
        // Smother nonsensical intervals.
        (end >= beg).then_some(Interval { beg, end })
      },
    }
  }
}
