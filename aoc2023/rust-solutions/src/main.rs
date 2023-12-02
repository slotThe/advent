#![feature(pattern)]

use rust_aoc_util::print_day;

pub mod day01;
pub mod day02;

fn main() {
  print_day(1, day01::day01());
  print_day(2, day02::day02());
}
