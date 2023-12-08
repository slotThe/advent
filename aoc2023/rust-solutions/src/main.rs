#![feature(pattern)]
#![feature(int_roundings)]

use rust_aoc_util::print_day;

pub mod day01;
pub mod day02;
pub mod day03;
pub mod day04;
pub mod day05;
pub mod day06;
pub mod day08;

fn main() {
  print_day(1, day01::day01());
  print_day(2, day02::day02());
  print_day(3, day03::day03());
  print_day(4, day04::day04());
  print_day(5, day05::day05());
  print_day(6, day06::day06());
  print_day(8, day08::day08());
}
