#![feature(iter_intersperse)]

use rust_aoc_util::print_day;

pub mod day1;
pub mod day10;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;

fn main() {
    print_day(1, day1::day1());
    print_day(2, day2::day2());
    print_day(3, day3::day3());
    print_day(4, day4::day4());
    print_day(5, day5::day5());
    print_day(6, day6::day6());
    print_day(7, day7::day7());
    print_day(10, day10::day10());
}
