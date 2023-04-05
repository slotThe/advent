#![feature(iter_intersperse)]

pub mod day1;
pub mod day10;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;
pub mod util;

fn main() {
    print_day(1, day1::day1());
    print_day(2, day2::day2());
    print_day(3, day3::day3());
    print_day(4, day4::day4());
    print_day(5, day5::day5());
    print_day(6, day6::day6());
    print_day(7, day7::day7());
    print_day2(10, day10::day10());
}

fn print_day<A, B>(num: usize, solutions: (A, B))
where
    A: std::fmt::Debug,
    B: std::fmt::Debug,
{
    println!("!!! Day {num} !!!");
    println!("First  task: {:?}", solutions.0);
    println!("Second task: {:?}\n", solutions.1);
}

fn print_day2<A, B>(num: usize, solutions: (A, B))
where
    A: std::fmt::Display,
    B: std::fmt::Display,
{
    println!("!!! Day {num} !!!");
    println!("First  task: {}", solutions.0);
    println!("Second task: \n{}\n", solutions.1);
}
