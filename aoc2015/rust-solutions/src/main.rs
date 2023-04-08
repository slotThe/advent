use anyhow::Result;
use rust_aoc_util::print_day;

pub mod day1;
pub mod day2;
pub mod day3;

fn main() -> Result<()> {
    print_day(1, day1::day1()?);
    print_day(2, day2::day2()?);
    print_day(3, day3::day3()?);
    Ok(())
}
