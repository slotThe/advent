use anyhow::Result;
use rust_aoc_util::print_day;

pub mod day1;

fn main() -> Result<()> {
    print_day(1, day1::day1()?);
    Ok(())
}
