pub mod day1;
pub mod day2;
pub mod day3;
pub mod util;

fn main() {
    print_day(
        1,
        day1::get_nth_most_wanted(1),
        day1::get_nth_most_wanted(3),
    );
    print_day(2, day2::day2(util::Part::One), day2::day2(util::Part::Two));
    println!("{:?}", day3::parse());
}

fn print_day<A, B>(num: usize, one: A, two: B)
where
    A: std::fmt::Debug,
    B: std::fmt::Debug,
{
    println!("!!! Day {num} !!!");
    println!("First  task: {:?}", one);
    println!("Second task: {:?}\n", two);
}
