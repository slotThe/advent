pub mod day1;

fn main() {
    print_day(1, day1::get_nth_most_wanted(1), day1::get_nth_most_wanted(3));
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
