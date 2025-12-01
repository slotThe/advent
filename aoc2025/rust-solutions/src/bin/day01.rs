fn main() {
    let inp: Vec<i32> = std::fs::read_to_string("../inputs/day01.txt")
        .unwrap()
        .lines()
        .map(|l| {
            let f = |x: i32| if l.starts_with("L") { -x } else { x };
            f(l.trim_start_matches(['L', 'R']).parse().unwrap())
        })
        .collect();

    let one = inp
        .iter()
        .scan(50, |d, x| {
            *d = (*d + x).rem_euclid(100);
            Some(*d)
        })
        .filter(|x| *x == 0)
        .count();
    assert_eq!(1011, one);
    println!("{one}");

    let two = inp
        .iter()
        .fold((0, 50), |(z, d), x| {
            (
                z + (x.abs() + (d * x.signum()).rem_euclid(100)) / 100,
                (d + x).rem_euclid(100),
            )
        })
        .0;
    assert_eq!(5937, two);
    println!("{:?}", two);
}
