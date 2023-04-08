use anyhow::Result;

pub fn day1() -> Result<(i32, usize)> {
    Ok(solve(std::fs::read_to_string("../inputs/day1.txt")?))
}

// This may or may not look nicer as a scan, but I figured that I might
// as well try to do things "the Rust way" (whatever that means).
pub fn solve(s: String) -> (i32, usize) {
    let mut floor = 0; // dude mutability lmao
    let mut basement = vec![];
    s.chars().enumerate().for_each(|(i, c)| {
        match c {
            '(' => floor += 1,
            ')' => floor -= 1,
            _ => (),
        };
        if floor == -1 {
            basement.push(i + 1);
        }
    });
    (floor, basement[0])
}
