pub fn day2() -> (usize, usize) {
    (
        get_score(parse_weapon, |_, me| *me),
        get_score(parse_game_result, pick_result),
    )
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum GameResult {
    Win = 6,
    Draw = 3,
    Lose = 0,
}

fn parse_game_result(w: char) -> GameResult {
    match w {
        'X' => GameResult::Lose,
        'Y' => GameResult::Draw,
        'Z' => GameResult::Win,
        _ => panic!("whoops"),
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Weapon {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

fn parse_weapon(w: char) -> Weapon {
    match w {
        'A' | 'X' => Weapon::Rock,
        'B' | 'Y' => Weapon::Paper,
        'C' | 'Z' => Weapon::Scissors,
        _ => panic!("whoops"),
    }
}

fn game_result(opponent: Weapon, me: Weapon) -> usize {
    use GameResult::*;
    use Weapon::*;
    (match (me, opponent) {
        (Rock, Scissors) => Win,
        (Scissors, Paper) => Win,
        (Paper, Rock) => Win,
        (a, b) => {
            if a == b {
                Draw
            } else {
                Lose
            }
        }
    }) as usize
}

fn pick_result(opponent: &Weapon, me: &GameResult) -> Weapon {
    use GameResult::*;
    use Weapon::*;
    match (*me, *opponent) {
        (Win, Rock) => Paper,
        (Win, Paper) => Scissors,
        (Win, Scissors) => Rock,
        (Lose, Rock) => Scissors,
        (Lose, Paper) => Rock,
        (Lose, Scissors) => Paper,
        (Draw, a) => a,
    }
}

fn get_score<A, F, G>(parse_me: F, transform_me: G) -> usize
where
    F: Fn(char) -> A,
    G: Fn(&Weapon, &A) -> Weapon,
    A: Copy,
{
    parse(
        std::fs::read_to_string("../inputs/day2.txt").unwrap(),
        parse_me,
    )
    .iter()
    .map(|(o, m)| {
        let my_weapon = transform_me(o, m);
        game_result(*o, my_weapon) + my_weapon as usize
    })
    .sum()
}

fn parse<A, F>(input: String, parse_me: F) -> Vec<(Weapon, A)>
where
    F: Fn(char) -> A,
{
    let uff = |l: &str, n: usize| l.chars().nth(n).unwrap();
    input
        .lines()
        .map(|l| (parse_weapon(uff(l, 0)), parse_me(uff(l, 2))))
        .collect()
}
