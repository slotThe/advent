use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    error::Error,
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use rust_aoc_util::{parse, transpose_with};

pub struct Move {
    muv: u32,
    from: u32,
    to: u32,
}

pub fn day5() -> (Option<String>, Option<String>) {
    (solve(|s| s), solve(|s| s.rev()))
}

pub fn solve<Out, Fun>(tamper: Fun) -> Option<String>
where
    // XXX: I don't know why a more generic Iterator does not work here.
    Fun: Fn(std::vec::IntoIter<String>) -> Out,
    Out: Iterator<Item = String>,
{
    let move_crates = |crates: &mut [Vec<String>], m: Move| {
        let mut moves = tamper(
            (0..m.muv)
                .map(|_| crates[m.from as usize].pop().unwrap())
                .collect::<Vec<String>>() // ???
                .into_iter(),
        )
        .collect();
        crates[m.to as usize].append(&mut moves);
    };
    get_input().map(|(mut crates, mvs)| {
        mvs.into_iter().for_each(|mv| move_crates(&mut crates, mv));
        crates
            .iter()
            .map(|stack| stack[stack.len() - 1].clone())
            .collect::<Vec<_>>()
            .concat()
    })
}

pub fn get_input() -> Option<(Vec<Vec<String>>, Vec<Move>)> {
    let p_first = |s| -> Result<Vec<String>, Error<&str>> {
        parse(s, p_crates).map(|s| s.iter().map(|s| s.to_string()).collect())
    };
    std::fs::read_to_string("../inputs/day5.txt")
        .ok()?
        .split_once("\n\n")
        .map(|(crates, muvs)| {
            (
                transpose_with(
                    |row| {
                        row.into_iter()
                            .filter(|s| !s.chars().all(char::is_whitespace))
                            .rev()
                            .collect()
                    },
                    crates.lines().flat_map(p_first).collect(),
                ),
                muvs.lines().map(|s| parse(s, p_move).unwrap()).collect(),
            )
        })
}

fn p_crates<'a>(inp: &'a str) -> IResult<&'a str, Vec<&'a str>> {
    let p_single = |inp: &'a str| -> IResult<&'a str, &'a str> {
        alt((
            delimited(tag("["), take(1_u32), tag("]")), // crate
            tag("   "),                                 // no crate
        ))(inp)
    };
    separated_list0(tag(" "), p_single)(inp)
}

fn p_move(inp: &str) -> IResult<&str, Move> {
    nom::combinator::map(
        tuple((
            preceded(tag("move "), nom::character::complete::u32),
            preceded(tag(" from "), nom::character::complete::u32),
            preceded(tag(" to "), nom::character::complete::u32),
        )),
        |(muv, from, to)| Move {
            muv,
            from: from - 1,
            to: to - 1,
        },
    )(inp)
}
