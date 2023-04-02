use anyhow::Result;
use nom::{combinator::all_consuming, Finish};

pub enum Part {
    One,
    Two,
}

pub fn parse<'a, R, F>(inp: &'a str, parser: F) -> Result<R, nom::error::Error<&'a str>>
where
    F: Fn(&'a str) -> nom::IResult<&'a str, R>,
{
    all_consuming(parser)(inp).finish().map(|(_, r)| r)
}

pub fn transpose<El: Clone>(xxs: Vec<Vec<El>>) -> Vec<Vec<El>> {
    transpose_with(|v| v, xxs)
}

pub fn transpose_with<I: Clone, O, F>(f: F, xxs: Vec<Vec<I>>) -> Vec<Vec<O>>
where
    F: Fn(Vec<I>) -> Vec<O>,
{
    (0..xxs[0].len())
        .map(|i| xxs.iter().map(|row| row[i].clone()).collect())
        .map(f)
        .collect()
}
