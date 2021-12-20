use anyhow::Result;
use nom::{combinator::all_consuming, Finish};

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

pub trait AdventString {
    fn pp(&self) -> String;
}

impl<A: ToString> AdventString for Option<A> {
    fn pp(&self) -> String {
        match self {
            None => "None".to_string(),
            Some(x) => x.to_string(),
        }
    }
}

impl<A: ToString> AdventString for Result<A> {
    fn pp(&self) -> String {
        match self {
            Err(e) => e.to_string(),
            Ok(x) => x.to_string(),
        }
    }
}

// Yup, this is really necessary, because apparently
//
//     impl<A: ToString> AdventString for A
//
// doesn't work and triggers E0119. Yikes.

impl AdventString for String {
    fn pp(&self) -> String {
        self.to_string()
    }
}

impl AdventString for i32 {
    fn pp(&self) -> String {
        self.to_string()
    }
}

impl AdventString for u32 {
    fn pp(&self) -> String {
        self.to_string()
    }
}

impl AdventString for u128 {
    fn pp(&self) -> String {
        self.to_string()
    }
}

impl AdventString for usize {
    fn pp(&self) -> String {
        self.to_string()
    }
}