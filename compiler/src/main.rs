#![allow(clippy::module_inception, clippy::unwrap_or_default)]

mod parsed;
mod scope_tree;
mod test_util;

use std::{
    env,
    fmt::{self, Display},
    fs, io,
};

fn main() {
    let result = parse_args(&mut env::args().skip(1));
    match result {
        Ok(()) => (),
        Err(e) => eprintln!("{e}"),
    }
}

fn parse_args<I: Iterator<Item = String>>(args: &mut I) -> Result<(), Error> {
    match args.next().ok_or(Error::InsufficientArguments)?.as_str() {
        "--prettyprint" => prettyprint(args)?,
        _ => return Err(Error::UnknownArgument),
    };
    Ok(())
}
fn prettyprint<I: Iterator<Item = String>>(args: &mut I) -> Result<(), Error> {
    let file = args.next().ok_or(Error::InsufficientArguments)?;
    let source = fs::read_to_string(&file).map_err(Error::Io)?;
    let (parsed, errors) = scope_tree::parse(&source);
    println!("{parsed:?}\n{errors:#?}");
    Ok(())
}

enum Error {
    InsufficientArguments,
    UnknownArgument,
    Io(io::Error),
}
impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InsufficientArguments => f.write_str("Not enough arguments specified"),
            Error::UnknownArgument => f.write_str("Unknown argument"),
            Error::Io(error) => Display::fmt(error, f),
        }
    }
}
