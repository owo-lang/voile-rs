#[macro_use]
extern crate voile;

mod args;
mod util;

fn main() {
    let args = args::pre();

    let checked = args
        .file
        .clone()
        .and_then(|s| util::parse_file(s.as_str()))
        .map(|ast| {
            println!("Parse successful.");
        })
        .unwrap_or_else(|| Default::default());
    println!("Hello Voile!");
}
