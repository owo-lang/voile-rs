extern crate voile;

use crate::repl::code_to_abs;
use minitt_util::repl::ReplEnvType;
use voile::check::check_decls;
use voile::check::monad::TCS;
use voile::syntax::abs::{trans_decls_contextual, TransState};

mod args;
mod repl;
mod util;

fn main_file(
    file_ref: Option<&String>,
    quiet: bool,
    parse_only: bool,
) -> Option<(TCS, TransState)> {
    let decls = util::parse_file(file_ref?)?;
    if !quiet {
        println!("Parse successful.");
    }
    if parse_only {
        return None;
    }

    // Translate to abstract syntax
    let abs_decls = trans_decls_contextual(Default::default(), decls).unwrap_or_else(|err| {
        eprintln!("{}", err);
        eprintln!("Nou!");
        std::process::exit(1)
    });

    // Type Check
    let mut tcs = TCS::default();
    tcs.meta_context
        .expand_with_fresh_meta(abs_decls.meta_count);
    let checked = check_decls(tcs, abs_decls.decls.clone()).unwrap_or_else(|err| {
        eprintln!("{}", err);
        eprintln!("Change my mind!");
        std::process::exit(1)
    });

    if !quiet {
        for (ty, val) in checked.gamma.iter().zip(checked.env.iter()) {
            println!("sign: {}", ty.ast);
            println!("body: {}", val.ast);
        }

        // Meme: https://github.com/owo-lang/voile-rs/issues/56
        println!("Checkmate, dram!");
    }

    Some((checked, abs_decls))
}

fn main() {
    let args = args::pre();

    let mut checked =
        main_file(args.file.as_ref(), args.quiet, args.parse_only).unwrap_or_default();

    if let Some(abs) = args
        .evaluate
        .and_then(|code| code_to_abs(&mut checked, &code))
    {
        let (tcs, trans_st) = checked;
        let (core, tcs) = tcs.evaluate(abs);
        println!("{}", core.ast);
        checked = (tcs, trans_st);
    }

    // REPL
    repl::repl(
        checked,
        if args.interactive_plain {
            Some(ReplEnvType::Plain)
        } else if args.interactive {
            Some(ReplEnvType::Rich)
        } else {
            None
        },
    );
}
