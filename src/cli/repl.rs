use std::fmt::Display;

use minitt_util::io::history_file;
use minitt_util::repl::{repl as repl_impl, MiniHelper, ReplEnvType};
use rustyline::Editor;

use voile::check::monad::{TCM, TCS as TCMS};
use voile::check::{check_decls, inline_metas};
use voile::syntax::abs::{trans_decls_contextual, trans_expr, Abs, TransState};
use voile::syntax::surf::{parse_expr_err_printed, parse_str_err_printed, Decl};
use voile_util::level::LiftEx;
use voile_util::meta::MI;

use crate::util::parse_file;

type TCS = (TCMS, TransState);

const PROMPT: &str = "=> ";
const QUIT_CMD: &str = ":quit";
const GAMMA_CMD: &str = ":gamma";
const CTX_CMD: &str = ":context";
const META_CMD: &str = ":meta";
const HELP_CMD: &str = ":help";
const LOAD_CMD: &str = ":load";
const INFER_CMD: &str = ":infer";
const EVAL_CMD: &str = ":eval";
const LEVEL_CMD: &str = ":level";

const LOAD_PFX: &str = ":load ";
const INFER_PFX: &str = ":infer ";
const EVAL_PFX: &str = ":eval ";
const LEVEL_PFX: &str = ":level ";

fn show_gamma(tcs: &TCS) {
    for val in &tcs.0.gamma {
        println!("val {};", val.ast);
    }
}

fn show_telescope(tcs: &TCS) {
    for val in &tcs.0.env {
        println!("let {};", val.ast);
    }
}

fn work(tcs: TCS, current_mode: ReplEnvType, line: &str) -> Option<TCS> {
    if line == QUIT_CMD {
        None
    } else if line.is_empty() {
        Some(tcs)
    } else if line == GAMMA_CMD {
        show_gamma(&tcs);
        Some(tcs)
    } else if line == CTX_CMD {
        show_telescope(&tcs);
        Some(tcs)
    } else if line == META_CMD {
        print!("{}", tcs.0.meta_context);
        Some(tcs)
    } else if line == HELP_CMD {
        help(current_mode);
        Some(tcs)
    } else if line.starts_with(LOAD_PFX) {
        Some(
            match parse_file(line.trim_start_matches(LOAD_CMD).trim_start()) {
                Some(decls) => update_tcs(tcs, decls),
                None => tcs,
            },
        )
    } else if line.starts_with(INFER_PFX) {
        Some(infer(tcs, line))
    } else if line.starts_with(LEVEL_PFX) {
        Some(level(tcs, line))
    } else if line.starts_with(EVAL_PFX) {
        Some(eval(tcs, line))
    } else if line.starts_with(':') {
        println!("Unrecognized command: {}", line);
        println!("Maybe you want to get some `:help`?");
        Some(tcs)
    } else {
        Some(match parse_str_err_printed(line).ok() {
            Some(decls) => update_tcs(tcs, decls),
            None => tcs,
        })
    }
}

fn infer(tcs: TCS, line: &str) -> TCS {
    expression_thing(tcs, line, INFER_CMD, |tcms, abs| {
        let (inferred, tcs) = tcms.infer(&abs)?;
        inline_metas(tcs, inferred)
    })
}

fn eval(tcs: TCS, line: &str) -> TCS {
    expression_thing(tcs, line, EVAL_CMD, |tcms, abs| Ok(tcms.evaluate(abs)))
}

fn level(tcs: TCS, line: &str) -> TCS {
    expression_thing(tcs, line, LEVEL_CMD, |tcms, abs| {
        let (val, tcs) = tcms.evaluate(abs);
        Ok((val.ast.level(), tcs))
    })
}

fn expression_thing<T: Display>(
    mut tcs: TCS,
    line: &str,
    cmd: &str,
    f: impl FnOnce(TCMS, Abs) -> TCM<(T, TCMS)>,
) -> TCS {
    if let Some(abs) = code_to_abs(&mut tcs, line.trim_start_matches(cmd).trim_start()) {
        if let Ok((show, tcms)) = f(tcs.0, abs).map_err(|err| eprintln!("{}", err)) {
            println!("{}", show);
            (tcms, tcs.1)
        } else {
            Default::default()
        }
    } else {
        tcs
    }
}

fn update_tcs(tcs: TCS, decls: Vec<Decl>) -> TCS {
    let mut state = tcs.1;
    state.meta_count = MI(tcs.0.meta_context.solutions().len());
    let state = trans_decls_contextual(state, decls)
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    let mut telescope = tcs.0;
    telescope
        .meta_context
        .expand_with_fresh_meta(state.meta_count);
    let tcs = check_decls(telescope, state.decls.clone())
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    (tcs, state)
}

pub fn code_to_abs(tcs: &mut TCS, code: &str) -> Option<Abs> {
    let trans_state = &mut tcs.1;
    trans_state.meta_count = MI(tcs.0.meta_context.solutions().len());
    trans_expr(
        parse_expr_err_printed(code).ok()?,
        &trans_state.decls,
        &mut trans_state.meta_count,
        &trans_state.context_mapping,
    )
    .map_err(|err| eprintln!("{}", err))
    .ok()
}

#[allow(clippy::print_literal)]
fn help(current_mode: ReplEnvType) {
    welcome_message(current_mode);
    println!(
        "\
         Commands:\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         ",
        QUIT_CMD,
        "Quit the REPL.",
        GAMMA_CMD,
        "Show current typing context.",
        META_CMD,
        "Show current meta solution context.",
        CTX_CMD,
        "Show current value context.",
        ":infer <EXPR>",
        "Infer the type of an expression.",
        ":eval <EXPR>",
        "Evaluate an expression, assuming it's well-typed.",
        ":level <EXPR>",
        "Find the universe level of an expression.",
        ":load <FILE>",
        "Load an external file.",
    );
}

fn welcome_message(current_mode: ReplEnvType) {
    println!(
        "Interactive voilec {}\n\
         Source code: https://github.com/owo-lang/voile-rs\n\
         Issue tracker: https://github.com/owo-lang/voile-rs/issues/new\n\n\

         The REPL has two modes: the RICH mode and the PLAIN mode.\n\
         Completion, history command, hints and (in the future) colored output are available in the \
         rich mode, but does not work entirely under Windows PowerShell ISE and Mintty \
         (Cygwin, MinGW and (possibly, depends on your installation) git-bash).\n\
         You are using the {} mode.\n\
         ",
        env!("CARGO_PKG_VERSION"),
        current_mode
    );
}

fn create_editor() -> Editor<MiniHelper> {
    minitt_util::repl::create_editor(&[
        QUIT_CMD, GAMMA_CMD, CTX_CMD, META_CMD, HELP_CMD, INFER_PFX, LOAD_PFX, EVAL_PFX, LEVEL_PFX,
    ])
}

pub fn repl(tcs: TCS, repl_kind: Option<ReplEnvType>) {
    if let Some(kind) = repl_kind {
        let history = || history_file("voilec").ok();
        repl_impl(
            tcs,
            PROMPT,
            kind,
            create_editor,
            history,
            welcome_message,
            work,
        );
    }
}
