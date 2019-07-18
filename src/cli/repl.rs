use std::fmt::Display;
use std::io::{stdin, stdout, Write};

use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{CompletionType, Config, Context, Editor, Helper};

use voile::check::check_decls;
use voile::check::monad::{MetaSolution, TCM, TCS as TCMS};
use voile::syntax::abs::{trans_decls_contextual, trans_expr, Abs, TransState};
use voile::syntax::common::MI;
use voile::syntax::core::LiftEx;
use voile::syntax::surf::{parse_expr_err_printed, parse_str_err_printed, Decl};

use crate::util::parse_file;

type TCS = (TCMS, TransState);

struct VoileHelper {
    all_cmd: Vec<String>,
    file_completer: FilenameCompleter,
}

impl Completer for VoileHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
        if line.starts_with(LOAD_PFX) {
            return self.file_completer.complete(line, pos, ctx);
        }
        let vec = self
            .all_cmd
            .iter()
            .filter(|cmd| cmd.starts_with(line))
            .map(|str| Pair {
                display: str.clone(),
                replacement: str.clone(),
            })
            .collect();
        Ok((0, vec))
    }
}

impl Hinter for VoileHelper {
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        if line.len() < 2 {
            return None;
        }
        self.all_cmd
            .iter()
            .filter(|cmd| cmd.starts_with(line))
            .cloned()
            .map(|cmd| cmd[pos..].to_string())
            .next()
    }
}

impl Highlighter for VoileHelper {}

impl Helper for VoileHelper {}

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

fn show_meta_solutions(tcs: &TCS) {
    use MetaSolution::*;
    let mut index = 0;
    for solution in tcs.0.meta_solutions() {
        match solution {
            Solved(solution) => println!("{}: {}", index, solution),
            Unsolved => println!("{}: ???", index),
            Inlined => println!("<inlined out>"),
        }
        index += 1;
    }
}

fn repl_work(tcs: TCS, current_mode: &str, line: &str) -> Option<TCS> {
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
        show_meta_solutions(&tcs);
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
    expression_thing(tcs, line, INFER_CMD, |tcms, abs| tcms.infer(&abs))
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
    state.meta_count = MI(tcs.0.meta_solutions().len());
    let state = trans_decls_contextual(state, decls)
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    let mut telescope = tcs.0;
    telescope.expand_with_fresh_meta(state.meta_count);
    let tcs = check_decls(telescope, state.decls.clone())
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    (tcs, state)
}

pub fn code_to_abs(tcs: &mut TCS, code: &str) -> Option<Abs> {
    let trans_state = &mut tcs.1;
    trans_state.meta_count = MI(tcs.0.meta_solutions().len());
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
fn help(current_mode: &str) {
    repl_welcome_message(current_mode);
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

fn repl_welcome_message(current_mode: &str) {
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

pub fn repl_plain(mut tcs: TCS) {
    repl_welcome_message("PLAIN");
    let stdin = stdin();
    loop {
        print!("{}", PROMPT);
        stdout().flush().expect("Cannot flush stdout!");
        let mut line = String::new();
        stdin.read_line(&mut line).expect("Cannot flush stdout!");
        if let Some(ok) = repl_work(tcs, "PLAIN", line.trim()) {
            tcs = ok;
        } else {
            break;
        };
    }
}

pub fn repl(mut tcs: TCS) {
    repl_welcome_message("RICH");
    let all_cmd: Vec<_> = vec![
        QUIT_CMD, GAMMA_CMD, CTX_CMD, META_CMD, HELP_CMD, INFER_PFX, LOAD_PFX, EVAL_PFX, LEVEL_PFX,
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    let mut r = Editor::with_config(
        Config::builder()
            .history_ignore_space(true)
            .completion_type(CompletionType::Circular)
            .build(),
    );
    r.set_helper(Some(VoileHelper {
        all_cmd,
        file_completer: FilenameCompleter::new(),
    }));
    // Load history?
    loop {
        match r.readline(PROMPT) {
            Ok(line) => {
                let line = line.trim();
                r.add_history_entry(line);
                if let Some(ok) = repl_work(tcs, "RICH", line) {
                    tcs = ok;
                } else {
                    break;
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted by Ctrl-c.");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Interrupted by Ctrl-d");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        };
    }
    // Write history?
}
