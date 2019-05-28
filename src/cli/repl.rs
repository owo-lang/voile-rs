use std::io::{stdin, stdout, Write};

use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::{CompletionType, Config, Context, Editor, Helper};

use voile::check::check_main;
use voile::check::monad::TCS as TCMS;
use voile::syntax::abs::{trans_decls_contextual, DeclTCS};
use voile::syntax::surf::{parse_str_err_printed, Decl};

use crate::util::parse_file;

type TCS = (TCMS, DeclTCS);

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
        Ok((
            0,
            self.all_cmd
                .iter()
                .filter(|cmd| cmd.starts_with(line))
                .map(|str| Pair {
                    display: str.clone(),
                    replacement: str.clone(),
                })
                .collect(),
        ))
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

const PROMPT: &'static str = "=> ";
const QUIT_CMD: &'static str = ":quit";
const GAMMA_CMD: &'static str = ":gamma";
const CTX_CMD: &'static str = ":context";
const HELP_CMD: &'static str = ":help";
const LOAD_CMD: &'static str = ":load";

const LOAD_PFX: &'static str = ":load ";

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

fn update_tcs(tcs: TCS, decls: Vec<Decl>) -> TCS {
    let (abs_decls, name_ctx) = trans_decls_contextual(tcs.1, decls)
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    let tcs = check_main(abs_decls.clone())
        .map_err(|err| eprintln!("{}", err))
        .unwrap_or_default();
    (tcs, (abs_decls, name_ctx))
}

fn help(current_mode: &str) {
    repl_welcome_message(current_mode);
    println!(
        "\
         Commands:\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         {:<20} {}\n\
         ",
        QUIT_CMD,
        "Quit the REPL.",
        GAMMA_CMD,
        "Show current typing context.",
        CTX_CMD,
        "Show current value context.",
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
    let all_cmd: Vec<_> = vec![QUIT_CMD, GAMMA_CMD, CTX_CMD, HELP_CMD, LOAD_CMD]
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
