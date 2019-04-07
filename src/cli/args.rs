use clap::{App, Shell};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "voilec",
    rename_all = "kebab-case",
    raw(setting = "structopt::clap::AppSettings::ColoredHelp")
)]
pub struct CliOptions {
    #[structopt(name = "FILE")]
    pub file: Option<String>,
}

fn app<'a, 'b>() -> App<'a, 'b> {
    let extra_help = "For extra help please head to \
                      https://github.com/owo-lang/voile-rs/issues/new";
    // Introduced a variable because stupid CLion :(
    let app: App = CliOptions::clap();
    app.after_help(extra_help)
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
}

pub fn pre() -> CliOptions {
    CliOptions::from_clap(&app().get_matches())
}
