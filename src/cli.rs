use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "x9", about= "x9 Programming Language")]
pub struct Options {
    #[structopt(short = "d", long = "debugger", help = "WIP: :^)")]
    pub debugger: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options { 
            debugger: false,
        }
    }
}