use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "x9", about = "x9 Programming Language")]
pub struct Options {
    pub files: Vec<String>,
    #[structopt(
        short = "e",
        long,
        help = "Execute the file(s), and then load the interpreter"
    )]
    pub load_file: bool,

    #[structopt(short = "d", long = "debugger", help = "WIP: :^)")]
    pub debugger: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options { 
            files: Vec::with_capacity(0),
            load_file: false,
            debugger: false, 
        }
    }
}
