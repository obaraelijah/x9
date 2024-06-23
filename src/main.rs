use structopt::StructOpt;
use x9::{cli, formatter, stdlib, modules};

fn main() -> Result<(), i32> {
    let opt = cli::Options::from_args();
    let mut sym_table = stdlib::create_stdlib_symbol_table(&opt);
    if opt.formatter {
        return formatter::format(&opt);
    }
    Ok(())
}
