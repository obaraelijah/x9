use structopt::StructOpt;
use x9::cli;

fn main() -> Result<(), anyhow::Error> {
    let _opt = cli::Options::from_args();
    Ok(())
}
