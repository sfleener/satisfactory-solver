use good_lp::{Solution, SolverModel, constraint, default_solver, variables};
use std::fs::File;
use std::io::BufReader;
use crate::data::{Data, Settings};

mod data;

fn main() -> eyre::Result<()> {
    let settings = BufReader::new(File::open("../ui/data/default.json")?);
    let settings: Settings = serde_json::from_reader(settings)?;


    let data = BufReader::new(File::open("../ui/data/data.json")?);
    let data: Data = serde_json::from_reader(data)?;

    Ok(())
}
