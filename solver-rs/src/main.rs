use good_lp::{Solution, SolverModel, constraint, default_solver, variables};
use std::fs::File;
use std::io::BufReader;
use crate::data::{RawData, Settings};
use crate::solver::create_model;

mod data;
mod solver;

fn main() -> eyre::Result<()> {
    let settings = BufReader::new(File::open("../ui/data/default.json")?);
    let mut settings: Settings = serde_json::from_reader(settings)?;


    let data = BufReader::new(File::open("../ui/data/data.json")?);
    let data: RawData = serde_json::from_reader(data)?;

    let (model, variables) = create_model(&data, &mut settings);

    let result = model.solve()?;

    let status = result.status();
    println!("status: {status:?}");
    let points = result.value(variables.sink_points);
    println!("{points}");

    Ok(())
}
