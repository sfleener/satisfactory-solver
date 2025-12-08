use crate::data::{Data, Settings};
use crate::outputs::output_graph;
use crate::solver::PreparedModel;
use std::fs::File;
use std::io::BufReader;

mod data;
mod outputs;
mod rational;
mod solver;

fn main() -> eyre::Result<()> {
    let settings = std::fs::read_to_string("data/default.json5")?;
    let mut settings: Settings = json5::from_str(&settings)?;

    println!("settings: {settings:?}");

    settings.floor_resource_limits(1e-5.into());

    let data = BufReader::new(File::open("data/data.json")?);
    let data: Data = serde_json::from_reader(data)?;

    let solved = PreparedModel::new(&data, &settings).solve()?;
    let values = solved.into_values(&settings, &data);

    // println!("{values:#?}");

    output_graph(&settings, &data, &values);

    Ok(())
}
