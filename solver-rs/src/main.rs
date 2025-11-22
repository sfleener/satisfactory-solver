use crate::data::{Data, Settings};
use crate::solver::PreparedModel;
use good_lp::{Solution, SolverModel};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

mod data;
mod solver;

fn main() -> eyre::Result<()> {
    let settings = BufReader::new(File::open("../ui/data/default.json")?);
    let mut settings: Settings = serde_json::from_reader(settings)?;

    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_1_C".into(), 6.0);
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_2_C".into(), 5.0);
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_3_C".into(), 2.5);
    settings.phase = Some(2);

    settings.floor_resource_limits(1e-5);

    let data = BufReader::new(File::open("../ui/data/data.json")?);
    let data: Data = serde_json::from_reader(data)?;

    let solved = PreparedModel::new(&data, &mut settings).solve()?;
    let values = solved.into_values(&settings, &data);

    println!("{values:#?}");

    Ok(())
}
