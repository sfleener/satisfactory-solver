use crate::data::{RawData, Settings};
use crate::solver::create_model;
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
        .insert("Desc_SpaceElevatorPart_1_C".to_string(), 5.0);
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_2_C".to_string(), 5.0);
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_3_C".to_string(), 2.5);

    let data = BufReader::new(File::open("../ui/data/data.json")?);
    let data: RawData = serde_json::from_reader(data)?;

    let (model, variables) = create_model(&data, &mut settings);

    let result = model.solve()?;

    let status = result.status();
    println!("status: {status:?}");

    let power_use = result.value(variables.power_use);
    let inputs = variables
        .inputs
        .into_iter()
        .map(|(k, var)| (k, result.value(var)))
        .filter(|(_, v)| *v > 0.001)
        .collect::<HashMap<_, _>>();
    let outputs = variables
        .outputs
        .into_iter()
        .map(|(k, var)| (k, result.value(var)))
        .filter(|(_, v)| *v > 0.001)
        .collect::<HashMap<_, _>>();
    let items = variables
        .items
        .into_iter()
        .map(|(k, var)| (k, result.value(var)))
        .filter(|(_, v)| *v > 0.001)
        .collect::<HashMap<_, _>>();
    let recipes = variables
        .recipes
        .into_iter()
        .map(|(k, var)| (k, result.value(var)))
        .filter(|(_, v)| *v > 0.001)
        .collect::<HashMap<_, _>>();

    println!("power: {power_use}");
    println!("inputs: {inputs:?}");
    println!("outputs: {outputs:?}");
    println!("items: {items:?}");
    println!("recipes: {recipes:?}");

    Ok(())
}
