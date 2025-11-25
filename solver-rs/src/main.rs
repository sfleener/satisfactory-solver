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
    let settings = BufReader::new(File::open("data/default.json")?);
    let mut settings: Settings = serde_json::from_reader(settings)?;

    // settings
    //     .outputs
    //     .insert("Desc_SpaceElevatorPart_1_C".into(), 6.into());
    // settings
    //     .outputs
    //     .insert("Desc_SpaceElevatorPart_2_C".into(), 5.into());
    // settings
    //     .outputs
    //     .insert("Desc_SpaceElevatorPart_3_C".into(), 2.5.into());
    // settings.phase = Some(2);

    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_2_C".into(), 25.into());
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_4_C".into(), 5.into());
    settings
        .outputs
        .insert("Desc_SpaceElevatorPart_5_C".into(), 1.into());
    settings
        .outputs
        .insert("Desc_ModularFrameHeavy_C".into(), 10.into());
    settings
        .outputs
        .insert("Desc_Motor_C".into(), 10.into());
    settings
        .outputs
        .insert("Desc_Computer_C".into(), 10.into());
    settings
        .outputs
        .insert("Desc_SteelPlateReinforced_C".into(), 20.into());
    settings.phase = Some(3);

    settings.floor_resource_limits(1e-5.into());

    let data = BufReader::new(File::open("data/data.json")?);
    let data: Data = serde_json::from_reader(data)?;

    let values = loop {
        let solved = match PreparedModel::new(&data, &settings).solve() {
            Ok(solved) => solved,
            Err(e) => {
                println!("Retrying: {e}");
                continue;
            }
        };
        break solved.into_values(&settings, &data);
    };

    // println!("{values:#?}");

    output_graph(&settings, &data, &values);

    Ok(())
}
