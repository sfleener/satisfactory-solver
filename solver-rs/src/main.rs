use crate::data::{Data, Settings};
use crate::outputs::output_graph;
use crate::solver::PreparedModel;
use clap::Parser;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

mod data;
mod outputs;
mod rational;
mod solver;

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    solution: Option<PathBuf>,
}

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let settings = std::fs::read_to_string("data/default.json5")?;
    let mut settings: Settings = json5::from_str(&settings)?;

    println!("settings: {settings:?}");

    settings.floor_resource_limits(1e-5.into());

    let data = BufReader::new(File::open("data/data.json")?);
    let data: Data = serde_json::from_reader(data)?;

    let values = if let Some(solution) = args.solution {
        serde_json::from_reader(BufReader::new(File::open(solution)?))?
    } else {
        let solved = PreparedModel::new(&data, &settings).solve()?;
        let values = solved.into_values(&settings, &data);
        std::fs::write("sln.json", serde_json::to_string(&values)?)?;
        values
    };

    // println!("{values:#?}");

    output_graph(&settings, &data, &values);

    Ok(())
}
