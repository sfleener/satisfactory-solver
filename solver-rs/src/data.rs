use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    pub resource_limits: HashMap<String, f64>,
    pub weights: Weights,
    pub recipes_off: HashSet<String>,
    pub inputs: HashMap<String, f64>,
    pub outputs: HashMap<String, f64>,
    pub max_item: Option<serde_json::Value>,
    #[serde(rename = "checkbox_Nuclear Waste")]
    pub force_nuclear_waste: bool,
    pub integer_recipes: bool,
}

impl Settings {
    pub fn floor_resource_limits(&mut self, floor: f64) {
        for v in self.resource_limits.values_mut() {
            if *v < floor {
                *v = floor;
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Weights {
    #[serde(rename = "Power Use")]
    pub power_use: f64,
    #[serde(rename = "Item Use")]
    pub item_use: f64,
    #[serde(rename = "Building Use")]
    pub building_use: f64,
    #[serde(rename = "Resource Use")]
    pub resource_use: f64,
    #[serde(rename = "Buildings Scaled")]
    pub buildings_scaled: f64,
    #[serde(rename = "Resources Scaled")]
    pub resources_scaled: f64,
    #[serde(rename = "Nuclear Waste")]
    pub nuclear_waste: f64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct RawData {
    pub items: HashMap<String, Arc<Item>>,
    pub resources: HashMap<String, Arc<Resource>>,
    pub recipes: HashMap<String, Recipe>,
    pub machines: HashMap<String, Arc<Machine>>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Item {
    pub name: String,
    pub energy: Option<f64>,
    pub form: Option<Form>,
    pub points: f64,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub enum Form {
    #[serde(rename = "RF_SOLID")]
    Solid,
    #[serde(rename = "RF_LIQUID")]
    Liquid,
    #[serde(rename = "RF_GAS")]
    Gas,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Resource {
    pub name: String,
    pub energy: f64,
    pub form: Form,
    pub points: f64,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Recipe {
    pub name: String,
    pub time: f64,
    pub ingredients: Vec<RecipeItem>,
    pub products: Vec<RecipeItem>,
    pub machine: String,
    pub power_use: f64,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct RecipeItem {
    pub item: String,
    pub amount: f64,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct Machine {
    pub name: String,
    pub power_use: f64,
    pub power_produced: f64,
}
