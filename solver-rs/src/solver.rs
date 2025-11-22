use crate::data::{Form, Item, RawData, Recipe, Resource, Settings};
use good_lp::solvers::highs::HighsProblem;
use good_lp::variable::UnsolvedProblem;
use good_lp::{
    Constraint, Expression, ProblemVariables, SolverModel, Variable, constraint, default_solver,
    variable, variables,
};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::iter::Sum;
use std::sync::Arc;

struct Data {
    resources: HashSet<String>,
    recipes: HashSet<String>,
    products: HashSet<String>,
    ingredients: HashSet<String>,
    all_items: HashSet<String>,
}

fn extract_items(raw_data: &RawData) -> Data {
    let resources = HashSet::from_iter(raw_data.resources.keys().cloned());
    let recipes = HashSet::from_iter(raw_data.recipes.keys().cloned());
    let mut products = HashSet::default();
    let mut ingredients = HashSet::default();

    for recipe in raw_data.recipes.values() {
        products.extend(
            recipe
                .products
                .iter()
                .map(|p| &p.item)
                .filter(|i| !raw_data.resources.contains_key(*i))
                .cloned(),
        );
        ingredients.extend(
            recipe
                .ingredients
                .iter()
                .map(|p| &p.item)
                .filter(|i| !raw_data.resources.contains_key(*i))
                .cloned(),
        );
    }

    let mut all_items = HashSet::new();
    all_items.extend(resources.iter().cloned());
    all_items.extend(ingredients.iter().cloned());
    all_items.extend(products.iter().cloned());

    Data {
        resources,
        recipes,
        products,
        ingredients,
        all_items,
    }
}

pub struct Variables {
    problem: ProblemVariables,
    n: HashMap<String, Variable>,
    x: HashMap<String, Variable>,
    i: HashMap<String, Variable>,
    r: HashMap<String, Variable>,
    power_use: Variable,
    item_use: Variable,
    building_use: Variable,
    resource_use: Variable,
    buildings_scaled: Variable,
    resources_scaled: Variable,
    pub sink_points: Variable,
    constraints: Vec<Constraint>,
}

pub struct Model {
    variables: Variables,
    problem: ProblemVariables,
    constraints: Vec<Constraint>,
}

impl Variables {
    pub fn define(settings: &Settings, data: &Data) -> Self {
        let mut problem = ProblemVariables::new();

        let n = problem.add_vector(variable().name("n").min(0.0), data.all_items.len());
        let x = problem.add_vector(variable().name("x").min(0.0), data.all_items.len());
        let i = problem.add_vector(variable().name("i").min(0.0), data.all_items.len());
        let r = problem.add_vector(
            if settings.integer_recipes {
                variable().name("r").min(0).integer()
            } else {
                variable().name("r").min(0)
            },
            data.recipes.len(),
        );
        let power_use = problem.add(variable().name("power_use").min(0.0));
        let item_use = problem.add(variable().name("item_use").min(0.0));
        let building_use = problem.add(variable().name("building_use").min(0.0));
        let resource_use = problem.add(variable().name("resource_use").min(0.0));
        let buildings_scaled = problem.add(variable().name("buildings_scaled").min(0.0));
        let resources_scaled = problem.add(variable().name("resources_scaled").min(0.0));
        let sink_points = problem.add(variable().name("sink_points").min(0.0));

        Variables {
            problem,
            n: data.all_items.iter().cloned().zip_eq(n).collect(),
            x: data.all_items.iter().cloned().zip_eq(x).collect(),
            i: data.all_items.iter().cloned().zip_eq(i).collect(),
            r: data.recipes.iter().cloned().zip_eq(r).collect(),
            power_use,
            item_use,
            building_use,
            resource_use,
            buildings_scaled,
            resources_scaled,
            sink_points,
            constraints: vec![],
        }
    }

    fn fix(&mut self, var: Variable, v: f64) {
        self.constraints.push(constraint::eq(var, v));
    }

    fn constrain(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    fn fix_input_amounts(&mut self, settings: &Settings, data: &Data) {
        for item in &data.all_items {
            if let Some(&input) = settings.inputs.get(item) {
                self.fix(self.n[item], input);
            } else {
                self.fix(self.n[item], 0.0);
            }
        }
    }

    fn fix_output_amount(&mut self, settings: &Settings) {
        if settings.outputs.is_empty() {
            return;
        }

        for (item, &amount) in &settings.outputs {
            if let Some(&x) = self.x.get(item) {
                self.fix(x, amount);
            } else {
                panic!("Output item '{item}' not found in model items.");
            }
        }
    }

    fn add_product_constraints(&mut self, data: &Data, recipes: &HashMap<String, Recipe>) {
        for item in &data.products {
            let expr = self.n[item]
                + Expression::sum(recipes.iter().flat_map(|(rk, rv)| {
                    rv.products
                        .iter()
                        .filter(|p| &p.item == item)
                        .map(|p| p.amount * 60.0 / rv.time * self.r[rk])
                }));
            if let Some(i) = self.i.get(item) {
                self.constrain(constraint::eq(expr, i))
            } else {
                panic!("Item '{item}' not found in model intermediate items.")
            }
        }
    }

    fn add_ingredient_constraints(&mut self, data: &Data, recipes: &HashMap<String, Recipe>) {
        for item in &data.ingredients {
            let expr = self.x[item]
                + Expression::sum(recipes.iter().flat_map(|(rk, rv)| {
                    rv.ingredients
                        .iter()
                        .filter(|p| &p.item == item)
                        .map(|p| p.amount * 60.0 / rv.time * self.r[rk])
                }));
            if let Some(i) = self.i.get(item) {
                self.constrain(constraint::eq(expr, i))
            } else {
                panic!("Item '{item}' not found in model intermediate items.")
            }
        }
    }

    fn add_resource_constraints(&mut self, settings: &Settings) {
        for (resource, &limit) in &settings.resource_limits {
            if let Some(&i) = self.i.get(resource) {
                self.constraints.push(constraint!(i <= limit));
            } else {
                panic!("Resource '{resource}' not found in model items.");
            }
        }
    }

    fn calculate_power_use(&mut self, data: &Data, recipes: &HashMap<String, Recipe>) {
        let expr = Expression::sum(
            data.recipes
                .iter()
                .map(|rk| recipes[rk].power_use * self.r[rk]),
        ) + Expression::sum(
            self.i
                .iter()
                .filter(|(item, _)| data.resources.contains(*item))
                .map(|(_, &item)| item * 0.168),
        );
        self.constrain(constraint!(expr == self.power_use));
    }

    fn calculate_item_use(&mut self, data: &Data) {
        static EXCLUDED: &[&str] = &[
            "Power_Produced",
            "Power_Produced_Other",
            "Power_Produced_Fuel",
            "Power_Produced_Nuclear",
        ];

        let expr = Expression::sum(
            data.all_items
                .iter()
                .filter(|item| EXCLUDED.iter().all(|e| e != item))
                .map(|item| self.i[item]),
        );
        self.constrain(constraint!(expr == self.item_use));
    }

    fn calculate_building_use(&mut self, data: &Data) {
        let expr = Expression::sum(data.recipes.iter().map(|r| self.r[r]));
        self.constrain(constraint!(expr == self.building_use));
    }

    fn calculate_resource_use(&mut self, settings: &Settings) {
        let expr = Expression::sum(settings.resource_limits.keys().map(|item| self.i[item]));
        self.constrain(constraint!(expr == self.resource_use));
    }

    fn calculate_buildings_scaled(&mut self, data: &Data, recipes: &HashMap<String, Recipe>) {
        let expr = Expression::sum(data.recipes.iter().map(|rk| {
            ((recipes[rk].ingredients.len() + recipes[rk].products.len() - 1) as f64).powf(1.584963)
                * self.r[rk]
                / 3
        }));
        self.constrain(constraint!(expr == self.buildings_scaled));
    }

    fn calculate_resources_scaled(&mut self, resource_weights: HashMap<String, f64>) {
        let expr = Expression::sum(
            resource_weights
                .iter()
                .filter_map(|(k, v)| self.i.get(k).map(|i| *v * *i)),
        );
        self.constrain(constraint!(expr == self.resources_scaled));
    }

    fn calculate_sink_points(&mut self, data: &Data, items: &HashMap<String, Arc<Item>>) {
        let expr = Expression::sum(data.all_items.iter().filter_map(|ik| {
            items
                .get(ik)
                .filter(|item| item.points > 0.0 && item.form == Some(Form::Solid))
                .map(|item| item.points)
                .map(|points| points * self.x[ik])
        }));
        self.constrain(constraint!(expr == self.sink_points));
    }

    fn disable_off_recipes(&mut self, settings: &Settings) {
        for recipe in &settings.recipes_off {
            self.fix(self.r[recipe], 0.0);
        }
    }

    fn set_objective(&mut self, settings: &Settings) -> good_lp::solvers::highs::HighsProblem {
        let mut waste_penalty_expr = self.x["Desc_NuclearWaste_C"]
            + self.x["Desc_NonFissibleUranium_C"]
            + self.x["Desc_PlutoniumPellet_C"]
            + self.x["Desc_PlutoniumCell_C"]
            + self.x["Desc_PlutoniumWaste_C"]
            + self.x["Desc_Ficsonium_C"];

        if settings.force_nuclear_waste {
            waste_penalty_expr = waste_penalty_expr + (self.x["Desc_PlutoniumFuelRod_C"] / 10);
        }

        let problem = match &settings.max_item {
            Some(serde_json::Value::String(a)) if a == "Points" => {
                self.fix(self.i["Desc_AlienProtein_C"], 0.0);
                self.fix(self.i["Desc_Gift_C"], 0.0);
                self.fix(self.i["Desc_Wood_C"], 0.0);
                self.fix(self.i["Desc_StingerParts_C"], 0.0);
                self.fix(self.i["Desc_SpitterParts_C"], 0.0);
                self.fix(self.i["Desc_HogParts_C"], 0.0);
                self.fix(self.i["Desc_HatcherParts_C"], 0.0);
                self.fix(self.i["Desc_Mycelia_C"], 0.0);
                self.fix(self.i["Desc_Leaves_C"], 0.0);

                self.problem.clone().minimise(
                    self.power_use * settings.weights.power_use
                        + waste_penalty_expr * settings.weights.nuclear_waste
                        - self.sink_points,
                )
            }
            Some(serde_json::Value::Bool(true)) => {
                todo!()
                // self.problem.clone().minimise(
                //     self.power_use * settings.weights.power_use
                //         + waste_penalty_expr * settings.weights.nuclear_waste
                //         - self.x[max_item] * 99999,
                // )
            }
            _ => self.problem.clone().minimise(
                self.power_use * settings.weights.power_use
                    + self.item_use * settings.weights.item_use
                    + self.building_use * settings.weights.building_use
                    + self.resource_use * settings.weights.resource_use
                    + self.buildings_scaled * settings.weights.buildings_scaled
                    + self.resources_scaled * settings.weights.resources_scaled
                    + waste_penalty_expr * settings.weights.nuclear_waste,
            ),
        };
        problem
            .using(default_solver)
            .with_all(self.constraints.clone())
    }
}

fn floor_resource_limits(settings: &mut Settings, floor: f64) {
    for (k, v) in &mut settings.resource_limits {
        if *v < floor {
            *v = floor;
        }
    }
}

pub fn create_model(raw: &RawData, settings: &mut Settings) -> (HighsProblem, Variables) {
    floor_resource_limits(settings, 1e-5);

    let data = extract_items(raw);
    let mut variables = Variables::define(settings, &data);
    variables.fix_input_amounts(settings, &data);
    variables.fix_output_amount(settings);
    variables.add_product_constraints(&data, &raw.recipes);
    variables.add_ingredient_constraints(&data, &raw.recipes);
    variables.add_resource_constraints(&settings);

    let filtered_limits = settings
        .resource_limits
        .iter()
        .filter(|(k, _)| *k != "Desc_Water_C")
        .map(|(k, v)| (k.clone(), *v))
        .collect::<HashMap<_, _>>();
    let avg_limit = filtered_limits.values().copied().sum::<f64>() / (filtered_limits.len() as f64);
    let resource_weights = data
        .resources
        .iter()
        .map(|resource| {
            (
                resource.clone(),
                avg_limit / settings.resource_limits[resource],
            )
        })
        .collect();

    variables.calculate_power_use(&data, &raw.recipes);
    variables.calculate_item_use(&data);
    variables.calculate_building_use(&data);
    variables.calculate_resource_use(&settings);
    variables.calculate_buildings_scaled(&data, &raw.recipes);
    variables.calculate_resources_scaled(resource_weights);
    variables.calculate_sink_points(&data, &raw.items);

    let model = variables.set_objective(settings);
    (model, variables)
}
