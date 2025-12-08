use crate::data::{Data, Form, ItemKey, MachineKey, RecipeKey, Settings};
use crate::rational::units::{Megawatts, Points, Recipes, Unitless};
use crate::rational::{ItemsPerMinute, Rat};
use eyre::bail;
use good_lp::{
    Constraint, Expression, ProblemVariables, Solution, SolutionStatus, SolverModel, Variable,
    WithTimeLimit, constraint, variable,
};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::iter::Sum;
use std::time::Duration;

#[cfg(feature = "scip")]
type PreparedProblem = good_lp::solvers::scip::SCIPProblem;
#[cfg(feature = "scip")]
static SOLVER_FN: fn(variable::UnsolvedProblem) -> PreparedProblem = good_lp::solvers::scip::scip;
#[cfg(all(feature = "highs", not(feature = "scip")))]
type PreparedProblem = good_lp::solvers::highs::HighsProblem;
#[cfg(all(feature = "highs", not(feature = "scip")))]
static SOLVER_FN: fn(variable::UnsolvedProblem) -> PreparedProblem = good_lp::solvers::highs::highs;
#[cfg(all(feature = "cbc", not(feature = "scip")))]
type PreparedProblem = good_lp::solvers::coin_cbc::CoinCbcProblem;
#[cfg(all(feature = "cbc", not(feature = "scip")))]
static SOLVER_FN: fn(variable::UnsolvedProblem) -> PreparedProblem =
    good_lp::solvers::coin_cbc::coin_cbc;

struct Keys {
    resources: HashSet<ItemKey>,
    recipes: HashSet<RecipeKey>,
    products: HashSet<ItemKey>,
    ingredients: HashSet<ItemKey>,
    all_items: HashSet<ItemKey>,
}

fn extract_items(raw_data: &Data) -> Keys {
    let resources: HashSet<_> = raw_data.resources.keys().copied().collect();
    let recipes = raw_data.recipes.keys().copied().collect();
    let mut products = HashSet::default();
    let mut ingredients = HashSet::default();

    for recipe in raw_data.recipes.values() {
        products.extend(
            recipe
                .products
                .iter()
                .map(|p| &p.item)
                .filter(|i| !raw_data.resources.contains_key(*i))
                .copied(),
        );
        ingredients.extend(
            recipe
                .ingredients
                .iter()
                .map(|p| &p.item)
                .filter(|i| !raw_data.resources.contains_key(*i))
                .copied(),
        );
    }

    let mut all_items = HashSet::new();
    all_items.extend(resources.iter().copied());
    all_items.extend(ingredients.iter().copied());
    all_items.extend(products.iter().copied());

    Keys {
        resources,
        recipes,
        products,
        ingredients,
        all_items,
    }
}

pub struct Variables {
    pub inputs: HashMap<ItemKey, Variable>,
    pub outputs: HashMap<ItemKey, Variable>,
    pub items: HashMap<ItemKey, Variable>,
    pub recipes: HashMap<RecipeKey, Variable>,
    pub power_use: Variable,
    pub item_use: Variable,
    pub building_use: Variable,
    pub resource_use: Variable,
    pub buildings_scaled: Variable,
    pub resources_scaled: Variable,
    pub sink_points: Variable,
}

pub struct Model {
    problem: ProblemVariables,
    constraints: Vec<Constraint>,
    pub inputs: HashMap<ItemKey, Variable>,
    pub outputs: HashMap<ItemKey, Variable>,
    pub items: HashMap<ItemKey, Variable>,
    pub recipes: HashMap<RecipeKey, Variable>,
    pub power_use: Variable,
    pub item_use: Variable,
    pub building_use: Variable,
    pub resource_use: Variable,
    pub buildings_scaled: Variable,
    pub resources_scaled: Variable,
    pub sink_points: Variable,
}

impl Model {
    pub fn define(
        settings: &Settings,
        data: &Data,
        all_items: &HashSet<ItemKey>,
        recipe_keys: &HashSet<RecipeKey>,
    ) -> Self {
        let mut problem = ProblemVariables::new();

        let inputs = problem.add_vector(variable().name("inputs").min(0), all_items.len());
        let outputs = problem.add_vector(variable().name("outputs").min(0), all_items.len());
        let items = problem.add_vector(variable().name("items").min(0), all_items.len());

        let (solid, fluid): (Vec<_>, Vec<_>) = data.recipes.iter().partition_map(|(k, r)| {
            if r.products
                .iter()
                .filter(|i| data.items.contains_key(&i.item))
                .any(|i| {
                    data.items
                        .get(&i.item)
                        .unwrap_or_else(|| panic!("Item not found in data: {:?}", i.item))
                        .form
                        == Some(Form::Solid)
                })
            {
                itertools::Either::Left(*k)
            } else {
                itertools::Either::Right(*k)
            }
        });

        let (variable_recipes, integer): (Vec<_>, Vec<_>) = solid
            .into_iter()
            .partition(|key| settings.floating_recipes.contains(key));

        let solid_recipes = problem.add_vector(
            if settings.integer_recipes {
                variable().name("recipes").min(0).integer()
            } else {
                variable().name("recipes").min(0)
            },
            integer.len(),
        );
        let fluid_recipes =
            problem.add_vector(variable().name("fluid_recipes").min(0), fluid.iter().len());
        let variable_recipe_vars = problem.add_vector(
            variable().name("variable_recipes").min(0),
            variable_recipes.len(),
        );
        let power_use = problem.add(variable().name("power_use").min(0));
        let item_use = problem.add(variable().name("item_use").min(0));
        let building_use = problem.add(variable().name("building_use").min(0));
        let resource_use = problem.add(variable().name("resource_use").min(0));
        let buildings_scaled = problem.add(variable().name("buildings_scaled").min(0));
        let resources_scaled = problem.add(variable().name("resources_scaled").min(0));
        let sink_points = problem.add(variable().name("sink_points").min(0));

        Model {
            problem,
            inputs: all_items.iter().copied().zip_eq(inputs).collect(),
            outputs: all_items.iter().copied().zip_eq(outputs).collect(),
            items: all_items.iter().copied().zip_eq(items).collect(),
            recipes: integer
                .iter()
                .copied()
                .zip_eq(solid_recipes)
                .chain(fluid.iter().copied().zip_eq(fluid_recipes))
                .chain(
                    variable_recipes
                        .iter()
                        .copied()
                        .zip_eq(variable_recipe_vars),
                )
                .collect(),
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

    fn fix_zero(&mut self, var: Variable) {
        self.constraints.push(constraint!(var == 0));
    }

    fn constrain(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    fn fix_input_amounts(&mut self, settings: &Settings, all_items: &HashSet<ItemKey>) {
        for item in all_items {
            let input = settings.inputs.get(item).copied().unwrap_or(Rat::ZERO);
            self.constrain(constraint!(self.inputs[item] == input.as_f64()));
        }
    }

    fn fix_output_amount(&mut self, settings: &Settings) {
        if settings.outputs.is_empty() {
            return;
        }

        for (item, &amount) in &settings.outputs {
            if let Some(&x) = self.outputs.get(item) {
                self.constrain(constraint!(x == amount.as_f64()));
            } else {
                panic!(
                    "Output item '{item}' not found in model items: {:?}.",
                    self.outputs.keys()
                );
            }
        }
    }

    fn fix_extras_amount(&mut self, settings: &Settings) {
        if settings.extras.is_empty() {
            return;
        }

        for (item, &amount) in &settings.extras {
            if let Some(&i) = self.items.get(item) {
                self.constrain(constraint!(i >= amount.as_f64()));
            } else {
                panic!(
                    "Extras item '{item}' not found in model items: {:?}.",
                    self.outputs.keys()
                );
            }
        }
    }

    fn add_product_constraints(&mut self, products: &HashSet<ItemKey>, data: &Data) {
        for item in products {
            let expr = self.inputs[item]
                + Expression::sum(data.recipes.iter().flat_map(|(rk, rv)| {
                    rv.products
                        .iter()
                        .filter(|p| &p.item == item)
                        .map(|p| p.amount.as_f64() * self.recipes[rk])
                }));
            if let Some(i) = self.items.get(item) {
                self.constrain(constraint::eq(expr, i));
            } else {
                panic!("Item '{item}' not found in model intermediate items.")
            }
        }
    }

    fn add_ingredient_constraints(&mut self, ingredients: &HashSet<ItemKey>, data: &Data) {
        for item in ingredients {
            let expr = self.outputs[item]
                + Expression::sum(data.recipes.iter().flat_map(|(recipe_key, recipe_data)| {
                    recipe_data
                        .ingredients
                        .iter()
                        .filter(|p| &p.item == item)
                        .map(|p| p.amount.as_f64() * self.recipes[recipe_key])
                }));
            if let Some(i) = self.items.get(item) {
                self.constrain(constraint!(expr == i));
            } else {
                panic!("Item '{item}' not found in model intermediate items.");
            }
        }
    }

    fn add_resource_constraints(&mut self, settings: &Settings) {
        for (resource, &limit) in &settings.resource_limits {
            if let Some(&i) = self.items.get(resource) {
                self.constraints.push(constraint!(i <= limit.as_f64()));
            } else {
                panic!("Resource '{resource}' not found in model items.");
            }
        }
    }

    fn calculate_power_use(&mut self, data: &Data, recipes: &HashSet<RecipeKey>) {
        let expr = Expression::sum(
            recipes
                .iter()
                .map(|rk| data.recipes[rk].power_use.as_f64() * self.recipes[rk]),
        ) + Expression::sum(
            self.items
                .iter()
                .filter(|(item, _)| data.resources.contains_key(*item))
                .map(|(_, &item)| item * 0.168),
        );
        self.constrain(constraint!(expr == self.power_use));
    }

    fn calculate_item_use(&mut self, items: &HashSet<ItemKey>) {
        let excluded = [
            ItemKey::new_static("Power_Produced"),
            ItemKey::new_static("Power_Produced_Other"),
            ItemKey::new_static("Power_Produced_Fuel"),
            ItemKey::new_static("Power_Produced_Nuclear"),
        ];

        let expr = Expression::sum(
            items
                .iter()
                .filter(|item| excluded.iter().all(|e| e != *item))
                .map(|item| self.items[item]),
        );
        self.constrain(constraint!(expr == self.item_use));
    }

    fn calculate_building_use(&mut self, recipes: &HashSet<RecipeKey>) {
        let expr = Expression::sum(recipes.iter().map(|r| self.recipes[r]));
        self.constrain(constraint!(expr == self.building_use));
    }

    fn calculate_resource_use(&mut self, settings: &Settings) {
        let expr = Expression::sum(settings.resource_limits.keys().map(|item| self.items[item]));
        self.constrain(constraint!(expr == self.resource_use));
    }

    fn calculate_buildings_scaled(&mut self, data: &Data, recipes: &HashSet<RecipeKey>) {
        let expr = Expression::sum(recipes.iter().map(|rk| {
            let recipe = &data.recipes[rk];
            let combined_len = recipe.ingredients.len() + recipe.products.len();
            (combined_len.checked_sub(1).unwrap_or_else(|| {
                panic!("Recipe has no ingredients or products: {rk} {combined_len}")
            }) as f64)
                .powf(1.584_963)
                * self.recipes[rk]
                / 3
        }));
        self.constrain(constraint!(expr == self.buildings_scaled));
    }

    fn calculate_resources_scaled(&mut self, resource_weights: HashMap<ItemKey, Rat<Unitless>>) {
        let expr = Expression::sum(
            resource_weights
                .iter()
                .filter_map(|(k, v)| self.items.get(k).map(|i| v.as_f64() * *i)),
        );
        self.constrain(constraint!(expr == self.resources_scaled));
    }

    fn calculate_sink_points(&mut self, data: &Data, items: &HashSet<ItemKey>) {
        let expr = Expression::sum(items.iter().filter_map(|ik| {
            data.items
                .get(ik)
                .filter(|item| item.points > Rat::ZERO && item.form == Some(Form::Solid))
                .map(|item| item.points)
                .map(|points| points.as_f64() * self.outputs[ik])
        }));
        self.constrain(constraint!(expr == self.sink_points));
    }

    fn disable_off_recipes(&mut self, settings: &Settings) {
        for recipe in &settings.recipes_off {
            self.fix_zero(self.recipes[recipe]);
        }
    }

    pub fn disable_locked_recipes(&mut self, settings: &Settings, data: &Data) {
        let Some(phase) = settings.phase else { return };
        let mut disabled: HashSet<MachineKey> = HashSet::new();
        disabled.insert("Build_Packager_C".into());
        if phase < 5 {
            disabled.insert("Build_Converter_C".into());
            disabled.insert("Build_QuantumEncoder_C".into());
        }
        if phase < 4 {
            disabled.insert("Build_GeneratorNuclear_C".into());
            disabled.insert("Build_HadronCollider_C".into());
            disabled.insert("Build_Blender_C".into());
        }
        if phase < 3 {
            disabled.insert("Build_OilRefinery_C".into());
            disabled.insert("Build_Packager_C".into());
            disabled.insert("Build_GeneratorFuel_C".into());
            disabled.insert("Build_ManufacturerMk1_C".into());
        }
        if phase < 2 {
            disabled.insert("Build_FoundryMk1_C".into());
            disabled.insert("Build_GeneratorCoal_C".into());
        }
        if phase < 1 {
            disabled.insert("Build_AssemblerMk1_C".into());
        }

        for (k, _) in data
            .recipes
            .iter()
            .filter(|(_, r)| disabled.contains(&r.machine))
        {
            // println!("Disabling {:?}", r.name);
            self.fix_zero(self.recipes[k]);
        }
    }

    fn set_objective(self, settings: &Settings) -> PreparedModel {
        let mut waste_penalty_expr = self.outputs[&"Desc_NuclearWaste_C".into()]
            + self.outputs[&"Desc_NonFissibleUranium_C".into()]
            + self.outputs[&"Desc_PlutoniumPellet_C".into()]
            + self.outputs[&"Desc_PlutoniumCell_C".into()]
            + self.outputs[&"Desc_PlutoniumWaste_C".into()]
            + self.outputs[&"Desc_Ficsonium_C".into()];

        if settings.force_nuclear_waste {
            waste_penalty_expr += self.outputs[&"Desc_PlutoniumFuelRod_C".into()] / 10;
        }

        let problem = match &settings.max_item {
            Some(serde_json::Value::String(a)) if a == "Points" => {
                todo!()
                // self.fix_zero(self.i["Desc_AlienProtein_C"]);
                // self.fix_zero(self.i["Desc_Gift_C"]);
                // self.fix_zero(self.i["Desc_Wood_C"]);
                // self.fix_zero(self.i["Desc_StingerParts_C"]);
                // self.fix_zero(self.i["Desc_SpitterParts_C"]);
                // self.fix_zero(self.i["Desc_HogParts_C"]);
                // self.fix_zero(self.i["Desc_HatcherParts_C"]);
                // self.fix_zero(self.i["Desc_Mycelia_C"]);
                // self.fix_zero(self.i["Desc_Leaves_C"]);
                //
                // self.problem.minimise(
                //     self.power_use * settings.weights.power_use
                //         + waste_penalty_expr * settings.weights.nuclear_waste
                //         - self.sink_points,
                // )
            }
            Some(serde_json::Value::Bool(true)) => {
                todo!()
                // self.problem.clone().minimise(
                //     self.power_use * settings.weights.power_use
                //         + waste_penalty_expr * settings.weights.nuclear_waste
                //         - self.x[max_item] * 99999,
                // )
            }
            _ => self.problem.minimise(
                self.power_use * settings.weights.power_use
                    + self.item_use * settings.weights.item_use
                    + self.building_use * settings.weights.building_use
                    + self.resource_use * settings.weights.resource_use
                    + self.buildings_scaled * settings.weights.buildings_scaled
                    + self.resources_scaled * settings.weights.resources_scaled
                    + waste_penalty_expr * settings.weights.nuclear_waste,
            ),
        };
        let problem = problem
            .using(|c| SOLVER_FN(c))
            // .with_time_limit(Duration::from_secs(300).as_secs_f64())
            .with_all(self.constraints.clone());
        let variables = Variables {
            inputs: self.inputs,
            outputs: self.outputs,
            items: self.items,
            recipes: self.recipes,
            power_use: self.power_use,
            item_use: self.item_use,
            building_use: self.building_use,
            resource_use: self.resource_use,
            buildings_scaled: self.buildings_scaled,
            resources_scaled: self.resources_scaled,
            sink_points: self.sink_points,
        };

        PreparedModel {
            problem,
            vars: variables,
        }
    }
}

pub struct PreparedModel {
    problem: PreparedProblem,
    vars: Variables,
}

impl PreparedModel {
    pub fn new(data: &Data, settings: &Settings) -> Self {
        let keys = extract_items(data);
        let mut model = Model::define(settings, data, &keys.all_items, &keys.recipes);
        model.fix_input_amounts(settings, &keys.all_items);
        model.fix_output_amount(settings);
        model.fix_extras_amount(settings);
        model.add_product_constraints(&keys.products, data);
        model.add_ingredient_constraints(&keys.all_items, data);
        model.add_resource_constraints(settings);

        let filtered_limits = settings
            .resource_limits
            .iter()
            .filter(|(k, _)| *k != &"Desc_Water_C".into())
            .map(|(k, v)| (*k, *v))
            .collect::<HashMap<_, _>>();
        let avg_limit = filtered_limits.values().copied().sum::<ItemsPerMinute>()
            / (Rat::<Unitless>::whole(filtered_limits.len().try_into().unwrap()));
        let resource_weights = keys
            .resources
            .iter()
            .map(|resource| (*resource, avg_limit / settings.resource_limits[resource]))
            .collect();

        model.calculate_power_use(data, &keys.recipes);
        model.calculate_item_use(&keys.all_items);
        model.calculate_building_use(&keys.recipes);
        model.calculate_resource_use(settings);
        model.calculate_buildings_scaled(data, &keys.recipes);
        model.calculate_resources_scaled(resource_weights);
        model.calculate_sink_points(data, &keys.products);

        model.disable_off_recipes(settings);
        model.disable_locked_recipes(settings, data);

        model.set_objective(settings)
    }

    pub fn solve(self) -> eyre::Result<SolvedProblem> {
        let result = self.problem.solve()?;
        match result.status() {
            SolutionStatus::Optimal => {
                println!("Found optimial solution");
            }
            SolutionStatus::TimeLimit => {
                println!("Solution hit time limit");
            }
            SolutionStatus::GapLimit => {
                println!("Solution hit gap limit");
            }
        }
        Ok(SolvedProblem {
            solution: result,
            vars: self.vars,
        })
    }
}

pub struct SolvedProblem {
    solution: <PreparedProblem as SolverModel>::Solution,
    vars: Variables,
}

impl SolvedProblem {
    pub fn into_values(self, settings: &Settings, data: &Data) -> SolutionValues {
        const EPSILON: f64 = 0.001;
        let power_sources = [
            ItemKey::new_static("Power_Produced_Other"),
            ItemKey::new_static("Power_Produced_Fuel"),
            ItemKey::new_static("Power_Produced_Nuclear"),
        ];

        let resources_needed = data
            .resources
            .iter()
            .filter_map(|(k, _)| self.vars.items.get(k).map(|v| (k, self.solution.value(*v))))
            .filter(|(_, v)| *v > EPSILON)
            .filter(|(k, _)| settings.resource_limits.contains_key(*k))
            .map(|(k, v)| (*k, (data.resources[k].name.clone(), v.into())))
            .collect();

        let items_needed = data
            .items
            .iter()
            .filter_map(|(k, _)| self.vars.items.get(k).map(|v| (k, self.solution.value(*v))))
            .filter(|(_, v)| *v > EPSILON)
            .filter(|(k, _)| !settings.resource_limits.contains_key(*k))
            .map(|(k, v)| (*k, (data.items[k].name.clone(), v.into())))
            .collect();

        let mut products_map = HashMap::new();
        for (item, var) in &self.vars.items {
            let item_val = self.solution.value(*var);
            if item_val <= EPSILON {
                continue;
            }

            let key = data
                .items
                .get(item)
                .map_or_else(|| data.resources[item].name.clone(), |i| i.name.clone());
            let products: &mut HashMap<_, _> = products_map.entry(key).or_default();
            for (recipe, var) in &self.vars.recipes {
                let recipe_val = self.solution.value(*var);
                if recipe_val <= EPSILON {
                    continue;
                }
                let recipe_val = Rat::<Recipes>::from(recipe_val);
                for ingredient in &data.recipes[recipe].ingredients {
                    if item != &ingredient.item {
                        continue;
                    }

                    let recipe_data = &data.recipes[recipe];
                    products.insert(recipe_data.name.clone(), ingredient.amount * recipe_val);
                }
            }
        }

        let mut ingredients_map = HashMap::new();
        for (recipe, var) in &self.vars.recipes {
            let recipe_val = self.solution.value(*var);
            if recipe_val <= EPSILON {
                continue;
            }
            let recipe_val = Rat::<Recipes>::from(recipe_val);

            let ingredients: &mut HashMap<_, _> = ingredients_map
                .entry(data.recipes[recipe].name.clone())
                .or_default();
            for ingredient in &data.recipes[recipe].ingredients {
                let name = data.items.get(&ingredient.item).map_or_else(
                    || {
                        data.resources
                            .get(&ingredient.item)
                            .map(|i| i.name.clone())
                            .expect("must exist")
                    },
                    |i| i.name.clone(),
                );
                ingredients.insert(name, ingredient.amount * recipe_val);
            }
        }

        SolutionValues {
            sink_points: self.solution.value(self.vars.sink_points).into(),
            items_input: self
                .vars
                .inputs
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (*k, (data.items[k].name.clone(), v.into())))
                .collect(),
            items_output: self
                .vars
                .outputs
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (*k, (data.items[k].name.clone(), v.into())))
                .collect(),
            resources_needed,
            items_needed,
            recipes_used: self
                .vars
                .recipes
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (*k, (data.recipes[k].name.clone(), v.into())))
                .collect(),
            power_produced: self
                .vars
                .outputs
                .iter()
                .filter(|(k, _)| power_sources.contains(*k))
                .map(|(k, v)| (*k, self.solution.value(*v).into()))
                .collect(),
            power_use: self.solution.value(self.vars.power_use),
            item_use: self.solution.value(self.vars.item_use),
            buildings: self.solution.value(self.vars.building_use),
            resources: self.solution.value(self.vars.resource_use),
            buildings_scaled: self.solution.value(self.vars.buildings_scaled),
            resources_scaled: self.solution.value(self.vars.resources_scaled),
            products_map,
            ingredients_map,
        }
    }
}

#[derive(Debug)]
pub struct SolutionValues {
    pub sink_points: Rat<Points>,
    pub items_input: HashMap<ItemKey, (String, ItemsPerMinute)>,
    pub items_output: HashMap<ItemKey, (String, ItemsPerMinute)>,
    pub resources_needed: HashMap<ItemKey, (String, ItemsPerMinute)>,
    pub items_needed: HashMap<ItemKey, (String, ItemsPerMinute)>,
    pub recipes_used: HashMap<RecipeKey, (String, Rat<Recipes>)>,
    pub power_produced: HashMap<ItemKey, Rat<Megawatts>>,

    pub power_use: f64,
    pub item_use: f64,
    pub buildings: f64,
    pub resources: f64,
    pub buildings_scaled: f64,
    pub resources_scaled: f64,

    pub products_map: HashMap<String, HashMap<String, ItemsPerMinute>>,
    pub ingredients_map: HashMap<String, HashMap<String, ItemsPerMinute>>,
}
