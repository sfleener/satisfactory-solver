use crate::data::{Data, Form, Key, Settings};
use eyre::bail;
use good_lp::solvers::highs::HighsProblem;
use good_lp::{
    Constraint, Expression, ProblemVariables, Solution, SolutionStatus, SolverModel, Variable,
    constraint, default_solver, variable,
};
use itertools::Itertools;
use num::{Float, FromPrimitive, Integer, Signed};
use petgraph::data::Build;
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::iter::Sum;
use std::num::NonZeroI64;
use std::ops::{Add, Neg};

type PreparedProblem = HighsProblem;

struct Keys {
    resources: HashSet<Key>,
    recipes: HashSet<Key>,
    products: HashSet<Key>,
    ingredients: HashSet<Key>,
    all_items: HashSet<Key>,
}

fn extract_items(raw_data: &Data) -> Keys {
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

    Keys {
        resources,
        recipes,
        products,
        ingredients,
        all_items,
    }
}

pub struct Variables {
    pub n: HashMap<Key, Variable>,
    pub x: HashMap<Key, Variable>,
    pub i: HashMap<Key, Variable>,
    pub r: HashMap<Key, Variable>,
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
    pub n: HashMap<Key, Variable>,
    pub x: HashMap<Key, Variable>,
    pub i: HashMap<Key, Variable>,
    pub r: HashMap<Key, Variable>,
    pub power_use: Variable,
    pub item_use: Variable,
    pub building_use: Variable,
    pub resource_use: Variable,
    pub buildings_scaled: Variable,
    pub resources_scaled: Variable,
    pub sink_points: Variable,
}

impl Model {
    pub fn define(settings: &Settings, all_items: &HashSet<Key>, recipes: &HashSet<Key>) -> Self {
        let mut problem = ProblemVariables::new();

        let n = problem.add_vector(variable().name("n").min(0), all_items.len());
        let x = problem.add_vector(variable().name("x").min(0), all_items.len());
        let i = problem.add_vector(variable().name("i").min(0), all_items.len());
        let r = problem.add_vector(
            if settings.integer_recipes {
                variable().name("r").min(0).integer()
            } else {
                variable().name("r").min(0)
            },
            recipes.len(),
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
            n: all_items.iter().cloned().zip_eq(n).collect(),
            x: all_items.iter().cloned().zip_eq(x).collect(),
            i: all_items.iter().cloned().zip_eq(i).collect(),
            r: recipes.iter().cloned().zip_eq(r).collect(),
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

    fn fix_input_amounts(&mut self, settings: &Settings, all_items: &HashSet<Key>) {
        for item in all_items {
            let input = settings.inputs.get(item).copied().unwrap_or(0.0);
            self.constrain(constraint!(self.n[item] == input));
        }
    }

    fn fix_output_amount(&mut self, settings: &Settings) {
        if settings.outputs.is_empty() {
            return;
        }

        for (item, &amount) in &settings.outputs {
            if let Some(&x) = self.x.get(item) {
                self.constrain(constraint!(x >= amount));
            } else {
                panic!(
                    "Output item '{item}' not found in model items: {:?}.",
                    self.x.keys()
                );
            }
        }
    }

    fn add_product_constraints(&mut self, products: &HashSet<Key>, data: &Data) {
        for item in products {
            let expr = self.n[item]
                + Expression::sum(data.recipes.iter().flat_map(|(rk, rv)| {
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

    fn add_ingredient_constraints(&mut self, ingredients: &HashSet<Key>, data: &Data) {
        for item in ingredients {
            let expr = self.x[item]
                + Expression::sum(data.recipes.iter().flat_map(|(recipe_key, recipe_data)| {
                    recipe_data
                        .ingredients
                        .iter()
                        .filter(|p| &p.item == item)
                        .map(|p| p.amount * 60.0 / recipe_data.time * self.r[recipe_key])
                }));
            if let Some(i) = self.i.get(item) {
                self.constrain(constraint!(expr == i))
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

    fn calculate_power_use(&mut self, data: &Data, recipes: &HashSet<Key>) {
        let expr = Expression::sum(
            recipes
                .iter()
                .map(|rk| data.recipes[rk].power_use * self.r[rk]),
        ) + Expression::sum(
            self.i
                .iter()
                .filter(|(item, _)| data.resources.contains_key(*item))
                .map(|(_, &item)| item * 0.168),
        );
        self.constrain(constraint!(expr == self.power_use));
    }

    fn calculate_item_use(&mut self, items: &HashSet<Key>) {
        let excluded = [
            Key::new_static("Power_Produced"),
            Key::new_static("Power_Produced_Other"),
            Key::new_static("Power_Produced_Fuel"),
            Key::new_static("Power_Produced_Nuclear"),
        ];

        let expr = Expression::sum(
            items
                .iter()
                .filter(|item| excluded.iter().all(|e| e != *item))
                .map(|item| self.i[item]),
        );
        self.constrain(constraint!(expr == self.item_use));
    }

    fn calculate_building_use(&mut self, recipes: &HashSet<Key>) {
        let expr = Expression::sum(recipes.iter().map(|r| self.r[r]));
        self.constrain(constraint!(expr == self.building_use));
    }

    fn calculate_resource_use(&mut self, settings: &Settings) {
        let expr = Expression::sum(settings.resource_limits.keys().map(|item| self.i[item]));
        self.constrain(constraint!(expr == self.resource_use));
    }

    fn calculate_buildings_scaled(&mut self, data: &Data, recipes: &HashSet<Key>) {
        let expr = Expression::sum(recipes.iter().map(|rk| {
            ((data.recipes[rk].ingredients.len() + data.recipes[rk].products.len() - 1) as f64)
                .powf(1.584963)
                * self.r[rk]
                / 3
        }));
        self.constrain(constraint!(expr == self.buildings_scaled));
    }

    fn calculate_resources_scaled(&mut self, resource_weights: HashMap<Key, f64>) {
        let expr = Expression::sum(
            resource_weights
                .iter()
                .filter_map(|(k, v)| self.i.get(k).map(|i| *v * *i)),
        );
        self.constrain(constraint!(expr == self.resources_scaled));
    }

    fn calculate_sink_points(&mut self, data: &Data, items: &HashSet<Key>) {
        let expr = Expression::sum(items.iter().filter_map(|ik| {
            data.items
                .get(ik)
                .filter(|item| item.points > 0.0 && item.form == Some(Form::Solid))
                .map(|item| item.points)
                .map(|points| points * self.x[ik])
        }));
        self.constrain(constraint!(expr == self.sink_points));
    }

    fn disable_off_recipes(&mut self, settings: &Settings) {
        for recipe in &settings.recipes_off {
            self.fix_zero(self.r[recipe]);
        }
    }

    pub fn disable_locked_recipes(&mut self, settings: &Settings, data: &Data) {
        let Some(phase) = settings.phase else { return };
        let mut disabled = HashSet::new();
        if phase < 5 {
            disabled.insert("Build_Converter_C");
            disabled.insert("Build_QuantumEncoder_C");
        }
        if phase < 4 {
            disabled.insert("Build_GeneratorNuclear_C");
            disabled.insert("Build_HadronCollider_C");
        }
        if phase < 3 {
            disabled.insert("Build_OilRefinery_C");
            disabled.insert("Build_Packager_C");
            disabled.insert("Build_GeneratorFuel_C");
            disabled.insert("Build_ManufacturerMk1_C");
        }
        if phase < 2 {
            disabled.insert("Build_FoundryMk1_C");
            disabled.insert("Build_GeneratorCoal_C");
        }
        if phase < 1 {
            disabled.insert("Build_AssemblerMk1_C");
        }

        for (k, r) in data
            .recipes
            .iter()
            .filter(|(_, r)| disabled.contains(r.machine.as_str()))
        {
            // println!("Disabling {:?}", r.name);
            self.fix_zero(self.r[k]);
        }
    }

    fn set_objective(mut self, settings: &Settings) -> PreparedModel {
        let mut waste_penalty_expr = self.x[&"Desc_NuclearWaste_C".into()]
            + self.x[&"Desc_NonFissibleUranium_C".into()]
            + self.x[&"Desc_PlutoniumPellet_C".into()]
            + self.x[&"Desc_PlutoniumCell_C".into()]
            + self.x[&"Desc_PlutoniumWaste_C".into()]
            + self.x[&"Desc_Ficsonium_C".into()];

        if settings.force_nuclear_waste {
            waste_penalty_expr =
                waste_penalty_expr + (self.x[&"Desc_PlutoniumFuelRod_C".into()] / 10);
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
            .using(default_solver)
            .with_all(self.constraints.clone());
        let variables = Variables {
            n: self.n,
            x: self.x,
            i: self.i,
            r: self.r,
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
        let mut model = Model::define(settings, &keys.all_items, &keys.recipes);
        model.fix_input_amounts(settings, &keys.all_items);
        model.fix_output_amount(settings);
        model.add_product_constraints(&keys.products, data);
        model.add_ingredient_constraints(&keys.all_items, data);
        model.add_resource_constraints(settings);

        let filtered_limits = settings
            .resource_limits
            .iter()
            .filter(|(k, _)| *k != &"Desc_Water_C".into())
            .map(|(k, v)| (k.clone(), *v))
            .collect::<HashMap<_, _>>();
        let avg_limit =
            filtered_limits.values().copied().sum::<f64>() / (filtered_limits.len() as f64);
        let resource_weights = keys
            .resources
            .iter()
            .map(|resource| {
                (
                    resource.clone(),
                    avg_limit / settings.resource_limits[resource],
                )
            })
            .collect();

        model.calculate_power_use(data, &keys.recipes);
        model.calculate_item_use(&keys.all_items);
        model.calculate_building_use(&keys.recipes);
        model.calculate_resource_use(settings);
        model.calculate_buildings_scaled(data, &keys.recipes);
        model.calculate_resources_scaled(resource_weights);
        model.calculate_sink_points(data, &keys.products);

        model.disable_off_recipes(&settings);
        model.disable_locked_recipes(settings, data);

        model.set_objective(settings)
    }

    pub fn solve(self) -> eyre::Result<SolvedProblem> {
        let result = self.problem.solve()?;
        match result.status() {
            SolutionStatus::Optimal => {}
            SolutionStatus::TimeLimit => {
                bail!("Solution hit time limit");
            }
            SolutionStatus::GapLimit => {
                bail!("Solution hit gap limit");
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
            Key::new_static("Power_Produced_Other"),
            Key::new_static("Power_Produced_Fuel"),
            Key::new_static("Power_Produced_Nuclear"),
        ];

        let resources_needed = data
            .resources
            .iter()
            .filter_map(|(k, _)| self.vars.i.get(k).map(|v| (k, self.solution.value(*v))))
            .filter(|(_, v)| *v > EPSILON)
            .filter(|(k, _)| settings.resource_limits.contains_key(*k))
            .map(|(k, v)| (data.resources[k].name.clone(), v.into()))
            .collect();

        let items_needed = data
            .items
            .iter()
            .filter_map(|(k, _)| self.vars.i.get(k).map(|v| (k, self.solution.value(*v))))
            .filter(|(_, v)| *v > EPSILON)
            .filter(|(k, _)| !settings.resource_limits.contains_key(*k))
            .map(|(k, v)| (data.items[k].name.clone(), v.into()))
            .collect();

        let mut graph = DiGraph::<(f64, String), f64>::new();
        let output = graph.add_node((0.0, "output".to_string()));
        let mut recipe_nodes = HashMap::new();

        for (recipe_key, var) in &self.vars.r {
            let recipe_val = self.solution.value(*var);
            if recipe_val <= EPSILON {
                continue;
            }

            let recipe = &data.recipes[recipe_key];
            let node = graph.add_node((recipe_val, recipe.name.clone()));

            // if let Some(item) = recipe.products.iter().find_map(|p| {
            //     for o in settings.outputs.keys() {
            //         if &p.item == o {
            //             return Some(p);
            //         }
            //     }
            //     None
            // }) {
            //     let amount = (60.0 / recipe.time) * item.amount * recipe_val;
            //
            //     graph.add_edge(node, output, (data.items[&item.item].name.clone(), amount));
            // }

            recipe_nodes.insert(recipe_key.clone(), (node, recipe.clone(), recipe_val));
        }

        let mut needs = BTreeMap::new();

        for (k, v) in &settings.outputs {
            needs.insert(k.clone(), (output, *v));
        }

        while let Some((needs_key, (needs_node, needs_amount))) = needs.pop_first() {
            for (k, v) in &self.vars.r {
                let recipe = &data.recipes[&k];
                let Some(recipe_item) = recipe.products.iter().find(|r| r.item == needs_key) else {
                    continue;
                };
            }
        }

        // println!("{:?}", Dot::new(&graph));

        let mut products_map = HashMap::new();
        for (item, var) in &self.vars.i {
            let item_val = self.solution.value(*var);
            if item_val <= EPSILON {
                continue;
            }

            let key = data
                .items
                .get(item)
                .map(|i| i.name.clone())
                .unwrap_or_else(|| data.resources[item].name.clone());
            let products: &mut HashMap<_, _> = products_map.entry(key).or_default();
            for (recipe, var) in &self.vars.r {
                let recipe_val = self.solution.value(*var);
                if recipe_val <= EPSILON {
                    continue;
                }
                for ingredient in &data.recipes[recipe].ingredients {
                    if item != &ingredient.item {
                        continue;
                    }

                    let recipe_data = &data.recipes[recipe];
                    products.insert(
                        recipe_data.name.clone(),
                        ((60.0 / recipe_data.time) * ingredient.amount * recipe_val).into(),
                    );
                }
            }
        }

        let mut ingredients_map = HashMap::new();
        for (recipe, var) in &self.vars.r {
            let recipe_val = self.solution.value(*var);
            if recipe_val <= EPSILON {
                continue;
            }

            let ingredients: &mut HashMap<_, _> = ingredients_map
                .entry(data.recipes[recipe].name.clone())
                .or_default();
            for ingredient in &data.recipes[recipe].ingredients {
                let name = data
                    .items
                    .get(&ingredient.item)
                    .map(|i| i.name.clone())
                    .unwrap_or_else(|| {
                        data.resources
                            .get(&ingredient.item)
                            .map(|i| i.name.clone())
                            .expect("must exist")
                    });
                ingredients.insert(
                    name,
                    ((60.0 / data.recipes[recipe].time) * ingredient.amount * recipe_val).into(),
                );
            }
        }

        SolutionValues {
            sink_points: self.solution.value(self.vars.sink_points).into(),
            items_input: self
                .vars
                .n
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (data.items[k].name.clone(), v.into()))
                .collect(),
            items_output: self
                .vars
                .x
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (data.items[k].name.clone(), v.into()))
                .collect(),
            resources_needed,
            items_needed,
            recipes_used: self
                .vars
                .r
                .iter()
                .map(|(k, v)| (k, self.solution.value(*v)))
                .filter(|(_, v)| *v > EPSILON)
                .map(|(k, v)| (data.recipes[k].name.clone(), v.into()))
                .collect(),
            power_produced: self
                .vars
                .x
                .iter()
                .filter(|(k, _)| power_sources.contains(*k))
                .map(|(k, v)| (k.clone(), self.solution.value(*v).into()))
                .collect(),
            power_use: self.solution.value(self.vars.power_use),
            item_use: self.solution.value(self.vars.item_use).into(),
            buildings: self.solution.value(self.vars.building_use).into(),
            resources: self.solution.value(self.vars.resource_use).into(),
            buildings_scaled: self.solution.value(self.vars.buildings_scaled),
            resources_scaled: self.solution.value(self.vars.resources_scaled),
            products_map,
            ingredients_map,
        }
    }
}

#[derive(Debug)]
pub struct SolutionValues {
    pub sink_points: Rational,
    pub items_input: HashMap<String, Rational>,
    pub items_output: HashMap<String, Rational>,
    pub resources_needed: HashMap<String, Rational>,
    pub items_needed: HashMap<String, Rational>,
    pub recipes_used: HashMap<String, Rational>,
    pub power_produced: HashMap<Key, Rational>,

    pub power_use: f64,
    pub item_use: Rational,
    pub buildings: Rational,
    pub resources: Rational,
    pub buildings_scaled: f64,
    pub resources_scaled: f64,

    pub products_map: HashMap<String, HashMap<String, Rational>>,
    pub ingredients_map: HashMap<String, HashMap<String, Rational>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Rational(num::rational::Rational64);

impl Rational {}

impl From<f64> for Rational {
    fn from(value: f64) -> Self {
        fn is_near(v: f64, expected: f64) -> bool {
            const EPSILON: f64 = 1e-5;
            v <= expected + EPSILON && v >= expected - EPSILON
        }

        fn is_zero(v: f64) -> bool {
            is_near(v, 0.0)
        }

        if is_zero(value) {
            return Self(num::rational::Rational64::ZERO);
        }

        let raw = num::rational::Rational64::from_f64(value)
            .unwrap()
            .reduced();
        let numer = *raw.numer();
        let denom = *raw.denom();
        let whole = numer / denom;
        let fractional = numer % denom;
        let fractional = fractional as f64 / denom as f64;

        if is_zero(fractional) {
            return Self(num::rational::Rational64::new(whole, 1));
        }

        for i in 1i64..=20 {
            for j in 1..=i {
                if is_near(fractional, j as f64 / i as f64) {
                    return Self(num::rational::Rational64::new((whole * j) + i, j));
                }
            }
        }

        panic!("Irreducable float: {value}");
    }
}

impl Debug for Rational {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let this = self.0.reduced();
        if *this.denom() == 1 {
            Debug::fmt(this.numer(), f)
        } else {
            let num = *this.numer();
            let den = *this.denom();

            let whole = num / den;
            if whole != 0 {
                write!(f, "{} {}/{}", whole, num % den, den)
            } else {
                write!(f, "{}/{}", num, den)
            }
        }
    }
}
