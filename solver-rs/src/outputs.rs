use crate::data::{Data, Settings};
use crate::rational::Rational;
use crate::solver::SolutionValues;
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use std::collections::{BTreeMap, HashMap};

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = DiGraph::<(Rational, String), Rational>::new();
    let output = graph.add_node((Rational::ZERO, "output".to_string()));
    let mut recipe_nodes = HashMap::new();
    let mut resource_nodes = HashMap::new();

    for (recipe_key, &(_, recipe_val)) in &values.recipes_used {
        let recipe = &data.recipes[recipe_key];
        let node = graph.add_node((recipe_val, recipe.name.clone()));

        recipe_nodes.insert(recipe_key.clone(), (node, recipe.clone(), recipe_val));
    }

    let mut provides_recipes: BTreeMap<_, BTreeMap<_, _>> = BTreeMap::new();
    for (k, (_, _, v)) in &recipe_nodes {
        let recipe = &data.recipes[k];
        for product in &recipe.products {
            let amount_per_recipe = Rational::from((60.0 / recipe.time) * product.amount);
            let amount = amount_per_recipe * *v;
            println!("Adding {k} with {amount:?}: {}", product.item);
            provides_recipes
                .entry(product.item)
                .or_default()
                .insert(*k, (amount_per_recipe, amount));
        }
    }

    let mut needs = BTreeMap::new();
    for (k, v) in &settings.outputs {
        needs.insert(k.clone(), (output, Rational::from(*v)));
    }

    println!("{needs:?}");

    let mut needs_resources = BTreeMap::new();

    while let Some((needs_key, (needs_node, mut needs_amount))) = needs.pop_first() {
        if data.resources.contains_key(&needs_key) {
            needs_resources.insert(needs_key, (needs_node, needs_amount));
            continue;
        }

        println!("checking needs {needs_amount:?} of {needs_key}");
        assert!(!needs_amount.is_zero());
        let Some(provides) = provides_recipes.get_mut(&needs_key) else {
            panic!(
                "No recipes remaining that provide {needs_amount:?} {needs_key}: {provides_recipes:?}"
            );
        };
        assert!(!provides.is_empty());
        let mut to_remove = vec![];
        for (provides_key, (provides_amount_per_recipe, provides_amount)) in &mut *provides {
            assert!(!provides_amount.is_zero());
            let (provides_node, provides_recipe, _) = recipe_nodes.get(provides_key).unwrap();

            let edge = if let Some(e) = graph.find_edge(*provides_node, needs_node) {
                e
            } else {
                graph.add_edge(*provides_node, needs_node, Rational::ZERO)
            };

            let edge_amount = graph.edge_weight_mut(edge).unwrap();

            let difference = if *provides_amount >= needs_amount {
                let difference = *edge_amount + needs_amount;
                *provides_amount = *provides_amount - needs_amount;
                needs_amount = Rational::ZERO;
                difference
            } else {
                let difference = *edge_amount + *provides_amount;
                needs_amount = needs_amount - *provides_amount;
                *provides_amount = Rational::ZERO;
                difference
            };

            *edge_amount = *edge_amount + difference;

            if provides_amount.is_zero() {
                to_remove.push(*provides_key);
            }

            let needed_provides_recipes = difference / *provides_amount_per_recipe;

            for ingredient in &provides_recipe.ingredients {
                let ingredient_per_recipe =
                    Rational::from((60.0 / provides_recipe.time) * ingredient.amount);
                needs.insert(
                    ingredient.item,
                    (
                        *provides_node,
                        ingredient_per_recipe * needed_provides_recipes,
                    ),
                );
            }

            if needs_amount.is_zero() {
                break;
            }
        }
        for r in to_remove {
            provides.remove(&r);
        }
        if provides.is_empty() {
            provides_recipes.remove(&needs_key);
        }
        if !needs_amount.is_zero() {
            needs.insert(needs_key, (needs_node, needs_amount.reduced()));
        }
    }

    for (needs_key, (needs_node, mut needs_amount)) in needs_resources {
        let resource_node = *resource_nodes.entry(needs_key).or_insert_with(|| {
            graph.add_node((
                Rational::ZERO,
                data.resources.get(&needs_key).unwrap().name.to_string(),
            ))
        });
        graph.add_edge(resource_node, needs_node, needs_amount);
    }

    println!("{:?}", Dot::new(&graph));
}
