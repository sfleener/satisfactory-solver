use crate::data::{Data, Settings};
use crate::rational::units::Recipes;
use crate::rational::{ItemsPerMinute, Rat};
use crate::solver::SolutionValues;
use petgraph::dot::Dot;
use petgraph::graph::DiGraph;
use petgraph::visit::EdgeRef;
use petgraph::{Direction, EdgeDirection};
use std::collections::{BTreeMap, HashMap, VecDeque};

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = DiGraph::<(Rat<Recipes>, String), ItemsPerMinute>::new();
    let output = graph.add_node((Rat::ZERO, "output".to_string()));
    let mut recipe_nodes = HashMap::new();
    let mut resource_nodes = HashMap::new();

    for (recipe_key, &(_, recipe_val)) in &values.recipes_used {
        let recipe = &data.recipes[recipe_key];
        let node = graph.add_node((recipe_val, recipe.name.clone()));

        recipe_nodes.insert(recipe_key.clone(), (node, recipe.clone(), recipe_val));
    }

    let mut needs = VecDeque::new();
    for (k, v) in &settings.outputs {
        needs.push_back((k.clone(), (output, Rat::from(*v))));
    }

    let mut provides_recipes: BTreeMap<_, BTreeMap<_, _>> = BTreeMap::new();
    for (k, (_, _, v)) in &recipe_nodes {
        let recipe = &data.recipes[k];
        for product in &recipe.products {
            let amount_per_recipe = product.amount;
            let amount = amount_per_recipe * (*v);
            println!("Adding {k} provides {amount:?} {}", product.item);
            provides_recipes
                .entry(product.item)
                .or_default()
                .insert(*k, (amount_per_recipe, amount));
        }

        for ingredient in &recipe.ingredients {
            let amount_per_recipe = ingredient.amount;
            let amount = amount_per_recipe * *v;
            println!("Adding {k} needs {amount:?} {}", ingredient.item);
            needs.push_back((ingredient.item, (recipe_nodes[k].0, amount)));
        }
    }

    println!("{needs:?}");

    let mut needs_resources = BTreeMap::new();

    while let Some((needs_key, (needs_node, mut needs_amount))) = needs.pop_front() {
        if data.resources.contains_key(&needs_key) {
            needs_resources.insert(needs_key, (needs_node, needs_amount));
            continue;
        }

        println!("checking needs {needs_amount:?} of {needs_key}");
        assert!(!needs_amount.is_zero());
        let Some(provides) = provides_recipes.get_mut(&needs_key) else {
            panic!(
                "No recipes remaining that provide {needs_amount:?} {needs_key}: {provides_recipes:?}\n{:?}",
                Dot::new(&graph)
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
                graph.add_edge(*provides_node, needs_node, Rat::ZERO)
            };

            let edge_amount = graph.edge_weight_mut(edge).unwrap();

            let difference = if *provides_amount >= needs_amount {
                let difference = *edge_amount + needs_amount;
                *provides_amount = *provides_amount - needs_amount;
                needs_amount = Rat::ZERO;
                difference
            } else {
                let difference = *edge_amount + *provides_amount;
                needs_amount = needs_amount - *provides_amount;
                *provides_amount = Rat::ZERO;
                difference
            };

            *edge_amount = *edge_amount + difference;

            if provides_amount.is_zero() {
                to_remove.push(*provides_key);
            }

            // let needed_provides_recipes = difference / *provides_amount_per_recipe;
            //
            // for ingredient in &provides_recipe.ingredients {
            //     let ingredient_per_recipe = ingredient.amount;
            //     needs.push_back((
            //         ingredient.item,
            //         (
            //             *provides_node,
            //             ingredient_per_recipe * needed_provides_recipes,
            //         ),
            //     ));
            // }

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
            needs.push_back((needs_key, (needs_node, needs_amount.reduced())));
        }
    }

    for (needs_key, (needs_node, needs_amount)) in needs_resources {
        let resource_node = *resource_nodes.entry(needs_key).or_insert_with(|| {
            graph.add_node((
                Rat::ZERO,
                data.resources.get(&needs_key).unwrap().name.to_string(),
            ))
        });
        graph.add_edge(resource_node, needs_node, needs_amount);
    }

    // assert!(provides_recipes.is_empty(), "{provides_recipes:?}");
    if !provides_recipes.is_empty() {
        for (k, v) in provides_recipes {
            let extra: ItemsPerMinute = v.values().into_iter().map(|(_, a)| *a).sum();
            let name = &data.items[&k].name;
            println!("Extra {name}: {extra:?}");
        }
    }

    let mut ranks = HashMap::new();
    let components = petgraph::algo::tarjan_scc(&graph);
    for node in components.into_iter().flatten() {
        if ranks.is_empty() {
            assert_eq!(node, output);
            ranks.insert(node, 0u16);
            continue;
        }
        let Some(max) = graph
            .edges_directed(node, Direction::Outgoing)
            .map(|edge| ranks.get(&edge.target()).copied().unwrap())
            .max()
        else {
            assert_eq!(node, output);
            ranks.insert(node, 0);
            continue;
        };
        ranks.insert(node, max + 1);
    }
    println!("{:?}", ranks);

    println!("{:?}", Dot::new(&graph));
}
