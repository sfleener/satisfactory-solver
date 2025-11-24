use crate::data::{Data, ItemKey, Settings};
use crate::rational::units::Recipes;
use crate::rational::{ItemsPerMinute, ItemsPerMinutePerRecipe, Rat};
use crate::solver::SolutionValues;
use petgraph::Direction;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fmt::{Debug, Formatter};

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = DiGraph::<(Rat<Recipes>, String), (ItemKey, ItemsPerMinute)>::new();
    let output = graph.add_node((Rat::ZERO, "output".to_string()));
    let mut recipe_nodes = HashMap::new();
    let mut resource_nodes = HashMap::new();

    for (recipe_key, &(_, recipe_val)) in &values.recipes_used {
        let recipe = &data.recipes[recipe_key];
        let node = graph.add_node((recipe_val, recipe.name.clone()));

        recipe_nodes.insert(recipe_key, (node, recipe.clone(), recipe_val));
    }

    let mut needs = VecDeque::new();
    for (k, v) in &settings.outputs {
        needs.push_back((*k, (output, *v)));
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
        for (provides_key, (_, provides_amount)) in &mut *provides {
            assert!(!provides_amount.is_zero());
            let (provides_node, _, _) = recipe_nodes.get(provides_key).unwrap();

            let edge = if let Some(e) = graph.find_edge(*provides_node, needs_node) {
                e
            } else {
                graph.add_edge(*provides_node, needs_node, (needs_key, Rat::ZERO))
            };

            let (_, edge_amount) = graph.edge_weight_mut(edge).unwrap();

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

            *edge_amount += difference;

            if provides_amount.is_zero() {
                to_remove.push(*provides_key);
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
            needs.push_back((needs_key, (needs_node, needs_amount.reduced())));
        }
    }

    for (needs_key, (needs_node, needs_amount)) in needs_resources {
        let resource_node = *resource_nodes.entry(needs_key).or_insert_with(|| {
            graph.add_node((
                needs_amount / ItemsPerMinutePerRecipe::ONE,
                data.resources.get(&needs_key).unwrap().name.clone(),
            ))
        });
        graph.add_edge(resource_node, needs_node, (needs_key, needs_amount));
    }

    if !provides_recipes.is_empty() {
        for (k, v) in provides_recipes {
            let extra: ItemsPerMinute = v.values().map(|(_, a)| *a).sum();
            let name = &data.items[&k].name;
            println!("Extra {name}: {extra:?}");
        }
    }

    let mut ranks = HashMap::new();
    let components = petgraph::algo::tarjan_scc(&graph);
    for node in components.into_iter().rev().flatten() {
        let Some(max) = graph
            .edges_directed(node, Direction::Incoming)
            .map(|edge| ranks.get(&edge.source()).copied().unwrap())
            .max()
        else {
            ranks.insert(node, 0);
            continue;
        };
        ranks.insert(node, max + 1);
    }
    // println!("{ranks:?}");

    println!(
        "{:?}",
        DotGraphFmt {
            graph: &graph,
            ranks: &ranks,
            data,
        }
    );
}

struct DotGraphFmt<'a> {
    graph: &'a DiGraph<(Rat<Recipes>, String), (ItemKey, ItemsPerMinute)>,
    ranks: &'a HashMap<NodeIndex, u16>,
    data: &'a Data,
}

impl Debug for DotGraphFmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut reverse_ranks: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (k, v) in self.ranks {
            reverse_ranks.entry(*v).or_default().push(*k);
        }

        f.write_str("flowchart TD\n")?;

        for (rank, nodes) in reverse_ranks {
            writeln!(f, "  subgraph L{rank}",)?;

            for node in nodes {
                let (count, name) = self.graph.node_weight(node).unwrap();
                if count.is_zero() {
                    writeln!(f, "    {}[{}]", node.index(), name)?;
                } else {
                    writeln!(f, "    {}[{:?} {}]", node.index(), count, name,)?;
                }
            }

            writeln!(f, "  end")?;
        }

        for edge in self.graph.edge_references() {
            let (item, amount) = edge.weight();
            writeln!(
                f,
                "  {} -->|{:?} {}| {}",
                edge.source().index(),
                amount,
                self.data
                    .items
                    .get(item)
                    .map_or_else(|| &self.data.resources.get(item).unwrap().name, |i| &i.name),
                edge.target().index(),
            )?;
        }
        Ok(())
    }
}
