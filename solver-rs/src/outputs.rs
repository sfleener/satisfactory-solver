use crate::data::{Data, ItemKey, RecipeKey, Settings};
use crate::rational::units::Recipes;
use crate::rational::{ItemsPerMinute, ItemsPerMinutePerRecipe, Rat};
use crate::solver::SolutionValues;
use assertables::{assert_ge, assert_le};
use bimap::BiBTreeMap;
use itertools::Itertools;
use petgraph::data::DataMap;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Bfs, EdgeRef, IntoEdgesDirected, Topo, Walker, WalkerIter};
use petgraph::{Direction, EdgeDirection};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::{Debug, Formatter};
use termtree::Tree;

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = ProductionGraph::new();
    let output_node = graph.add_node((Rat::ZERO, "output".to_string()));
    let mut recipe_nodes = BTreeMap::new();
    let mut resource_nodes = BTreeMap::new();
    let mut input_nodes = BTreeMap::new();

    for (recipe_key, &(_, recipe_val)) in &values.recipes_used {
        let recipe = &data.recipes[recipe_key];
        let node = graph.add_node((recipe_val, recipe.name.clone()));

        recipe_nodes.insert(recipe_key, (node, recipe.clone(), recipe_val));
    }

    for (k, (name, v)) in &values.items_input {
        let node = graph.add_node((*v / ItemsPerMinutePerRecipe::ONE, name.clone()));
        input_nodes.insert(*k, node);
    }

    let mut needs = VecDeque::new();
    for (k, mut v) in &settings.outputs {
        if v.is_near_zero() {
            let (_, a) = &values.items_output[k];
            v = a;
        }
        needs.push_back((*k, (output_node, *v)));
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
                .insert(**k, (amount_per_recipe, amount));
        }

        for ingredient in &recipe.ingredients {
            let amount_per_recipe = ingredient.amount;
            let amount = amount_per_recipe * *v;
            println!("Adding {k} needs {amount:?} {}", ingredient.item);
            needs.push_back((ingredient.item, (recipe_nodes[k].0, amount)));
        }
    }

    let mut provides_inputs = BTreeMap::new();
    for (k, (_, amount)) in &values.items_input {
        provides_inputs.insert(*k, (input_nodes[k], *amount));
    }

    println!("{needs:?}");

    let mut needs_resources: BTreeMap<_, Vec<_>> = BTreeMap::new();

    while let Some((needs_key, (needs_node, mut needs_amount))) = needs.pop_front() {
        println!(
            "checking {:?} needs {needs_amount:?} of {needs_key}",
            graph.node_weight(needs_node)
        );
        if data.resources.contains_key(&needs_key) {
            println!("Is resource, checking after");
            needs_resources
                .entry(needs_key)
                .or_default()
                .push((needs_node, needs_amount));
            continue;
        }

        assert!(
            !needs_amount.is_near_zero(),
            "Non-zero needs remaining: {needs_amount:?}"
        );
        if let Some((provides_node, provides_amount)) = provides_inputs.get_mut(&needs_key)
            && !provides_amount.is_near_zero()
        {
            let edge = if let Some(e) = graph.find_edge(*provides_node, needs_node) {
                e
            } else {
                graph.add_edge(
                    *provides_node,
                    needs_node,
                    BeltKind::Items {
                        item: needs_key,
                        rate: Rat::ZERO,
                    },
                )
            };

            let BeltKind::Items {
                item: _,
                rate: edge_amount,
            } = graph.edge_weight_mut(edge).unwrap()
            else {
                unreachable!()
            };

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

            if needs_amount.is_near_zero() {
                continue;
            }
        }

        let Some(provides) = provides_recipes.get_mut(&needs_key) else {
            panic!(
                "No recipes remaining that provide {needs_amount:?} {needs_key}: {provides_recipes:?}\n{:?}",
                Dot::new(&graph)
            );
        };
        assert!(!provides.is_empty());
        let mut to_remove = vec![];
        for (provides_key, (_, provides_amount)) in &mut *provides {
            assert!(!provides_amount.is_near_zero());
            let (provides_node, _, _) = recipe_nodes.get(provides_key).unwrap();

            let edge = if let Some(e) = graph.find_edge(*provides_node, needs_node) {
                e
            } else {
                graph.add_edge(
                    *provides_node,
                    needs_node,
                    BeltKind::Items {
                        item: needs_key,
                        rate: Rat::ZERO,
                    },
                )
            };

            let BeltKind::Items {
                item: _,
                rate: edge_amount,
            } = graph.edge_weight_mut(edge).unwrap()
            else {
                unreachable!()
            };

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

            if provides_amount.is_near_zero() {
                to_remove.push(*provides_key);
            }

            if needs_amount.is_near_zero() {
                break;
            }
        }
        for r in to_remove {
            provides.remove(&r);
        }
        if provides.is_empty() {
            provides_recipes.remove(&needs_key);
        }
        if !needs_amount.is_near_zero() {
            needs.push_back((needs_key, (needs_node, needs_amount.reduced())));
        }
    }

    for (needs_key, needs) in needs_resources {
        for (needs_node, needs_amount) in needs {
            println!(
                "Needed resource {:?}: {} {:?}",
                needs_key,
                needs_amount,
                graph.node_weight(needs_node)
            );

            let new_value = needs_amount / ItemsPerMinutePerRecipe::ONE;

            let resource_node = *resource_nodes.entry(needs_key).or_insert_with(|| {
                graph.add_node((
                    Rat::ZERO,
                    data.resources.get(&needs_key).unwrap().name.clone(),
                ))
            });
            let weight = &mut graph.node_weight_mut(resource_node).unwrap().0;
            *weight = *weight + new_value;
            graph.add_edge(
                resource_node,
                needs_node,
                BeltKind::Items {
                    item: needs_key,
                    rate: needs_amount,
                },
            );
        }
    }

    if !provides_recipes.is_empty() {
        for (k, v) in &provides_recipes {
            let extra: ItemsPerMinute = v.values().map(|(_, a)| *a).sum();
            let name = &data
                .items
                .get(&k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &data.resources[&k].name);
            println!("Extra {name}: {extra:?}");
        }
    }

    let ranks = deduce_ranks(&graph);

    {
        for node in graph.node_indices() {
            if node == output_node {
                continue;
            }
            if ranks[&node].rank == 0 {
                continue;
            }
            if graph.edges_directed(node, Direction::Outgoing).count() != 1 {
                continue;
            }
            let target = graph
                .edges_directed(node, Direction::Outgoing)
                .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                .next()
                .unwrap()
                .target();
            if target == output_node {
                continue;
            }
            let node_rank = ranks.get(&node).unwrap_or_else(|| {
                let nodes = graph
                    .node_indices()
                    .map(|i| (i, graph.node_weight(i)))
                    .collect::<Vec<_>>();
                panic!(
                    "Rank not found for node {node:?}\n\nnodes: {nodes:?}\n\nranks: {ranks:?}\n\n"
                )
            });
            let target_rank = ranks.get(&target).unwrap();
            if node_rank >= target_rank {
                continue;
            }
            graph.add_edge(target, node, BeltKind::InlineLink);
        }
    }

    // deduce ranks a second time once inline links have been generated
    let ranks = deduce_ranks(&graph);

    // assert ranks are well-ordered
    for (&node, &rank) in &ranks {
        for outgoing in graph
            .edges_directed(node, Direction::Outgoing)
            .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
        {
            assert_le!(rank, ranks[&outgoing.target()]);
        }
        for incoming in graph
            .edges_directed(node, Direction::Incoming)
            .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
        {
            assert_ge!(rank, ranks[&incoming.target()]);
        }
    }

    for (rank, layer) in ranks
        .iter()
        .sorted_by_key(|(_, v)| v.rank)
        .chunk_by(|(_, v)| v.rank)
        .into_iter()
    {
        println!("~~~ LAYER {} ~~~", rank);
        for (c_id, component) in layer
            .sorted_by_key(|(_, v)| v.component)
            .chunk_by(|(_, v)| v.component)
            .into_iter()
        {
            if c_id.is_none() {
                for (&node, _) in component {
                    let (amount, name) = graph.node_weight(node).unwrap();
                    let incoming = graph
                        .edges_directed(node, Direction::Incoming)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                        .map(|edge| &graph.node_weight(edge.source()).unwrap().1)
                        .collect::<BTreeSet<_>>();
                    let outgoing = graph
                        .edges_directed(node, Direction::Outgoing)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                        .map(|edge| *ranks.get(&edge.target()).unwrap())
                        .collect::<BTreeSet<_>>();
                    println!(
                        "{} {}: {:?} --> {} --> {:?}",
                        amount, name, incoming, rank, outgoing
                    );
                }
            } else {
                #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
                struct GlobalNodeIndex(NodeIndex);
                #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
                struct LocalNodeIndex(NodeIndex);

                let component_nodes = component
                    .into_iter()
                    .map(|(i, _)| GlobalNodeIndex(*i))
                    .collect::<Vec<_>>();

                let mut local = DiGraph::<GlobalNodeIndex, ()>::new();
                let local_nodes = BiBTreeMap::from_iter(
                    component_nodes
                        .iter()
                        .copied()
                        .map(|i| (i, LocalNodeIndex(local.add_node(i)))),
                );
                for &id in &component_nodes {
                    for edge in graph
                        .edges_directed(id.0, Direction::Outgoing)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                    {
                        if local.node_weight(edge.target()).is_none()
                            || local.node_weight(id.0).is_none()
                        {
                            continue;
                        };
                        local.add_edge(edge.target(), id.0, ());
                    }
                }

                fn tree<'a>(
                    node: GlobalNodeIndex,
                    ranks: &BTreeMap<NodeIndex, Rank>,
                    local_nodes: &BiBTreeMap<GlobalNodeIndex, LocalNodeIndex>,
                    graph: &'a ProductionGraph,
                ) -> Tree<String> {
                    let (amount, name) = graph.node_weight(node.0).unwrap();
                    let incoming = graph
                        .edges_directed(node.0, Direction::Incoming)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                        .map(|edge| &graph.node_weight(edge.source()).unwrap().1)
                        .collect::<BTreeSet<_>>();
                    let outgoing = graph
                        .edges_directed(node.0, Direction::Outgoing)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                        .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                        .collect::<BTreeSet<_>>();
                    let s = format!(
                        "{} {}: {:?} --> ... --> {:?}",
                        amount, name, incoming, outgoing
                    );
                    // let mut t = Tree::new(graph.node_weight(node.0).unwrap().1.as_str());
                    let mut t = Tree::new(s);
                    for edge in graph
                        .edges_directed(node.0, Direction::Incoming)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                    {
                        let g = GlobalNodeIndex(edge.source());
                        if !local_nodes.contains_left(&g) {
                            continue;
                        }
                        t.push(tree(g, ranks, local_nodes, graph));
                    }
                    t
                }

                let root = LocalNodeIndex(Topo::new(&local).next(&local).unwrap());
                print!(
                    "{}",
                    tree(
                        *local_nodes.get_by_right(&root).unwrap(),
                        &ranks,
                        &local_nodes,
                        &graph
                    )
                );
            }
        }
    }

    println!(
        "{:?}",
        DotGraphFmt {
            graph: &graph,
            ranks: &ranks,
            data,
            extra_inputs: &provides_inputs,
            extras: &provides_recipes,
        }
    );
}

#[derive(Debug, Clone)]
enum BeltKind {
    Items { item: ItemKey, rate: ItemsPerMinute },
    InlineLink,
}

fn deduce_ranks(graph: &ProductionGraph) -> BTreeMap<NodeIndex, Rank> {
    let mut ranks = BTreeMap::<_, Rank>::new();
    let components = petgraph::algo::tarjan_scc(&graph);

    let mut next_component_id = 0;
    for component in components.iter().rev() {
        if component.is_empty() {
            continue;
        }

        let component_id = if component.len() <= 1 {
            None
        } else {
            let id = next_component_id;
            next_component_id += 1;
            Some(id)
        };
        let component_nodes = component
            .iter()
            .map(|i| &graph.node_weight(*i).unwrap().1)
            .collect::<Vec<_>>();
        println!("Component: {component_nodes:?}");

        let max_component_parent = component
            .iter()
            .flat_map(|&node| {
                graph
                    .edges_directed(node, Direction::Incoming)
                    .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                    .filter_map(|edge| ranks.get(&edge.source()).copied().map(|r| r.rank))
            })
            .max();

        if let Some(max) = max_component_parent {
            for &node in component {
                ranks.insert(
                    node,
                    Rank {
                        rank: max + 1,
                        component: component_id,
                    },
                );
            }
        } else {
            for &node in component {
                ranks.insert(
                    node,
                    Rank {
                        rank: 0,
                        component: component_id,
                    },
                );
            }
        }
    }
    ranks
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Rank {
    rank: u16,
    component: Option<u8>,
}

type ProductionGraph = DiGraph<(Rat<Recipes>, String), BeltKind>;

struct DotGraphFmt<'a> {
    graph: &'a ProductionGraph,
    ranks: &'a BTreeMap<NodeIndex, Rank>,
    data: &'a Data,
    extra_inputs: &'a BTreeMap<ItemKey, (NodeIndex, ItemsPerMinute)>,
    extras: &'a BTreeMap<ItemKey, BTreeMap<RecipeKey, (ItemsPerMinutePerRecipe, ItemsPerMinute)>>,
}

impl Debug for DotGraphFmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut reverse_ranks: BTreeMap<_, Vec<_>> = BTreeMap::new();
        for (k, v) in self.ranks {
            reverse_ranks.entry(*v).or_default().push(*k);
        }

        f.write_str("flowchart TD\n")?;

        for (rank, nodes) in reverse_ranks {
            writeln!(f, "  subgraph L{}", rank.rank)?;

            for node in nodes {
                let (count, name) = self.graph.node_weight(node).unwrap();
                if count.is_near_zero() {
                    writeln!(f, "    {}[{}]", node.index(), name)?;
                } else {
                    writeln!(f, "    {}[{:?} {}]", node.index(), count, name,)?;
                }
            }

            writeln!(f, "  end")?;
        }

        writeln!(f, "  subgraph Extras")?;
        for (k, (_, extra)) in self.extra_inputs {
            if extra.is_near_zero() {
                continue;
            }
            let name = &self
                .data
                .items
                .get(&k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &self.data.resources[&k].name);
            writeln!(f, "    {k}[{extra:?} {name}]",)?;
        }
        for (k, v) in self.extras {
            let extra: ItemsPerMinute = v.values().map(|(_, a)| *a).sum();
            let name = &self
                .data
                .items
                .get(&k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &self.data.resources[&k].name);
            writeln!(f, "    {k}[{extra:?} {name}]",)?;
        }
        writeln!(f, "  end")?;

        for edge in self.graph.edge_references() {
            match edge.weight() {
                BeltKind::Items { item, rate } => {
                    writeln!(
                        f,
                        "  {} -->|{:?} {}| {}",
                        edge.source().index(),
                        rate,
                        self.data.items.get(item).map_or_else(
                            || &self.data.resources.get(item).unwrap().name,
                            |i| &i.name
                        ),
                        edge.target().index(),
                    )?;
                }
                BeltKind::InlineLink => {}
            }
        }
        Ok(())
    }
}
