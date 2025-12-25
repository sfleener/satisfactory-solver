use crate::data::{Data, ItemKey, Name, Recipe, RecipeKey, Settings};
use crate::rational::units::Recipes;
use crate::rational::{ItemsPerMinute, ItemsPerMinutePerRecipe, Rat};
use crate::solver::SolutionValues;
use assertables::{assert_ge, assert_le};
use bimap::BiBTreeMap;
use itertools::Itertools;
use petgraph::Direction;
use petgraph::data::DataMap;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use petgraph::visit::{EdgeRef, IntoEdgesDirected, Walker};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;
use termtree::Tree;

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = ProductionGraph::new();
    let mut resource_nodes = BTreeMap::new();

    let recipe_nodes = graph.add_recipe_nodes(data, values);
    let input_nodes = graph.add_input_nodes(values);

    let mut needs = graph.add_base_needs(settings, values);

    let mut provides_recipes: BTreeMap<_, BTreeMap<_, _>> = BTreeMap::new();
    for (k, RecipeNode { amount: v, .. }) in &recipe_nodes {
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
            let rate = amount_per_recipe * *v;
            println!("Adding {k} needs {rate:?} {}", ingredient.item);
            needs.push_back(NeedsEntry {
                item: ingredient.item,
                node: recipe_nodes[k].node,
                rate,
            });
        }
    }

    let mut provides_inputs = BTreeMap::new();
    for (k, (_, amount)) in &values.items_input {
        provides_inputs.insert(*k, (input_nodes[k], *amount));
    }

    println!("{needs:?}");

    let mut needs_resources: BTreeMap<_, Vec<_>> = BTreeMap::new();

    while let Some(NeedsEntry {
        item: needs_key,
        node: needs_node,
        rate: mut needs_rate,
    }) = needs.pop_front()
    {
        println!(
            "checking {:?} needs {needs_rate:?} of {needs_key}",
            graph.node_weight(needs_node)
        );
        if data.resources.contains_key(&needs_key) {
            println!("Is resource, checking after");
            needs_resources
                .entry(needs_key)
                .or_default()
                .push((needs_node, needs_rate));
            continue;
        }

        assert!(
            !needs_rate.is_near_zero(),
            "Non-zero needs remaining: {needs_rate:?}"
        );
        if let Some((provides_node, provides_amount)) = provides_inputs.get_mut(&needs_key)
            && !provides_amount.is_near_zero()
        {
            let edge = graph.get_or_insert_edge(
                *provides_node,
                needs_node,
                BeltKind::Items {
                    item: needs_key,
                    rate: Rat::ZERO,
                },
            );

            let BeltKind::Items {
                item: _,
                rate: edge_amount,
            } = graph.edge_weight_mut(edge).unwrap()
            else {
                unreachable!()
            };

            let difference = if *provides_amount >= needs_rate {
                let difference = *edge_amount + needs_rate;
                *provides_amount = *provides_amount - needs_rate;
                needs_rate = Rat::ZERO;
                difference
            } else {
                let difference = *edge_amount + *provides_amount;
                needs_rate = needs_rate - *provides_amount;
                *provides_amount = Rat::ZERO;
                difference
            };

            *edge_amount += difference;

            if needs_rate.is_near_zero() {
                continue;
            }
        }

        let Some(provides) = provides_recipes.get_mut(&needs_key) else {
            panic!(
                "No recipes remaining that provide {needs_rate:?} {needs_key}: {provides_recipes:?}\n{:?}",
                Dot::new(&graph)
            );
        };
        assert!(!provides.is_empty());
        let mut to_remove = vec![];
        for (provides_key, (_, provides_amount)) in &mut *provides {
            assert!(!provides_amount.is_near_zero());
            let (provides_node, _, _) = recipe_nodes.get(provides_key).unwrap();

            let edge = graph.get_or_insert_edge(
                *provides_node,
                needs_node,
                BeltKind::Items {
                    item: needs_key,
                    rate: Rat::ZERO,
                },
            );

            let BeltKind::Items {
                item: _,
                rate: edge_amount,
            } = graph.edge_weight_mut(edge).unwrap()
            else {
                unreachable!()
            };

            let difference = if *provides_amount >= needs_rate {
                let difference = *edge_amount + needs_rate;
                *provides_amount = *provides_amount - needs_rate;
                needs_rate = Rat::ZERO;
                difference
            } else {
                let difference = *edge_amount + *provides_amount;
                needs_rate = needs_rate - *provides_amount;
                *provides_amount = Rat::ZERO;
                difference
            };

            *edge_amount += difference;

            if provides_amount.is_near_zero() {
                to_remove.push(*provides_key);
            }

            if needs_rate.is_near_zero() {
                break;
            }
        }
        for r in to_remove {
            provides.remove(&r);
        }
        if provides.is_empty() {
            provides_recipes.remove(&needs_key);
        }
        if !needs_rate.is_near_zero() {
            needs.push_back((needs_key, (needs_node, needs_rate.reduced())));
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
                graph.add_node(
                    Rat::ZERO,
                    data.resources.get(&needs_key).unwrap().name.clone(),
                )
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

    // Dump extra resources
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

    let ranks = graph.deduce_ranks();

    {
        for node in graph.node_indices() {
            if graph.is_output(node) {
                continue;
            }
            if ranks[&node].rank == 0 {
                continue;
            }
            if graph
                .edges_directed(node, Direction::Outgoing)
                .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                .count()
                != 1
            {
                continue;
            }
            let target = graph
                .edges_directed(node, Direction::Outgoing)
                .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                .next()
                .unwrap()
                .target();
            if graph.is_output(target) {
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
    let ranks = graph.deduce_ranks();

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
                        .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                        .collect::<BTreeSet<_>>();
                    println!(
                        "{} {}: {:?} --> {} --> {:?}",
                        amount, name, incoming, rank, outgoing
                    );
                }
            } else {
                let component_nodes = component
                    .into_iter()
                    .map(|(i, _)| GlobalNodeIndex::Node(*i))
                    .collect::<Vec<_>>();
                // println!(
                //     ">>>>>>{:?}",
                //     component_nodes
                //         .iter()
                //         .map(|i| graph.node_weight(i.id()).unwrap().1.clone())
                //         .collect::<Vec<_>>()
                // );

                let mut local = ComponentGraph::new();
                let local_root = LocalNodeIndex(local.add_node(GlobalNodeIndex::Root));
                let mut local_nodes = BiBTreeMap::from_iter(
                    component_nodes
                        .iter()
                        .copied()
                        .map(|i| (i, LocalNodeIndex(local.add_node(i)))),
                );
                local_nodes.insert(GlobalNodeIndex::Root, local_root);
                for &id in &component_nodes {
                    let local_id = local_nodes.get_by_left(&id).unwrap();
                    let mut has_outgoing = false;
                    for edge in graph
                        .edges_directed(id.id(), Direction::Outgoing)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                    {
                        let Some(target) =
                            local_nodes.get_by_left(&GlobalNodeIndex::Node(edge.target()))
                        else {
                            continue;
                        };
                        local.add_edge(target.0, local_id.0, ());
                        has_outgoing = true;
                    }

                    if !has_outgoing {
                        local.add_edge(local_root.0, local_id.0, ());
                    }
                }

                fn tree<'a>(
                    local_node: LocalNodeIndex,
                    ranks: &BTreeMap<NodeIndex, Rank>,
                    local_nodes: &BiBTreeMap<GlobalNodeIndex, LocalNodeIndex>,
                    graph: &'a ProductionGraph,
                    local_graph: &DiGraph<GlobalNodeIndex, ()>,
                    visited: &mut BTreeSet<LocalNodeIndex>,
                ) -> Tree<String> {
                    let global_node = *local_nodes.get_by_right(&local_node).unwrap();
                    let s = match global_node {
                        GlobalNodeIndex::Node(node) => {
                            let (amount, name) = graph.node_weight(node).unwrap();
                            let incoming = graph
                                .edges_directed(node, Direction::Incoming)
                                .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                                .map(|edge| &graph.node_weight(edge.source()).unwrap().1)
                                .collect::<BTreeSet<_>>();
                            let outgoing = graph
                                .edges_directed(node, Direction::Outgoing)
                                .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                                .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                                .collect::<BTreeSet<_>>();
                            format!(
                                "{} {}: {:?} --> ... --> {:?}",
                                amount, name, incoming, outgoing
                            )
                        }
                        GlobalNodeIndex::Root => "Group".to_string(),
                    };

                    let mut t = Tree::new(s);
                    for edge in local_graph.edges_directed(local_node.0, Direction::Outgoing) {
                        let new = LocalNodeIndex(edge.target());
                        if visited.contains(&new) {
                            continue;
                        }
                        visited.insert(new);
                        t.push(tree(new, ranks, local_nodes, graph, local_graph, visited));
                    }
                    t
                }

                fn tree_len<T: Display>(tree: &Tree<T>) -> usize {
                    tree.leaves.iter().map(|l| tree_len(l)).sum::<usize>() + 1
                }

                let tree = if petgraph::algo::is_cyclic_directed(&local) {
                    let mut root = Tree::new("Cyclic Group".to_string());
                    for local_node in local.node_indices() {
                        let local_node = LocalNodeIndex(local_node);
                        if local_node == local_root {
                            continue;
                        }
                        let GlobalNodeIndex::Node(node) =
                            *local_nodes.get_by_right(&local_node).unwrap()
                        else {
                            unreachable!()
                        };

                        let (amount, name) = graph.node_weight(node).unwrap();
                        let incoming = graph
                            .edges_directed(node, Direction::Incoming)
                            .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                            .map(|edge| &graph.node_weight(edge.source()).unwrap().1)
                            .collect::<BTreeSet<_>>();
                        let outgoing = graph
                            .edges_directed(node, Direction::Outgoing)
                            .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                            .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                            .collect::<BTreeSet<_>>();
                        let s = format!(
                            "{} {}: {:?} --> ... --> {:?}",
                            amount, name, incoming, outgoing
                        );
                        root.push(Tree::new(s));
                    }
                    root
                } else {
                    let tree = tree(
                        local_root,
                        &ranks,
                        &local_nodes,
                        &graph,
                        &local,
                        &mut BTreeSet::new(),
                    );
                    assert_eq!(tree_len(&tree), component_nodes.len() + 1);
                    tree
                };
                print!("{tree}");
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum GlobalNodeIndex {
    Node(NodeIndex),
    Root,
}
impl GlobalNodeIndex {
    fn id(self) -> NodeIndex {
        let Self::Node(id) = self else { panic!() };
        id
    }
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct LocalNodeIndex(NodeIndex);

struct ComponentGraph {
    inner: DiGraph<GlobalNodeIndex, ()>,
}

impl ComponentGraph {
    fn new() -> Self {
        Self {
            inner: DiGraph::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Rank {
    rank: u16,
    component: Option<u8>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ProductionNodeIndex(NodeIndex);

struct ProductionNode {
    amount: Rat<Recipes>,
    name: Name,
}

struct RecipeNode {
    node: ProductionNodeIndex,
    recipe: Arc<Recipe>,
    amount: Rat<Recipes>,
}

impl RecipeNode {
    pub fn new(node: ProductionNodeIndex, recipe: Arc<Recipe>, amount: Rat<Recipes>) -> Self {
        Self {
            node,
            recipe,
            amount,
        }
    }
}

struct ProductionGraph {
    inner: DiGraph<ProductionNode, BeltKind>,
    root: ProductionNodeIndex,
}

impl ProductionGraph {
    fn new() -> Self {
        let mut inner = DiGraph::new();
        let root = ProductionNodeIndex(inner.add_node(ProductionNode {
            amount: Rat::ZERO,
            name: Name::new_static("output"),
        }));
        Self { inner, root }
    }

    fn is_output(&self, node: ProductionNodeIndex) -> bool {
        node == self.root
    }

    fn add_node(&mut self, amount: Rat<Recipes>, name: Name) -> ProductionNodeIndex {
        ProductionNodeIndex(self.inner.add_node(ProductionNode {
            amount,
            name: name.into(),
        }))
    }

    pub fn add_recipe_nodes(
        &mut self,
        data: &Data,
        values: &SolutionValues,
    ) -> BTreeMap<RecipeKey, RecipeNode> {
        let mut recipe_nodes = BTreeMap::new();
        for (&recipe_key, &(_, recipe_val)) in &values.recipes_used {
            let recipe = &data.recipes[&recipe_key];
            let node = self.add_node(recipe_val, recipe.name.clone());

            recipe_nodes.insert(
                recipe_key,
                RecipeNode::new(node, recipe.clone(), recipe_val),
            );
        }
        recipe_nodes
    }

    pub fn add_input_nodes(
        &mut self,
        values: &SolutionValues,
    ) -> BTreeMap<ItemKey, ProductionNodeIndex> {
        let mut input_nodes = BTreeMap::new();
        for (k, (name, v)) in &values.items_input {
            let node = self.add_node(*v / ItemsPerMinutePerRecipe::ONE, *name);
            input_nodes.insert(*k, node);
        }
        input_nodes
    }

    fn add_base_needs(
        &mut self,
        settings: &Settings,
        values: &SolutionValues,
    ) -> VecDeque<NeedsEntry> {
        let mut needs = VecDeque::new();
        for (k, mut v) in &settings.outputs {
            if v.is_near_zero() {
                let (_, a) = &values.items_output[k];
                v = a;
            }
            needs.push_back(NeedsEntry {
                item: *k,
                node: self.root,
                rate: *v,
            });
        }
        needs
    }

    fn get_or_insert_edge(
        &mut self,
        src: ProductionNodeIndex,
        dst: ProductionNodeIndex,
        default: BeltKind,
    ) -> EdgeIndex {
        if let Some(e) = self.inner.find_edge(src.0, dst.0) {
            e
        } else {
            self.inner.add_edge(src.0, dst.0, default)
        }
    }

    fn get_or_insert_edge_with(
        &mut self,
        src: ProductionNodeIndex,
        dst: ProductionNodeIndex,
        default: impl FnOnce() -> BeltKind,
    ) -> EdgeIndex {
        if let Some(e) = self.inner.find_edge(src.0, dst.0) {
            e
        } else {
            self.inner.add_edge(src.0, dst.0, default())
        }
    }

    fn deduce_ranks(&self) -> BTreeMap<NodeIndex, Rank> {
        let mut ranks = BTreeMap::<_, Rank>::new();
        let components = petgraph::algo::tarjan_scc(&self.inner);

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
                .map(|i| &self.inner.node_weight(*i).unwrap())
                .collect::<Vec<_>>();
            // println!("Component: {component_nodes:?}");

            let max_component_parent = component
                .iter()
                .flat_map(|&node| {
                    self.inner
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
}

#[derive(Debug)]
struct NeedsEntry {
    item: ItemKey,
    node: ProductionNodeIndex,
    rate: ItemsPerMinute,
}

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
