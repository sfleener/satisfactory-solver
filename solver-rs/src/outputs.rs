use crate::{
    data::{Data, ItemKey, Name, RecipeKey, Settings},
    rational::units::Recipes,
    rational::{ItemsPerMinute, ItemsPerMinutePerRecipe, Rat},
    solver::SolutionValues,
};
use assertables::{assert_ge, assert_le};
use bimap::BiBTreeMap;
use itertools::Itertools;
use petgraph::{
    Direction,
    dot::Dot,
    graph::{DiGraph, EdgeIndex, EdgeReference, NodeIndex},
    visit::EdgeRef,
};
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt::{Debug, Display, Formatter},
};
use termtree::Tree;

pub fn output_graph(settings: &Settings, data: &Data, values: &SolutionValues) {
    let mut graph = ProductionGraph::new();
    let mut resource_nodes = BTreeMap::new();

    let recipe_nodes = graph.add_recipe_nodes(data, values);
    let input_nodes = graph.add_input_nodes(values);

    let mut needs_queue = graph.add_base_needs(settings, values);

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
                .insert(*k, (amount_per_recipe, amount));
        }

        for ingredient in &recipe.ingredients {
            let amount_per_recipe = ingredient.amount;
            let rate = amount_per_recipe * *v;
            println!("Adding {k} needs {rate:?} {}", ingredient.item);
            needs_queue.push_back(NeedsEntry {
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

    println!("{needs_queue:?}");

    let mut needs_resources: BTreeMap<_, Vec<_>> = BTreeMap::new();

    while let Some(mut needs) = needs_queue.pop_front() {
        println!(
            "checking {:?} needs {:?} of {}",
            graph.get_node(needs.node),
            needs.rate,
            needs.item,
        );
        if data.resources.contains_key(&needs.item) {
            println!("Is resource, checking after");
            needs_resources
                .entry(needs.item)
                .or_default()
                .push((needs.node, needs.rate));
            continue;
        }

        assert!(
            !needs.rate.is_near_zero(),
            "Non-zero needs remaining: {:?}",
            needs.rate
        );
        if let Some((provides_node, provides_rate)) = provides_inputs.get_mut(&needs.item)
            && !provides_rate.is_near_zero()
        {
            graph.update_needs(&mut needs, *provides_node, provides_rate);

            if needs.rate.is_near_zero() {
                continue;
            }
        }

        let Some(provides) = provides_recipes.get_mut(&needs.item) else {
            panic!(
                "No recipes remaining that provide {:?} {}: {provides_recipes:?}\n{:?}",
                needs.rate,
                needs.item,
                graph.as_dot(),
            );
        };
        assert!(!provides.is_empty());
        let mut to_remove = vec![];
        for (provides_key, (_, provides_amount)) in &mut *provides {
            assert!(!provides_amount.is_near_zero());
            let RecipeNode {
                node: provides_node,
                ..
            } = recipe_nodes.get(provides_key).unwrap();

            graph.update_needs(&mut needs, *provides_node, provides_amount);

            if provides_amount.is_near_zero() {
                to_remove.push(*provides_key);
            }

            if needs.rate.is_near_zero() {
                break;
            }
        }
        for r in to_remove {
            provides.remove(&r);
        }
        if provides.is_empty() {
            provides_recipes.remove(&needs.item);
        }
        if !needs.rate.is_near_zero() {
            needs.rate.reduce();
            needs_queue.push_back(needs);
        }
    }

    for (needs_key, needs) in needs_resources {
        for (needs_node, needs_amount) in needs {
            println!(
                "Needed resource {:?}: {} {:?}",
                needs_key,
                needs_amount,
                graph.get_node(needs_node)
            );

            let new_value = needs_amount / ItemsPerMinutePerRecipe::ONE;

            let resource_node = *resource_nodes.entry(needs_key).or_insert_with(|| {
                graph.add_node(Rat::ZERO, data.resources.get(&needs_key).unwrap().name)
            });
            graph.increase_node_amount(resource_node, new_value);
            graph.add_edge(resource_node, needs_node, needs_key, needs_amount);
        }
    }

    // Dump extra resources
    if !provides_recipes.is_empty() {
        for (k, v) in &provides_recipes {
            let extra: ItemsPerMinute = v.values().map(|(_, a)| *a).sum();
            let name = &data
                .items
                .get(k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &data.resources[k].name);
            println!("Extra {name}: {extra:?}");
        }
    }

    let ranks = graph.deduce_ranks();

    {
        for node in graph.node_ids() {
            if graph.is_output(node) {
                continue;
            }
            if ranks[&node].rank == 0 {
                continue;
            }
            if graph.outgoing_edges(node).count() != 1 {
                continue;
            }
            let target = graph.outgoing_edges(node).next().unwrap().target();
            if graph.is_output(target) {
                continue;
            }
            let node_rank = ranks.get(&node).unwrap_or_else(|| {
                let nodes = graph
                    .node_ids()
                    .map(|i| (i, graph.get_node(i)))
                    .collect::<Vec<_>>();
                panic!(
                    "Rank not found for node {node:?}\n\nnodes: {nodes:?}\n\nranks: {ranks:?}\n\n"
                )
            });
            let target_rank = ranks.get(&target).unwrap();
            if node_rank >= target_rank {
                continue;
            }
            graph.add_backlink(node, target);
        }
    }

    // deduce ranks a second time once inline links have been generated
    let ranks = graph.deduce_ranks();

    // assert ranks are well-ordered
    for (&node, &rank) in &ranks {
        for outgoing in graph.outgoing_edges(node) {
            assert_le!(rank, ranks[&outgoing.target()]);
        }
        for incoming in graph.incoming_edges(node) {
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
                    let ProductionNode { amount, name } = graph.get_node(node).unwrap();
                    let incoming = graph
                        .incoming_edges(node)
                        .map(|edge| &graph.get_node(edge.source()).unwrap().name)
                        .collect::<BTreeSet<_>>();
                    let outgoing = graph
                        .outgoing_edges(node)
                        .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                        .collect::<BTreeSet<_>>();
                    println!(
                        "{} {}: {:?} --> {} --> {:?}",
                        amount, name, incoming, rank, outgoing
                    );
                }
            } else {
                let component_nodes = component.into_iter().collect::<Vec<_>>();
                // println!(
                //     ">>>>>>{:?}",
                //     component_nodes
                //         .iter()
                //         .map(|i| graph.get_node(i.id()).unwrap().1.clone())
                //         .collect::<Vec<_>>()
                // );

                let mut local = ComponentGraph::new();
                let mut local_nodes = BiBTreeMap::from_iter(
                    component_nodes
                        .iter()
                        .copied()
                        .map(|(&i, _)| (GlobalNodeIndex::Node(i), local.add_node(i))),
                );
                local_nodes.insert(GlobalNodeIndex::Root, local.root);
                for &(&id, _) in &component_nodes {
                    let &local_id = local_nodes.get_by_left(&GlobalNodeIndex::Node(id)).unwrap();
                    let mut has_outgoing = false;
                    for edge in graph.outgoing_edges(id) {
                        let Some(&target) =
                            local_nodes.get_by_left(&GlobalNodeIndex::Node(edge.target()))
                        else {
                            continue;
                        };
                        local.add_edge(target, local_id);
                        has_outgoing = true;
                    }

                    if !has_outgoing {
                        local.add_edge(local.root(), local_id);
                    }
                }

                fn tree(
                    local_node: ComponentNodeIndex,
                    ranks: &BTreeMap<ProductionNodeIndex, Rank>,
                    local_nodes: &BiBTreeMap<GlobalNodeIndex, ComponentNodeIndex>,
                    graph: &ProductionGraph,
                    local_graph: &ComponentGraph,
                    visited: &mut BTreeSet<ComponentNodeIndex>,
                ) -> Tree<String> {
                    let global_node = *local_nodes.get_by_right(&local_node).unwrap();
                    let s = match global_node {
                        GlobalNodeIndex::Node(node) => {
                            let ProductionNode { amount, name } = graph.get_node(node).unwrap();
                            let incoming = graph
                                .incoming_edges(node)
                                .map(|edge| &graph.get_node(edge.source()).unwrap().name)
                                .collect::<BTreeSet<_>>();
                            let outgoing = graph
                                .outgoing_edges(node)
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
                    for edge in local_graph.outgoing_edges(local_node) {
                        let new = edge.target();
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

                let tree = if local.is_cyclic() {
                    let mut root = Tree::new("Cyclic Group".to_string());
                    for local_node in local.node_ids() {
                        if local_node == local.root() {
                            continue;
                        }
                        let GlobalNodeIndex::Node(node) =
                            *local_nodes.get_by_right(&local_node).unwrap()
                        else {
                            unreachable!()
                        };

                        let ProductionNode { amount, name } = graph.get_node(node).unwrap();
                        let incoming = graph
                            .incoming_edges(node)
                            .map(|edge| &graph.get_node(edge.source()).unwrap().name)
                            .collect::<BTreeSet<_>>();
                        let outgoing = graph
                            .outgoing_edges(node)
                            .map(|edge| ranks.get(&edge.target()).unwrap().rank)
                            .collect::<BTreeSet<_>>();
                        let s = format!(
                            "{} {}: {:?} --> ... --> {:?}",
                            amount, name, incoming, outgoing
                        );
                        root.push(Tree::new(s));
                    }
                    println!("{:?}", local.dot_writer(&graph));
                    root
                } else {
                    let tree = tree(
                        local.root(),
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
    Node(ProductionNodeIndex),
    Root,
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ComponentNodeIndex(NodeIndex);

#[derive(Debug, Clone, Copy)]
struct ComponentEdgeReference<'a>(EdgeReference<'a, ()>);

impl ComponentEdgeReference<'_> {
    fn target(self) -> ComponentNodeIndex {
        ComponentNodeIndex(self.0.target())
    }
}

struct ComponentGraph {
    inner: DiGraph<GlobalNodeIndex, ()>,
    root: ComponentNodeIndex,
}

impl ComponentGraph {
    fn new() -> Self {
        let mut inner = DiGraph::new();
        let root = ComponentNodeIndex(inner.add_node(GlobalNodeIndex::Root));
        Self { inner, root }
    }

    fn root(&self) -> ComponentNodeIndex {
        self.root
    }

    fn is_cyclic(&self) -> bool {
        petgraph::algo::is_cyclic_directed(&self.inner)
    }

    fn add_node(&mut self, node: ProductionNodeIndex) -> ComponentNodeIndex {
        ComponentNodeIndex(self.inner.add_node(GlobalNodeIndex::Node(node)))
    }

    fn add_edge(&mut self, src: ComponentNodeIndex, dst: ComponentNodeIndex) {
        self.inner.add_edge(src.0, dst.0, ());
    }

    fn node_ids(&self) -> impl Iterator<Item = ComponentNodeIndex> + use<> {
        self.inner.node_indices().map(ComponentNodeIndex)
    }

    fn outgoing_edges<'a>(
        &'a self,
        node: ComponentNodeIndex,
    ) -> impl Iterator<Item = ComponentEdgeReference<'a>> + use<'a> {
        self.inner
            .edges_directed(node.0, Direction::Outgoing)
            .map(ComponentEdgeReference)
    }

    fn dot_writer<'a>(&'a self, graph: &'a ProductionGraph) -> impl Debug + use<'a> {
        struct DotWriter<'a> {
            local: &'a ComponentGraph,
            graph: &'a ProductionGraph,
        }

        impl Debug for DotWriter<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_str("flowchart TD\n")?;

                for node in self.local.node_ids() {
                    match self.local.inner.node_weight(node.0).unwrap() {
                        GlobalNodeIndex::Node(n) => {
                            writeln!(
                                f,
                                "  {}[{}]",
                                node.0.index(),
                                self.graph.get_node(*n).unwrap().name
                            )?;
                        }
                        GlobalNodeIndex::Root => {}
                    }
                }
                for edge in self.local.inner.edge_references() {
                    if edge.source() == self.local.root.0 || edge.target() == self.local.root.0 {
                        continue;
                    }

                    writeln!(
                        f,
                        "  {} --> {}",
                        edge.target().index(),
                        edge.source().index()
                    )?;
                }

                Ok(())
            }
        }

        DotWriter { local: self, graph }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Rank {
    rank: u16,
    component: Option<u8>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ProductionNodeIndex(NodeIndex);

impl ProductionNodeIndex {
    fn index(self) -> usize {
        self.0.index()
    }
}

#[derive(Debug, Clone, Copy)]
struct ProductionEdgeReference<'a>(EdgeReference<'a, BeltKind>);

impl ProductionEdgeReference<'_> {
    fn source(self) -> ProductionNodeIndex {
        ProductionNodeIndex(self.0.source())
    }

    fn target(self) -> ProductionNodeIndex {
        ProductionNodeIndex(self.0.target())
    }
}

#[derive(Debug)]
struct ProductionNode {
    amount: Rat<Recipes>,
    name: Name,
}

impl Display for ProductionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.amount, self.name)
    }
}

struct RecipeNode {
    node: ProductionNodeIndex,
    amount: Rat<Recipes>,
}

impl RecipeNode {
    pub fn new(node: ProductionNodeIndex, amount: Rat<Recipes>) -> Self {
        Self { node, amount }
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
        ProductionNodeIndex(self.inner.add_node(ProductionNode { amount, name }))
    }

    pub fn add_recipe_nodes(
        &mut self,
        data: &Data,
        values: &SolutionValues,
    ) -> BTreeMap<RecipeKey, RecipeNode> {
        let mut recipe_nodes = BTreeMap::new();
        for (&recipe_key, &(_, recipe_val)) in &values.recipes_used {
            let recipe = &data.recipes[&recipe_key];
            let node = self.add_node(recipe_val, recipe.name);

            recipe_nodes.insert(recipe_key, RecipeNode::new(node, recipe_val));
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

    fn outgoing_edges<'a>(
        &'a self,
        node: ProductionNodeIndex,
    ) -> impl Iterator<Item = ProductionEdgeReference<'a>> + use<'a> {
        self.edges(node, Direction::Outgoing)
    }

    fn incoming_edges<'a>(
        &'a self,
        node: ProductionNodeIndex,
    ) -> impl Iterator<Item = ProductionEdgeReference<'a>> + use<'a> {
        self.edges(node, Direction::Incoming)
    }

    fn edges<'a>(
        &'a self,
        node: ProductionNodeIndex,
        direction: Direction,
    ) -> impl Iterator<Item = ProductionEdgeReference<'a>> + use<'a> {
        self.inner
            .edges_directed(node.0, direction)
            .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
            .map(ProductionEdgeReference)
    }

    fn add_edge(
        &mut self,
        src: ProductionNodeIndex,
        dst: ProductionNodeIndex,
        item: ItemKey,
        rate: ItemsPerMinute,
    ) -> EdgeIndex {
        self.inner
            .add_edge(src.0, dst.0, BeltKind::Items { item, rate })
    }

    fn get_or_insert_default_edge(
        &mut self,
        src: ProductionNodeIndex,
        dst: ProductionNodeIndex,
        default_item: ItemKey,
    ) -> EdgeIndex {
        if let Some(e) = self.inner.find_edge(src.0, dst.0) {
            e
        } else {
            self.add_edge(src, dst, default_item, Rat::ZERO)
        }
    }

    fn node_ids(&self) -> impl Iterator<Item = ProductionNodeIndex> + use<> {
        self.inner.node_indices().map(ProductionNodeIndex)
    }

    fn get_node(&self, node: ProductionNodeIndex) -> Option<&ProductionNode> {
        self.inner.node_weight(node.0)
    }

    fn add_backlink(&mut self, src: ProductionNodeIndex, dst: ProductionNodeIndex) {
        self.inner.add_edge(dst.0, src.0, BeltKind::InlineLink);
    }

    fn update_needs(
        &mut self,
        needs: &mut NeedsEntry,
        provides_node: ProductionNodeIndex,
        provides_rate: &mut ItemsPerMinute,
    ) {
        let edge = self.get_or_insert_default_edge(provides_node, needs.node, needs.item);

        let BeltKind::Items {
            item: _,
            rate: edge_amount,
        } = self.inner.edge_weight_mut(edge).unwrap()
        else {
            unreachable!()
        };

        let difference = if *provides_rate >= needs.rate {
            let difference = *edge_amount + needs.rate;
            *provides_rate -= needs.rate;
            needs.rate = Rat::ZERO;
            difference
        } else {
            let difference = *edge_amount + *provides_rate;
            needs.rate -= *provides_rate;
            *provides_rate = Rat::ZERO;
            difference
        };

        *edge_amount += difference;
    }

    fn increase_node_amount(&mut self, node: ProductionNodeIndex, delta: Rat<Recipes>) {
        let weight = &mut self.inner.node_weight_mut(node.0).unwrap().amount;
        *weight += delta;
    }

    fn deduce_ranks(&self) -> BTreeMap<ProductionNodeIndex, Rank> {
        let mut ranks: BTreeMap<ProductionNodeIndex, _> = BTreeMap::<_, Rank>::new();
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
            // let component_nodes = component
            //     .iter()
            //     .map(|i| &self.inner.node_weight(*i).unwrap())
            //     .collect::<Vec<_>>();
            // println!("Component: {component_nodes:?}");

            let max_component_parent = component
                .iter()
                .flat_map(|&node| {
                    self.inner
                        .edges_directed(node, Direction::Incoming)
                        .filter(|e| matches!(e.weight(), BeltKind::Items { .. }))
                        .filter_map(|edge| {
                            ranks
                                .get(&ProductionNodeIndex(edge.source()))
                                .copied()
                                .map(|r| r.rank)
                        })
                })
                .max();

            if let Some(max) = max_component_parent {
                for &node in component {
                    ranks.insert(
                        ProductionNodeIndex(node),
                        Rank {
                            rank: max + 1,
                            component: component_id,
                        },
                    );
                }
            } else {
                for &node in component {
                    ranks.insert(
                        ProductionNodeIndex(node),
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

    fn as_dot(&self) -> Dot<'_, &DiGraph<ProductionNode, BeltKind>> {
        Dot::new(&self.inner)
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
    ranks: &'a BTreeMap<ProductionNodeIndex, Rank>,
    data: &'a Data,
    extra_inputs: &'a BTreeMap<ItemKey, (ProductionNodeIndex, ItemsPerMinute)>,
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

            for id in nodes {
                let node = self.graph.get_node(id).unwrap();
                if node.amount.is_near_zero() {
                    writeln!(f, "    {}[{}]", id.index(), node.name)?;
                } else {
                    writeln!(f, "    {}[{:?} {}]", id.index(), node.amount, node.name)?;
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
                .get(k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &self.data.resources[k].name);
            writeln!(f, "    {k}[{extra:?} {name}]",)?;
        }
        for (k, v) in self.extras {
            let extra: ItemsPerMinute = v.values().map(|(_, a)| *a).sum();
            let name = &self
                .data
                .items
                .get(k)
                .map(|i| &i.name)
                .unwrap_or_else(|| &self.data.resources[k].name);
            writeln!(f, "    {k}[{extra:?} {name}]",)?;
        }
        writeln!(f, "  end")?;

        for edge in self.graph.inner.edge_references() {
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
