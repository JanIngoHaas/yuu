use crate::pass_yir_lowering::BasicBlock;
use crate::pass_yir_lowering::{
    Operand,
    yir::{self, Instruction, Variable},
};
use indexmap::{IndexMap, IndexSet};
use petgraph::data::DataMap;
use petgraph::{
    Direction,
    algo::isomorphism::is_isomorphic,
    dot::{Dot, Config},
    visit::{Dfs, EdgeRef},
};
use std::fmt::Display;
use std::fs::File;
use std::hash::Hash;
use std::io::Write;
use std::process::Command;
use std::rc::Rc;

type G = petgraph::graphmap::GraphMap<Node, EdgeType, petgraph::Directed>;

//#[derive(Clone, Copy, PartialEq, Eq)]
//enum EdgeType {
//    Definitive,
//    PossiblyZero,
//}

type EdgeType = ();

#[derive(Clone, Copy, Eq, Debug)]
enum Node {
    Heap,
    StackVariable { var: Variable },
    MemoryAddress { addr: u64 },
    GarbageMemory,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Heap => write!(f, "heap"),
            Node::StackVariable { var} => {
                var.write_unique_name(f)?;
                write!(f, "(StackVar)")
            }
            Node::MemoryAddress { addr } => {
                write!(f, "addr_{}(MemAddr)", addr)
            }
            Node::GarbageMemory => write!(f, "garbage")
        }
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Node::Heap, Node::Heap) => std::cmp::Ordering::Equal,
            (Node::StackVariable { var: v1, .. }, Node::StackVariable { var: v2, .. }) => v1.id().cmp(&v2.id()),
            (Node::MemoryAddress { addr: a1 }, Node::MemoryAddress { addr: a2 }) => a1.cmp(a2),
            (Node::GarbageMemory, Node::GarbageMemory) => std::cmp::Ordering::Equal,
            (Node::Heap, _) => std::cmp::Ordering::Less,
            (Node::StackVariable { .. }, Node::Heap) => std::cmp::Ordering::Greater,
            (Node::StackVariable { .. }, Node::MemoryAddress { .. }) => std::cmp::Ordering::Less,
            (Node::StackVariable { .. }, Node::GarbageMemory) => std::cmp::Ordering::Less,
            (Node::MemoryAddress { .. }, Node::Heap) => std::cmp::Ordering::Greater,
            (Node::MemoryAddress { .. }, Node::StackVariable { .. }) => std::cmp::Ordering::Greater,
            (Node::MemoryAddress { .. }, Node::GarbageMemory) => std::cmp::Ordering::Less,
            (Node::GarbageMemory, _) => std::cmp::Ordering::Greater,
        }
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Node {
    fn heap_alloc() -> Node {
        Node::Heap
    }

    fn stack_alloc(var: Variable) -> Node {
        Node::StackVariable { var }
    }

    fn memory_address(addr: u64) -> Node {
        Node::MemoryAddress { addr }
    }

    fn garbage_mem_addr() -> Node {
        Node::GarbageMemory
    }
}

impl Hash for Node {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Node::Heap => {
                0u8.hash(state);
            }
            Node::StackVariable { var, .. } => {
                1u8.hash(state);
                var.hash(state);
            }
            Node::MemoryAddress { addr } => {
                2u8.hash(state);
                addr.hash(state);
            }
            Node::GarbageMemory => {
                3u8.hash(state);
            }
        }
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Node::Heap, Node::Heap) => true,
            (Node::StackVariable { var: v1, .. }, Node::StackVariable { var: v2, .. }) => v1 == v2,
            (Node::MemoryAddress { addr: a1 }, Node::MemoryAddress { addr: a2 }) => a1 == a2,
            (Node::GarbageMemory, Node::GarbageMemory) => true,
            _ => false,
        }
    }
}


fn perform_reachability_analysis(g: &G, stack_vars: &IndexSet<Node>) -> (G, G) {
    let mut reachable = IndexSet::new();
    let mut dfs = Dfs::empty(g);

    // Use all variables as starting points, but only if the node still exists in the graph
    for var in stack_vars {
        dfs.stack.push(*var);
    }

    while let Some(n) = dfs.next(g) {
        reachable.insert(n);
    }

    let mut subgraph_reachable = G::default();
    let mut subgraph_unreachable = G::default();

    for (n1, n2, _) in g.all_edges() {
        if reachable.contains(&n1) && reachable.contains(&n2) {
            subgraph_reachable.add_node(n1);
            subgraph_reachable.add_node(n2);
            subgraph_reachable.add_edge(n1, n2, ());
        } else {
            subgraph_unreachable.add_node(n1);
            subgraph_unreachable.add_node(n2);
            subgraph_unreachable.add_edge(n1, n2, ());
        }
    }

    (subgraph_reachable, subgraph_unreachable)
}

// fn emit_heap_frees(
//     g: &G,
//     yir_func: &mut crate::pass_yir_lowering::yir::Function,
// ) -> Vec<Instruction> {
//     let mut instructions = Vec::new();

//     // Simple approach: just generate HeapFree for all HeapAlloc nodes in the graph
//     for node_idx in g.node_indices() {
//         if let Some(node_weight) = g.node_weight(node_idx) {
//             if matches!(node_weight, NodeType::HeapAlloc(_)) {
//                 use crate::pass_parse::ast::InternUstr;
//                 let ptr_var = yir_func.fresh_variable("ptr".intern(),
//                     crate::pass_type_inference::primitive_u64().ptr_to());

//                 let heap_free = Instruction::HeapFree {
//                     ptr: Operand::Variable(ptr_var)
//                 };
//                 instructions.push(heap_free);
//             }
//         }
//     }

//     instructions
// }

// This performs a complete pass over the whole graph accounting for cycles. Runtime: Whole graph, i.e. O(all edges + all nodes)
fn garbage_collect_cycle_aware(g: &mut G, stack: &mut IndexSet<Node>, to_remove: &[Node]) {
    for v in to_remove {
        let out = stack.swap_remove(v);
        //debug_assert!(out);
    }

    // Extract the subgraph before modifying the main graph
    let (s_reach, _s_unreach) = perform_reachability_analysis(g, stack);

    *g = s_reach;
}

fn remove_nodes_cascade(g: &mut G, mut to_remove: Vec<Node>) {
    while let Some(node) = to_remove.pop() {
        for neigh in g.neighbors_directed(node, Direction::Outgoing) {
            if g.neighbors_directed(neigh, Direction::Incoming).count() == 1 {
                if matches!(g.node_weight(neigh), Some(Node::Heap)) {
                    to_remove.push(neigh);
                }
            }
        }

        let _ = g.remove_node(node);
    }
}

// This performs only a pass over the subgraph not accounting for cycles (but it's quite fast!) -
// This requires that all worklist items are still contained in the graph! The garbage is collected based on the worklist items.
fn garbage_collect_from_nodes<'a>(g: &mut G, stack: &mut IndexSet<Node>, to_remove: Vec<Node>) {
    for v in &to_remove {
        let _ = stack.swap_remove(v);
        //debug_assert!(out);
    }

    remove_nodes_cascade(g, to_remove);
}

fn garbage_collect_from_edges(g: &mut G, to_remove: Vec<(Node, Node)>) {
    let mut worklist = Vec::with_capacity(to_remove.len());

    for tr in to_remove {
        g.remove_edge(tr.0, tr.1);
        worklist.push(tr.1);
    }

    remove_nodes_cascade(g, worklist);
}

fn process_instruction(g: &mut G, stack: &mut IndexSet<Node>, instruction: &Instruction) -> bool {
    println!("Processing instruction: {:?}", instruction);
    match instruction {
        Instruction::Alloca { target } => {
            let stack_node = g.add_node(Node::stack_alloc(*target));
            stack.insert(stack_node);
            g.add_edge(stack_node, Node::garbage_mem_addr(), ());
            println!("  ALLOCA: Created uninitialized stack node {:?} for variable {:?}",
                     stack_node, target);
            true
        }

        Instruction::HeapAlloc { target, .. } => {
            let heap_node = Node::heap_alloc();
            let stack_node = Node::stack_alloc(*target);

            let node_from = g.add_node(stack_node);
            let node_to = g.add_node(heap_node);

            g.add_edge(node_from, node_to, ());

            stack.insert(node_from);
            true
        }

        Instruction::TakeAddress { target, source } => {
            let stack_node = Node::stack_alloc(*target);
            g.add_node(stack_node);
            // Create edge from target to source (target points to source)

            g.add_edge(stack_node, Node::stack_alloc(*source), ());
            stack.insert(stack_node);
            true
        }

        Instruction::Load { target, source } => {
            if let Operand::Variable(source_var) = source {
                let target_node = g.add_node(Node::stack_alloc(*target));

                let source_outgoing = g
                    .edges_directed(Node::stack_alloc(*source_var), Direction::Outgoing)
                    .map(|x| x.target()).collect::<Vec<_>>();

                for edge_target in source_outgoing {
                    g.add_edge(target_node, edge_target, ());
                }
            }
            true
        }

        Instruction::Store { dest: d, value: v } => {
            match (d, v) {
                (Operand::Variable(variable_to), value) => {
                    println!("  STORE: Looking for variable {:?} in graph", variable_to);
                    let lookup_node = Node::stack_alloc(*variable_to);
                    println!("  STORE: Lookup node is {:?}", lookup_node);

                    // Step 1: Find what dest points to and remove all edges into this direction. 
                    let edges_to_remove = g
                        .edges_directed(lookup_node, Direction::Outgoing)
                        .map(|x| (x.source(), x.target())).collect::<Vec<_>>();

                    for (from, to) in &edges_to_remove {
                        g.remove_edge(*from, *to);
                    }

                    // Step 2: Store the new value
                    if let Operand::Variable(variable_from) = value {
                        let targets = g
                            .edges_directed(Node::stack_alloc(*variable_from), Direction::Outgoing)
                            .map(|x| x.1)
                            .collect::<Vec<_>>();
                        for target in targets {
                            g.add_edge(Node::stack_alloc(*variable_to), target, ());
                        }
                    }

                    // Step 3: Garbage collect any nodes that became unreachable
                    garbage_collect_from_edges(g, edges_to_remove);
                }
                _ => return false,
            };
            true
        }

        Instruction::IntToPtr { target, source } => {
            let target_node = Node::stack_alloc(*target);
            g.add_node(target_node);
            stack.insert(target_node);

            let addr = match source {
                Operand::U64Const(i) => *i as u64,
                _ => 0,
            };
            let addr_node = Node::memory_address(addr);
            g.add_node(addr_node);

            g.add_edge(target_node, addr_node, ());
            true
        }

        Instruction::KillSet { vars } => {
            println!("  KILLSET: Removing variables {:?}", vars);
            let nodes_to_remove: Vec<Node> = vars.iter().map(|x| Node::stack_alloc(*x)).collect();
            println!("  KILLSET: Mapped to nodes {:?}", nodes_to_remove);
            garbage_collect_from_nodes(
                g,
                stack,
                nodes_to_remove,
            );
            true
        }

        _ => false,
    }
}

fn build_predecessor_map(function: &yir::Function) -> IndexMap<yir::Label, Vec<&BasicBlock>> {
    let mut predecessors = IndexMap::new();

    for (_, block) in &function.blocks {
        match &block.terminator {
            yir::ControlFlow::Jump { target } => {
                predecessors
                    .entry(target.clone())
                    .or_insert_with(Vec::new)
                    .push(block);
            }
            yir::ControlFlow::Branch {
                if_true, if_false, ..
            } => {
                predecessors
                    .entry(if_true.clone())
                    .or_insert_with(Vec::new)
                    .push(block);
                predecessors
                    .entry(if_false.clone())
                    .or_insert_with(Vec::new)
                    .push(block);
            }
            yir::ControlFlow::JumpTable {
                jump_targets,
                default,
                ..
            } => {
                for (_, target) in jump_targets {
                    predecessors
                        .entry(target.clone())
                        .or_insert_with(Vec::new)
                        .push(block);
                }
                if let Some(default_target) = default {
                    predecessors
                        .entry(default_target.clone())
                        .or_insert_with(Vec::new)
                        .push(block);
                }
            }
            yir::ControlFlow::Return(_) | yir::ControlFlow::Unterminated => {}
        }
    }

    predecessors
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum WorkingState {
    Pending,
    NotStarted,
}

#[derive(Clone)]
struct LifetimeState {
    graph: G,
    stack: IndexSet<Node>,
    status: WorkingState,
}

impl LifetimeState {
    fn pending() -> LifetimeState {
        LifetimeState {
            graph: Default::default(),
            stack: Default::default(),
            status: WorkingState::Pending,
        }
    }
}

impl Default for LifetimeState {
    fn default() -> Self {
        Self {
            graph: Default::default(),
            stack: Default::default(),
            status: WorkingState::NotStarted,
        }
    }
}

fn process_block(
    block: &yir::BasicBlock,
    predecessor_map: &IndexMap<yir::Label, Vec<&yir::BasicBlock>>,
    memo: &mut IndexMap<yir::Label, Rc<LifetimeState>>,
) -> Rc<LifetimeState> {
    let initial_state = memo.get(&block.label);

    if let Some(is) = initial_state {
        return is.clone();
    }

    // Otherwise, we have to actually do the full computation...

    // THis will later be replaced... We're using this here to actually get informed about loops..
    memo.insert(block.label, Rc::new(LifetimeState::pending()));

    let mut initial_state = Option::<LifetimeState>::None;

    // Retrieve state from all predecessors

    if let Some(pred_bbs) = predecessor_map.get(&block.label) {
        for pred_bb in pred_bbs {
            let pred_bb_state = process_block(pred_bb, predecessor_map, memo);
            if let Some(already_existing_state) = &initial_state {
                // If we don't have a full calc yet of ANY predecessor...
                if !is_isomorphic(&already_existing_state.graph, &pred_bb_state.graph) {
                    panic!("User Bug: Graphs not isomorphic"); // TODO: Make this a lifetime error that gets printed to console... We can print the subgraphs which are NOT isomorphic
                }
            } else {
                initial_state = Some(pred_bb_state.as_ref().clone());
            }
        }
    }

    // If it's still "None", we have the "entry" block...
    let mut initial_state = initial_state.unwrap_or_default();

    // TODO: If we have a "return" terminator, we need to be careful! Implicitly, we have to add another temp... But that for later.

    for (idx, instr) in block.instructions.iter().enumerate() {
        process_instruction(&mut initial_state.graph, &mut initial_state.stack, instr);
        let name = format!("Block {} - {idx}", block.label);

        let dot_content = format!("{:?}", Dot::with_config(&initial_state.graph, &[Config::EdgeNoLabel]));
        let filename = format!("{name}.dot");
        let pdf_filename = format!("{name}.pdf");

        if let Ok(mut file) = File::create(&filename) {
            let _ = file.write_all(dot_content.as_bytes());

            let _ = Command::new("dot")
                .args(["-Tpdf", &filename, "-o", &pdf_filename])
                .output();
        }
    }

    let is = Rc::new(initial_state);
    memo.insert(block.label, is.clone());
    is
}

pub struct LifetimeAnalysis;

impl LifetimeAnalysis {
    pub fn new() -> Self {
        LifetimeAnalysis
    }

    pub fn run(&self, module: &yir::Module) {
        analyze_module(module);
    }
}

fn analyze_module(module: &yir::Module) {
    for (name, func_state) in &module.functions {
        let function = match func_state {
            yir::FunctionDeclarationState::Defined(func) => func,
            yir::FunctionDeclarationState::Declared(_) => continue,
        };

        let graph = process_function(function);
    }
}

fn process_function(func: &yir::Function) -> G {
    let predecessor_map = build_predecessor_map(func);
    let mut memo = IndexMap::default();

    // Process all blocks to build the complete memoization table
    for block in func.blocks.values() {
        process_block(block, &predecessor_map, &mut memo);
    }

    // Find return blocks - they should all have isomorphic graphs after KillSet processing
    for block in func.blocks.values() {
        if matches!(block.terminator, yir::ControlFlow::Return(_)) {
            if let Some(block_state) = memo.get(&block.label) {
                return block_state.graph.clone();
            }
        }
    }

    // No return block found --> retrieve last block (implicit return block...)
    let out = func.blocks.last().unwrap();
    let mut out = memo.get(&out.1.label).unwrap().clone();
    memo.clear();
    let out = std::rc::Rc::get_mut(&mut out).unwrap_or_else(|| {
        unreachable!("Compiler Bug: We shouldnt ahve any outstanding refs here...")
    });
    std::mem::take(out).graph
}
