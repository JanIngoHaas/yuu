use crate::pass_yir_lowering::BasicBlock;
use crate::pass_yir_lowering::{
    Operand,
    yir::{self, Instruction, Variable},
};
use indexmap::{IndexMap, IndexSet};
use petgraph::data::DataMap;
use petgraph::{Direction, algo::isomorphism::is_isomorphic, visit::EdgeRef};
use ustr::UstrMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

type G = petgraph::graphmap::GraphMap<Node, EdgeType, petgraph::Directed>;

type PendingFrees = IndexMap<ustr::Ustr, IndexMap<yir::Label, Vec<(usize, Instruction)>>>;

//#[derive(Clone, Copy, PartialEq, Eq)]
//enum EdgeType {
//    Definitive,
//    PossiblyZero,
//}

type EdgeType = ();

#[derive(Clone, Copy, Eq, Debug)]
enum Node {
    Heap { id: u64 },
    StackVariable { var: Variable },
    MemoryAddress { addr: u64 },
    GarbageMemory,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Heap { id } => write!(f, "heap_{}", id),
            Node::StackVariable { var } => {
                var.write_unique_name(f)?;
                write!(f, "(StackVar)")
            }
            Node::MemoryAddress { addr } => {
                write!(f, "addr_{}(MemAddr)", addr)
            }
            Node::GarbageMemory => write!(f, "garbage"),
        }
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Node::Heap { id: id1 }, Node::Heap { id: id2 }) => id1.cmp(id2),
            (Node::StackVariable { var: v1, .. }, Node::StackVariable { var: v2, .. }) => {
                v1.id().cmp(&v2.id())
            }
            (Node::MemoryAddress { addr: a1 }, Node::MemoryAddress { addr: a2 }) => a1.cmp(a2),
            (Node::GarbageMemory, Node::GarbageMemory) => std::cmp::Ordering::Equal,
            (Node::Heap { .. }, _) => std::cmp::Ordering::Less,
            (Node::StackVariable { .. }, Node::Heap { .. }) => std::cmp::Ordering::Greater,
            (Node::StackVariable { .. }, Node::MemoryAddress { .. }) => std::cmp::Ordering::Less,
            (Node::StackVariable { .. }, Node::GarbageMemory) => std::cmp::Ordering::Less,
            (Node::MemoryAddress { .. }, Node::Heap { .. }) => std::cmp::Ordering::Greater,
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
    fn heap_alloc(id: u64) -> Node {
        Node::Heap { id }
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
            Node::Heap { id } => {
                0u8.hash(state);
                id.hash(state);
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
            (Node::Heap { id: id1 }, Node::Heap { id: id2 }) => id1 == id2,
            (Node::StackVariable { var: v1, .. }, Node::StackVariable { var: v2, .. }) => v1 == v2,
            (Node::MemoryAddress { addr: a1 }, Node::MemoryAddress { addr: a2 }) => a1 == a2,
            (Node::GarbageMemory, Node::GarbageMemory) => true,
            _ => false,
        }
    }
}

// fn perform_reachability_analysis(g: &G, stack_vars: &IndexSet<Node>) -> (G, G) {
//     let mut reachable = IndexSet::new();
//     let mut dfs = Dfs::empty(g);

//     // Use all variables as starting points, but only if the node still exists in the graph
//     for var in stack_vars {
//         dfs.stack.push(*var);
//     }

//     while let Some(n) = dfs.next(g) {
//         reachable.insert(n);
//     }

//     let mut subgraph_reachable = G::default();
//     let mut subgraph_unreachable = G::default();

//     for (n1, n2, _) in g.all_edges() {
//         if reachable.contains(&n1) && reachable.contains(&n2) {
//             subgraph_reachable.add_node(n1);
//             subgraph_reachable.add_node(n2);
//             subgraph_reachable.add_edge(n1, n2, ());
//         } else {
//             subgraph_unreachable.add_node(n1);
//             subgraph_unreachable.add_node(n2);
//             subgraph_unreachable.add_edge(n1, n2, ());
//         }
//     }

//     (subgraph_reachable, subgraph_unreachable)
// }

// fn emit_heap_frees(
//     g: &G,
//     yir_func: &mut crate::pass_yir_lowering::yir::Function,
// ) -> Vec<Instruction> {
//     let mut instructions = Vec::new();

//     // Use topological sort to respect dependency order
//     let topo_order = toposort(g, None).unwrap();

//     for node in topo_order {
//         if matches!(node, Node::Heap { .. }) {
//             use crate::pass_parse::ast::InternUstr;
//             let ptr_var = yir_func.fresh_variable("temp_for_heap_free".intern(),
//                 crate::pass_type_inference::primitive_u64().ptr_to());

//             let heap_free = Instruction::HeapFree {
//                 ptr: Operand::Variable(ptr_var)
//             };
//             instructions.push(heap_free);
//         }
//     }

//     instructions
// }

// // This performs a complete pass over the whole graph accounting for cycles. Runtime: Whole graph, i.e. O(all edges + all nodes)
// fn garbage_collect_cycle_aware(g: &mut G, stack: &mut IndexSet<Node>, to_remove: &[Node]) {
//     for v in to_remove {
//         let out = stack.swap_remove(v);
//         //debug_assert!(out);
//     }

//     // Extract the subgraph before modifying the main graph
//     let (s_reach, _s_unreach) = perform_reachability_analysis(g, stack);

//     *g = s_reach;
// }

fn emit_frees_for_removed_nodes(removed_nodes: &[Node]) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    for &node in removed_nodes {
        if let Node::Heap { id } = node {
            use crate::pass_parse::ast::InternUstr;
            // Create a simple variable for the heap free operation
            // We use the heap node ID to create a unique variable
            let ptr_var = Variable::new(
                format!("temp_heap_free_{}", id).as_str().intern(),
                id as i64,
                crate::pass_type_inference::primitive_u64().ptr_to(),
            );

            let heap_free = Instruction::HeapFree {
                ptr: Operand::Variable(ptr_var),
            };
            instructions.push(heap_free);
        }
    }

    instructions
}

fn remove_nodes_cascade(g: &mut G, mut to_remove: Vec<Node>) -> Vec<Instruction> {
    let mut removed_topological = to_remove.clone();

    while let Some(node) = to_remove.pop() {
        for neigh in g.neighbors_directed(node, Direction::Outgoing) {
            if g.neighbors_directed(neigh, Direction::Incoming).count() == 1 {
                if matches!(g.node_weight(neigh), Some(Node::Heap { .. })) {
                    to_remove.push(neigh);
                }
            }
        }
        removed_topological.push(node);
        let _ = g.remove_node(node);
    }

    emit_frees_for_removed_nodes(&removed_topological)
}

// This performs only a pass over the subgraph not accounting for cycles (but it's quite fast!) -
// This requires that all worklist items are still contained in the graph! The garbage is collected based on the worklist items.
fn garbage_collect_from_nodes(
    g: &mut G,
    stack: &mut IndexSet<Node>,
    to_remove: Vec<Node>,
) -> Vec<Instruction> {
    for v in &to_remove {
        let _ = stack.swap_remove(v);
        //debug_assert!(out);
    }

    remove_nodes_cascade(g, to_remove)
}

fn garbage_collect_from_edges(g: &mut G, to_remove: Vec<(Node, Node)>) -> Vec<Instruction> {
    let mut worklist = Vec::with_capacity(to_remove.len());

    for tr in to_remove {
        g.remove_edge(tr.0, tr.1);
        worklist.push(tr.1);
    }

    remove_nodes_cascade(g, worklist)
}

fn process_instruction(
    g: &mut G,
    stack: &mut IndexSet<Node>,
    instruction: &Instruction,
    heap_counter: &mut u64,
) -> Vec<Instruction> {
    match instruction {
        Instruction::Alloca { target } => {
            let stack_node = g.add_node(Node::stack_alloc(*target));
            stack.insert(stack_node);
            g.add_edge(stack_node, Node::garbage_mem_addr(), ());
            Vec::new()
        }

        Instruction::HeapAlloc { target, .. } => {
            let heap_node = Node::heap_alloc(*heap_counter);
            *heap_counter += 1;
            let stack_node = Node::stack_alloc(*target);

            let node_from = g.add_node(stack_node);
            let node_to = g.add_node(heap_node);

            g.add_edge(node_from, node_to, ());

            stack.insert(node_from);
            Vec::new()
        }

        Instruction::TakeAddress { target, source } => {
            let stack_node = Node::stack_alloc(*target);
            g.add_node(stack_node);
            // Create edge from target to source (target points to source)

            g.add_edge(stack_node, Node::stack_alloc(*source), ());
            stack.insert(stack_node);
            Vec::new()
        }

        Instruction::Load { target, source } => {
            if let Operand::Variable(source_var) = source {
                let target_node = g.add_node(Node::stack_alloc(*target));

                let source_outgoing = g
                    .edges_directed(Node::stack_alloc(*source_var), Direction::Outgoing)
                    .map(|x| x.target())
                    .collect::<Vec<_>>();

                for edge_target in source_outgoing {
                    g.add_edge(target_node, edge_target, ());
                }
            }
            Vec::new()
        }

        Instruction::Store { dest: d, value: v } => {
            match (d, v) {
                (Operand::Variable(variable_to), value) => {
                    let lookup_node = Node::stack_alloc(*variable_to);

                    // Step 1: Find what dest points to and remove all edges into this direction.
                    let edges_to_remove = g
                        .edges_directed(lookup_node, Direction::Outgoing)
                        .map(|x| (x.source(), x.target()))
                        .collect::<Vec<_>>();

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
                    garbage_collect_from_edges(g, edges_to_remove)
                }
                _ => Vec::new(),
            }
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
            Vec::new()
        }

        Instruction::KillSet { vars } => {
            let nodes_to_remove: Vec<Node> = vars.iter().map(|x| Node::stack_alloc(*x)).collect();
            garbage_collect_from_nodes(g, stack, nodes_to_remove)
        }

        _ => Vec::new(),
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

#[derive(Clone)]
struct LifetimeState {
    graph: G,
    stack: IndexSet<Node>,
    heap_counter: u64,
}

impl Default for LifetimeState {
    fn default() -> Self {
        Self {
            graph: Default::default(),
            stack: Default::default(),
            heap_counter: 0,
        }
    }
}

fn process_block(
    block: &yir::BasicBlock,
    predecessor_map: &IndexMap<yir::Label, Vec<&yir::BasicBlock>>,
    memo: &mut IndexMap<yir::Label, Rc<LifetimeState>>,
    function_name: ustr::Ustr,
) -> (Rc<LifetimeState>, PendingFrees) {
    let initial_state = memo.get(&block.label);

    if let Some(is) = initial_state {
        return (is.clone(), IndexMap::new());
    }

    // Otherwise, we have to actually do the full computation...

    // THis will later be replaced... We're using this here to actually get informed about loops..
    memo.insert(block.label, Rc::new(LifetimeState::default()));

    let mut initial_state = Option::<LifetimeState>::None;

    // Retrieve state from all predecessors

    if let Some(pred_bbs) = predecessor_map.get(&block.label) {
        for pred_bb in pred_bbs {
            let (pred_bb_state, _pred_frees) =
                process_block(pred_bb, predecessor_map, memo, function_name);
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
    let mut all_pending_frees: PendingFrees = IndexMap::new();

    // TODO: If we have a "return" terminator, we need to be careful! Implicitly, we have to add another temp... But that for later.

    for (idx, instr) in block.instructions.iter().enumerate() {
        let free_instructions = process_instruction(
            &mut initial_state.graph,
            &mut initial_state.stack,
            instr,
            &mut initial_state.heap_counter,
        );

        // Convert instructions to tuples with position info
        for heap_free_instruction in free_instructions {
            all_pending_frees
                .entry(function_name)
                .or_default()
                .entry(block.label)
                .or_default()
                .push((idx, heap_free_instruction)); // (instruction_index, heap_free_instruction)
        }

        // Debug visualization (commented out to avoid file creation)
        // let name = format!("Block {} - {idx}", block.label);
        // let dot_content = format!("{:?}", Dot::with_config(&initial_state.graph, &[Config::EdgeNoLabel]));
        // let filename = format!("{name}.dot");
        // let pdf_filename = format!("{name}.pdf");
        // if let Ok(mut file) = File::create(&filename) {
        //     let _ = file.write_all(dot_content.as_bytes());
        //     let _ = Command::new("dot")
        //         .args(["-Tpdf", &filename, "-o", &pdf_filename])
        //         .output();
        // }
    }

    let is = Rc::new(initial_state);
    memo.insert(block.label, is.clone());
    (is, all_pending_frees)
}

pub struct LifetimeAnalysis;

impl LifetimeAnalysis {
    pub fn new() -> Self {
        LifetimeAnalysis
    }

    pub fn run(&self, module: &mut yir::Module) {
        let pending_frees = analyze_module(module);
        emit_pending_frees(module, pending_frees);
    }
}

fn analyze_module(module: &yir::Module) -> PendingFrees {
    let mut all_pending_frees: PendingFrees = IndexMap::new();

    for (_name, func_state) in &module.functions {
        let function = match func_state {
            yir::FunctionDeclarationState::Defined(func) => func,
            yir::FunctionDeclarationState::Declared(_) => continue,
        };

        let (_graph, pending_frees) = process_function(function, *_name);
        // Merge the function's pending frees into the module's pending frees
        for (func_name, func_frees) in pending_frees {
            let entry = all_pending_frees.entry(func_name).or_default();
            for (block_label, block_frees) in func_frees {
                entry.entry(block_label).or_default().extend(block_frees);
            }
        }
    }

    all_pending_frees
}

fn emit_pending_frees(module: &mut yir::Module, pending_frees: PendingFrees) {
    // Data is already organized by function -> block -> list of frees
    for (func_name, func_frees) in pending_frees {
        if let Some(yir::FunctionDeclarationState::Defined(func)) =
            module.functions.get_mut(&func_name)
        {
            let func = std::sync::Arc::get_mut(func).expect("Function should be uniquely owned");

            let mut label_to_key = IndexMap::new();
            for (key, block) in &func.blocks {
                label_to_key.insert(block.label, key.clone());
            }

            for (block_label, block_frees) in func_frees {
                if let Some(block) = func.blocks.get_mut(&block_label.id()) {
                    // Process backwards from highest index.
                    // Higher indices don't affect lower ones when inserting
                    for (instruction_index, heap_free_instruction) in block_frees.into_iter().rev() {
                        let insert_index = instruction_index + 1;
                        if insert_index <= block.instructions.len() {
                            block
                                .instructions
                                .insert(insert_index, heap_free_instruction);
                        }
                    }
                }
            }
        }
    }
}

fn process_function(func: &yir::Function, function_name: ustr::Ustr) -> (G, PendingFrees) {
    let predecessor_map = build_predecessor_map(func);
    let mut memo = IndexMap::default();
    let mut all_pending_frees: PendingFrees = IndexMap::new();

    // Process all blocks to build the complete memoization table
    for block in func.blocks.values() {
        let (_state, pending_frees) =
            process_block(block, &predecessor_map, &mut memo, function_name);
        // Merge block's pending frees into function's pending frees
        for (func_name, func_frees) in pending_frees {
            let entry = all_pending_frees.entry(func_name).or_default();
            for (block_label, block_frees) in func_frees {
                entry.entry(block_label).or_default().extend(block_frees);
            }
        }
    }

    // Find return blocks - they should all have isomorphic graphs after KillSet processing
    for block in func.blocks.values() {
        if matches!(block.terminator, yir::ControlFlow::Return(_)) {
            if let Some(block_state) = memo.get(&block.label) {
                return (block_state.graph.clone(), all_pending_frees);
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
    let graph = std::mem::take(out).graph;
    (graph, all_pending_frees)
}
