use crate::pass_yir_lowering::BasicBlock;
use crate::pass_yir_lowering::{
    yir::{self, Instruction},
};
use indexmap::IndexMap;
use petgraph::algo::isomorphism::is_isomorphic;
use std::rc::Rc;
use crate::pass_lifetime_analysis::memory_graph::{MemoryGraph, G};
use crate::pass_type_inference::TypeRegistry;

type PendingFrees = IndexMap<ustr::Ustr, IndexMap<yir::Label, Vec<(usize, Instruction)>>>;

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

fn process_block(
    block: &yir::BasicBlock,
    predecessor_map: &IndexMap<yir::Label, Vec<&yir::BasicBlock>>,
    memo: &mut IndexMap<yir::Label, Rc<MemoryGraph>>,
    function_name: ustr::Ustr,
    type_registry: &TypeRegistry,
) -> (Rc<MemoryGraph>, PendingFrees) {
    let initial_state = memo.get(&block.label);

    if let Some(is) = initial_state {
        return (is.clone(), IndexMap::new());
    }

    // Otherwise, we have to actually do the full computation...

    // THis will later be replaced... We're using this here to actually get informed about loops..
    memo.insert(block.label, Rc::new(MemoryGraph::default()));

    let mut initial_state = Option::<MemoryGraph>::None;

    // Retrieve state from all predecessors

    if let Some(pred_bbs) = predecessor_map.get(&block.label) {
        for pred_bb in pred_bbs {
            let (pred_bb_state, _pred_frees) =
                process_block(pred_bb, predecessor_map, memo, function_name, type_registry);
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
        let free_instructions = initial_state.process_instruction(instr, type_registry);

        // Convert instructions to tuples with position info
        for heap_free_instruction in free_instructions {
            all_pending_frees
                .entry(function_name)
                .or_default()
                .entry(block.label)
                .or_default()
                .push((idx, heap_free_instruction)); // (instruction_index, heap_free_instruction)
        }
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

    pub fn run(&self, module: &mut yir::Module, type_registry: &TypeRegistry) {
        let pending_frees = analyze_module(module, type_registry);
        emit_pending_frees(module, pending_frees);
    }
}

fn analyze_module(module: &yir::Module, type_registry: &TypeRegistry) -> PendingFrees {
    let mut all_pending_frees: PendingFrees = IndexMap::new();

    for (_name, func_state) in &module.functions {
        let function = match func_state {
            yir::FunctionDeclarationState::Defined(func) => func,
            yir::FunctionDeclarationState::Declared(_) => continue,
        };

        let (_graph, pending_frees) = process_function(function, *_name, type_registry);
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
                    for (instruction_index, instr) in block_frees.into_iter().rev() {
                        if instruction_index <= block.instructions.len() {
                            block.instructions.insert(instruction_index, instr);
                        }
                    }
                }
            }
        }
    }
}

fn process_function(func: &yir::Function, function_name: ustr::Ustr, type_registry: &TypeRegistry) -> (G, PendingFrees) {
    let predecessor_map = build_predecessor_map(func);
    let mut memo = IndexMap::default();
    let mut all_pending_frees: PendingFrees = IndexMap::new();

    // Process all blocks to build the complete memoization table
    for block in func.blocks.values() {
        let (_state, pending_frees) =
            process_block(block, &predecessor_map, &mut memo, function_name, type_registry);
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
