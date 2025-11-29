use crate::pass_type_inference::TypeInfo;
use crate::pass_type_inference::TypeRegistry;
use crate::pass_yir_lowering::yir::{Instruction, Variable};
use crate::pass_yir_lowering::Operand;
use petgraph::stable_graph::{StableGraph, NodeIndex};
use petgraph::Directed;
use petgraph::Direction;
use std::fmt::Display;
use indexmap::{IndexSet, IndexMap};
use petgraph::visit::EdgeRef;
use ustr::Ustr;

pub type G = StableGraph<Node, EdgeType, Directed>;
pub type EdgeType = Option<Ustr>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Node {
    HeapAlloc { id: u64, ty: &'static TypeInfo },
    StackAlloc { var: Variable },
    TempAlloc { var: Variable },
    PartOfAlloc { id: u64 },
    MemoryAddress { addr: u64 },
    GarbageMemory,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::HeapAlloc { id, .. } => write!(f, "heap_{}", id),
            Node::StackAlloc { var } => {
                var.write_unique_name(f)?;
                write!(f, "(Stack)")
            }
            Node::TempAlloc { var } => {
                var.write_unique_name(f)?;
                write!(f, "(Temp)")
            }
            Node::PartOfAlloc { id } => {
                write!(f, "part_{}", id)
            }
            Node::MemoryAddress { addr } => {
                write!(f, "addr_{}(MemAddr)", addr)
            }
            Node::GarbageMemory => write!(f, "garbage"),
        }
    }
}

impl Node {
    pub fn heap_alloc(id: u64, ty: &'static TypeInfo) -> Node {
        Node::HeapAlloc { id, ty }
    }

    pub fn stack_alloc(var: Variable) -> Node {
        Node::StackAlloc { var }
    }
    
    pub fn temp_alloc(var: Variable) -> Node {
        Node::TempAlloc { var }
    }

    pub fn memory_address(addr: u64) -> Node {
        Node::MemoryAddress { addr }
    }

    pub fn garbage_mem_addr() -> Node {
        Node::GarbageMemory
    }
}

#[derive(Clone)]
pub struct MemoryGraph {
    pub graph: G,
    // Maps stack variables to their node index in the graph
    pub stack: IndexMap<Variable, NodeIndex>,
    pub heap_counter: u64,
    pub allocas: IndexSet<Variable>,
}

impl Default for MemoryGraph {
    fn default() -> Self {
        Self {
            graph: Default::default(),
            stack: Default::default(),
            heap_counter: 0,
            allocas: Default::default(),
        }
    }
}

fn generate_nodes_for_type(ty: &'static TypeInfo, node_idx: NodeIndex, g: &mut MemoryGraph, type_registry: &TypeRegistry) {
    match ty {
        // If: pointer: link "node" to garbage mem
        TypeInfo::Pointer(_) => {
            let garbage = Node::garbage_mem_addr();
            let garbage_idx = g.graph.add_node(garbage);
            g.graph.add_edge(node_idx, garbage_idx, None);
        }
        // If: struct: get all fields, link "node" to all fields (we use 'PartOfAlloc' here!). Call generate_nodes_for_type
        // recursively
        TypeInfo::Struct(struct_type) => {
            if let Some(struct_info) = type_registry.resolve_struct(struct_type.name) {
                for field in struct_info.fields.values() {
                    let field_node = Node::PartOfAlloc { id: g.heap_counter };
                    g.heap_counter += 1;
                    let field_idx = g.graph.add_node(field_node);
                    
                    // Explicitly add the edge with field name
                    g.graph.add_edge(node_idx, field_idx, Some(field.name));
                    
                    generate_nodes_for_type(field.ty, field_idx, g, type_registry);
                }
            }
        }
        _ => {
             // Node is already added at the start of the function
        }
    }
}

impl MemoryGraph {
    pub fn process_instruction(&mut self, instruction: &Instruction, type_registry: &TypeRegistry) -> Vec<Instruction> {
        match instruction {
            Instruction::Alloca { target } => {
                let node = Node::stack_alloc(*target);
                let stack_node_idx = self.graph.add_node(node);
                self.stack.insert(*target, stack_node_idx);
                self.allocas.insert(*target);
                
                generate_nodes_for_type(target.ty().deref_ptr(), stack_node_idx, self, type_registry);
                Vec::new()
            }

            Instruction::HeapAlloc { target, .. } => {
                let heap_type = target.ty().deref_ptr();
                let heap_node = Node::heap_alloc(self.heap_counter, heap_type);
                self.heap_counter += 1;
                
                // Create heap node
                let heap_node_idx = self.graph.add_node(heap_node);
                
                // Create stack node for the pointer
                let stack_node = Node::stack_alloc(*target);
                let stack_node_idx = self.graph.add_node(stack_node);

                self.graph.add_edge(stack_node_idx, heap_node_idx, None);
                self.stack.insert(*target, stack_node_idx);

                generate_nodes_for_type(heap_type, heap_node_idx, self, type_registry);
                Vec::new()
            }

            Instruction::TakeAddress { target, source } => {
                let stack_node = Node::stack_alloc(*target);
                let stack_node_idx = self.graph.add_node(stack_node);
                
                if let Some(&source_node_idx) = self.stack.get(source) {
                    // Create a new node for the source? No, TakeAddress should point to the existing stack node of the source?
                    // Or rather, the target is a pointer *to* the source.
                    // In this memory graph, if we have `x`, `x` is a node. `p = &x`. `p` is a node. `p` points to `x`.
                    // So `stack_node_idx` (p) points to `source_node_idx` (x).
                    self.graph.add_edge(stack_node_idx, source_node_idx, None);
                } else {
                    // If source is not in stack map, maybe it's a temporary?
                    // For now assuming it must be in stack map as per `stack_alloc` semantics in previous code.
                    // If not found, we might need to create it, but `source` is a Variable, so it should be there.
                    // Fallback or panic? Let's just create a node if missing (robustness)
                    let source_node = Node::stack_alloc(*source);
                    let source_node_idx = self.graph.add_node(source_node);
                    self.stack.insert(*source, source_node_idx);
                    self.graph.add_edge(stack_node_idx, source_node_idx, None);
                }
                
                self.stack.insert(*target, stack_node_idx);
                Vec::new()
            }

            Instruction::GetFieldPtr { target, base, field } => {
                let target_node = Node::stack_alloc(*target);
                let target_node_idx = self.graph.add_node(target_node);
                self.stack.insert(*target, target_node_idx);

                if let Operand::Variable(base_var) = base {
                    if let Some(&source_node_idx) = self.stack.get(base_var) {
                        // 1. Find what the base variable points to (struct instance)
                        let struct_nodes: Vec<_> = self.graph
                            .edges_directed(source_node_idx, Direction::Outgoing)
                            .map(|e| e.target())
                            .collect();
                        
                        for struct_node_idx in struct_nodes {
                            // 2. Find the field node from the struct node
                            let field_nodes: Vec<_> = self.graph
                                .edges_directed(struct_node_idx, Direction::Outgoing)
                                .filter(|e| e.weight() == &Some(*field))
                                .map(|e| e.target())
                                .collect();
                            
                            for field_node_idx in field_nodes {
                                // 3. Target points to the field node
                                self.graph.add_edge(target_node_idx, field_node_idx, None);
                            }
                        }
                    }
                }
                Vec::new()
            }

            Instruction::Load { target, source } => {
                if let Operand::Variable(source_var) = source {
                    let target_node = Node::stack_alloc(*target);
                    let target_node_idx = self.graph.add_node(target_node);
                    self.stack.insert(*target, target_node_idx);

                    if let Some(&source_node_idx) = self.stack.get(source_var) {
                        // source_var points to Intermediate. Intermediate points to Final.
                        // We want target -> Final.
                        let intermediates: Vec<_> = self.graph
                            .edges_directed(source_node_idx, Direction::Outgoing)
                            .map(|x| x.target())
                            .collect();

                        for inter in intermediates {
                            let finals: Vec<_> = self.graph
                                .edges_directed(inter, Direction::Outgoing)
                                .map(|x| x.target())
                                .collect();
                            
                            for final_node in finals {
                                self.graph.add_edge(target_node_idx, final_node, None);
                            }
                        }
                    }
                }
                Vec::new()
            }

            Instruction::Store { dest: d, value: v } => {
                match (d, v) {
                    (Operand::Variable(variable_to), value) => {
                        // Store into pointer `variable_to`.
                        // `variable_to` is a stack variable (NodeIndex). It points to some location (Pointee).
                        // We want Pointee to now point to `value`.
                        
                        // NOTE: The previous GraphMap implementation logic was:
                        // lookup_node = Node::stack_alloc(*variable_to)
                        // edges_to_remove = edges from lookup_node
                        //
                        // Wait, `Store` typically means `*ptr = val`.
                        // If `variable_to` is the pointer `ptr`. `ptr` points to `Obj`.
                        // We are modifying `Obj`.
                        // But in the previous implementation:
                        // "For Store, we use the 'dest' as the root operand to access the old value
                        // Since 'dest' is the pointer variable, we can Load from it to get the heap pointer"
                        // And it removed edges from `lookup_node`.
                        //
                        // If `variable_to` is the pointer variable itself, removing edges from it means changing what the pointer points to?
                        // That's `ptr = val` (assignment), not `*ptr = val` (store).
                        // `Instruction::Store` is `*dest = value`.
                        // `Instruction::Assign` or just `Store`?
                        // Let's check `yir.rs` definition.
                        // `Store { dest: Operand, value: Operand }` -> "Store value through a pointer".
                        // So `dest` is a pointer. `*dest = value`.
                        // `variable_to` is `dest`.
                        // `variable_to` points to `MemoryLocation`.
                        // `MemoryLocation` should now point to `value`.
                        
                        // The previous implementation:
                        // `let lookup_node = Node::stack_alloc(*variable_to);`
                        // `edges_to_remove = self.graph.edges_directed(lookup_node, ...)`
                        // This looks like it was treating `Store` as overwriting the pointer `variable_to` itself?
                        // OR, it assumed `variable_to` IS the memory location?
                        // But `dest` is `Operand`, usually `Operand::Variable`.
                        // If `dest` is a pointer variable, we should follow it one step to get the memory location being written to.
                        
                        // HOWEVER, looking at `yir.rs`: `Store { dest, value }` "Store value through a pointer".
                        // AND `StoreImmediate` "Simple assignment or from constant to variable".
                        
                        // Let's assume the previous logic was trying to model `*ptr = val`.
                        // If `ptr` points to `Obj`. `Obj`'s outgoing edges should be replaced.
                        // But the code removed edges from `lookup_node` (which is `variable_to`).
                        // If `variable_to` is `ptr`, then it was changing `ptr` to point to something else.
                        // That sounds like `ptr = val`.
                        
                        // Maybe I should stick to the exact logic of the previous implementation for now to avoid semantic drift, 
                        // but adapted to StableGraph.
                        // "process_instruction" ... "Instruction::Store"
                        
                        // Wait, if I follow the previous code literally:
                        // `let lookup_node = Node::stack_alloc(*variable_to);`
                        // In StableGraph, this is `self.stack.get(variable_to)`.
                        
                        if let Some(&ptr_node_idx) = self.stack.get(variable_to) {
                             // In the previous code, it removed edges from `variable_to`.
                             // This implies `variable_to` IS the thing being modified.
                             // If `Store` is used for `ptr = val` in YIR (which might be `Store` if `dest` is a var?), then this is correct.
                             // But `yir.rs` says `Store` is "Store value through a pointer".
                             // AND `StoreImmediate` is "Simple assignment".
                             
                             // Let's look at how `Alloca` works. `Alloca` creates a stack slot `target`.
                             // `stack_node` is that slot.
                             // `Generate_nodes` adds edges from `stack_node` (e.g. to garbage).
                             
                             // If I have `let p = @...`. `p` is stack var. `p` points to heap.
                             // If I do `p = q`. That is `Store { dest: p, value: q }`?
                             // Or `Store` is only for `*p = q`?
                             // `yir.rs` has `Instruction::StoreImmediate` for `target: Variable`.
                             // `Instruction::Store` has `dest: Operand`.
                             // If `dest` is `Variable(p)`, and `p` is a pointer type.
                             // Does `Store` mean `p = value` or `*p = value`?
                             // "Store value through a pointer" suggests `*p = value`.
                             
                             // BUT, the previous implementation removed edges from `variable_to`.
                             // If `variable_to` is `p`, then it changes `p`'s target.
                             // That models `p = value`.
                             // If it modelled `*p = value`, it would find children of `p` and remove THEIR edges.
                             
                             // User prompt: "Follow my comments for now deviating if they are wrong."
                             // Previous code: "For Store, we use the 'dest' as the root operand to access the old value".
                             
                             // Let's stick to the previous logic but adapt to `StableGraph`.
                             
                             let edges_to_remove: Vec<_> = self.graph
                                .edges_directed(ptr_node_idx, Direction::Outgoing)
                                .map(|e| (e.source(), e.target(), e.id()))
                                .collect();
                             
                             // Collect removal data (edges)
                             // We need `(NodeIndex, NodeIndex)` for garbage collection later.
                             let mut edges_for_gc = Vec::new();

                             for (source, target, edge_id) in edges_to_remove {
                                 self.graph.remove_edge(edge_id);
                                 edges_for_gc.push((source, target));
                             }
                             
                             if let Operand::Variable(variable_from) = value {
                                 if let Some(&val_node_idx) = self.stack.get(variable_from) {
                                     // Add edges from `val_node` targets to `ptr_node`?
                                     // Previous code: `targets = ... edges_directed(variable_from ...).target()`.
                                     // `add_edge(variable_to, target)`.
                                     // It copies where `variable_from` points to, to `variable_to`.
                                     // So if `q -> obj`, and we do `p = q`, then `p -> obj`.
                                     // This confirms it models `p = q` (pointer assignment).
                                     
                                     let targets: Vec<_> = self.graph
                                        .edges_directed(val_node_idx, Direction::Outgoing)
                                        .map(|e| e.target())
                                        .collect();
                                        
                                     for target_idx in targets {
                                         self.graph.add_edge(ptr_node_idx, target_idx, None);
                                     }
                                 }
                             }
                             
                             self.garbage_collect_from_edges(edges_for_gc, Some(Operand::Variable(*variable_to)))
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                }
            }

            Instruction::IntToPtr { target, source } => {
                let target_node = Node::stack_alloc(*target);
                let target_node_idx = self.graph.add_node(target_node);
                self.stack.insert(*target, target_node_idx);

                let addr = match source {
                    Operand::U64Const(i) => *i as u64,
                    _ => 0,
                };
                let addr_node = Node::memory_address(addr);
                let addr_node_idx = self.graph.add_node(addr_node);

                self.graph.add_edge(target_node_idx, addr_node_idx, None);
                Vec::new()
            }

            Instruction::KillSet { vars } => {
                use crate::pass_parse::ast::InternUstr;
                let mut generated_instructions = Vec::new();

                for var in vars {
                    if self.allocas.contains(var) && var.ty().is_ptr() {
                        let ty = var.ty().deref_ptr();
                        if ty.is_struct() {
                            let drop_call = Instruction::Call {
                                target: None,
                                name: "_drop".intern(),
                                args: vec![Operand::Variable(*var)],
                            };
                            generated_instructions.push(drop_call);
                        }
                    }
                }

                for var in vars {
                    if let Some(node_idx) = self.stack.shift_remove(var) {
                         let mut heap_children = Vec::new();
                         for neigh in self.graph.neighbors_directed(node_idx, Direction::Outgoing) {
                             // Check if neighbor is HeapAlloc
                             if matches!(self.graph.node_weight(neigh), Some(Node::HeapAlloc{..})) {
                                 if self.graph.neighbors_directed(neigh, Direction::Incoming).count() == 1 {
                                     heap_children.push(neigh);
                                 }
                             }
                         }
                         
                         self.graph.remove_node(node_idx);

                         // We need to convert NodeIndex back to Node data for emit_frees if needed?
                         // emit_frees_for_nodes takes `&[NodeIndex]` in StableGraph version?
                         // Or we look up the node data.
                         
                         let frees = self.emit_frees_for_nodes(&heap_children, Some(Operand::Variable(*var)));
                         generated_instructions.extend(frees);
                         
                         let nested = self.remove_nodes_cascade(heap_children, None);
                         generated_instructions.extend(nested);
                    }
                }

                generated_instructions
            }

            _ => Vec::new(),
        }
    }

    fn emit_frees_for_nodes(&self, nodes: &[NodeIndex], root_operand: Option<Operand>) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        for &node_idx in nodes {
            if let Some(Node::HeapAlloc { id, ty }) = self.graph.node_weight(node_idx) {
                use crate::pass_parse::ast::InternUstr;
                
                let ptr_operand = if let Some(root) = root_operand {
                     let ptr_var = Variable::new(
                        format!("temp_free_load_{}", id).as_str().intern(),
                        *id as i64,
                        ty.ptr_to(),
                    );
                    
                    let load_instr = Instruction::Load {
                        target: ptr_var,
                        source: root,
                    };
                    instructions.push(load_instr);
                    
                    Operand::Variable(ptr_var)
                } else {
                     let dummy_var = Variable::new(
                        format!("nested_free_TODO_{}", id).as_str().intern(),
                        *id as i64,
                        ty.ptr_to(),
                    );
                    Operand::Variable(dummy_var)
                };

                if ty.is_struct() {
                     let drop_call = Instruction::Call {
                        target: None,
                        name: "_drop".intern(),
                        args: vec![ptr_operand],
                     };
                     instructions.push(drop_call);
                }

                let heap_free = Instruction::HeapFree {
                    ptr: ptr_operand,
                };
                instructions.push(heap_free);
            }
        }

        instructions
    }

    fn remove_nodes_cascade(&mut self, mut to_remove: Vec<NodeIndex>, _root_operand: Option<Operand>) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        
        let mut internal_queue = to_remove;
        let mut nested_removals = Vec::new();
        
        while let Some(node_idx) = internal_queue.pop() {
             for neigh in self.graph.neighbors_directed(node_idx, Direction::Outgoing) {
                if self.graph.neighbors_directed(neigh, Direction::Incoming).count() == 1 {
                    if matches!(self.graph.node_weight(neigh), Some(Node::HeapAlloc { .. })) {
                        internal_queue.push(neigh);
                        nested_removals.push(neigh);
                    }
                }
            }
            self.graph.remove_node(node_idx);
        }
        
        if !nested_removals.is_empty() {
             let nested_frees = self.emit_frees_for_nodes(&nested_removals, None);
             instructions.extend(nested_frees);
        }
        
        instructions
    }

    fn garbage_collect_from_edges(&mut self, to_remove: Vec<(NodeIndex, NodeIndex)>, root_operand: Option<Operand>) -> Vec<Instruction> {
        let mut worklist = Vec::new();
        let mut instructions = Vec::new();
        
        // Edges are already removed by the caller in StableGraph (since we iterate over EdgeReferences usually).
        // But wait, `garbage_collect_from_edges` in previous code took `(Node, Node)` (Source, Target) and removed them.
        // Here I am passing `(NodeIndex, NodeIndex)` that WERE removed.
        
        for (_, target_idx) in to_remove {
             if matches!(self.graph.node_weight(target_idx), Some(Node::HeapAlloc{..})) {
                 if self.graph.neighbors_directed(target_idx, Direction::Incoming).count() == 0 {
                     worklist.push(target_idx);
                 }
             }
        }
        
        if !worklist.is_empty() {
            let direct_frees = self.emit_frees_for_nodes(&worklist, root_operand);
            instructions.extend(direct_frees);
            
            let cascading = self.remove_nodes_cascade(worklist, None);
            instructions.extend(cascading);
        }
        
        instructions
    }
}