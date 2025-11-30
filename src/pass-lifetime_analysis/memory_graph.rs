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
        TypeInfo::Pointer(_) => {
            let garbage = Node::garbage_mem_addr();
            let garbage_idx = g.graph.add_node(garbage);
            g.graph.add_edge(node_idx, garbage_idx, None);
        }
        TypeInfo::Struct(struct_type) => {
            if let Some(struct_info) = type_registry.resolve_struct(struct_type.name) {
                for field in struct_info.fields.values() {
                    let field_node = Node::PartOfAlloc { id: g.heap_counter };
                    g.heap_counter += 1;
                    let field_idx = g.graph.add_node(field_node);

                    g.graph.add_edge(node_idx, field_idx, Some(field.name));
                    generate_nodes_for_type(field.ty, field_idx, g, type_registry);
                }
            }
        }
        _ => {}
    }
}

impl MemoryGraph {
    pub fn is_induced_isomorphic(&self, other: &MemoryGraph) -> bool {
        let mut visited = IndexSet::new();

        for (stack_var, &self_node) in &self.stack {
            if let Some(&other_node) = other.stack.get(stack_var) {
                if !self.is_induced_isomorphic_recursive(self_node, other, other_node, &mut visited) {
                    return false;
                }
            } else {
                return false;
            }
        }

        self.stack.len() == other.stack.len()
    }

    fn is_induced_isomorphic_recursive(
        &self,
        self_node: NodeIndex,
        other: &MemoryGraph,
        other_node: NodeIndex,
        visited: &mut IndexSet<(NodeIndex, NodeIndex)>,
    ) -> bool {
        let node_pair = (self_node, other_node);
        if visited.contains(&node_pair) {
            return true;
        }

        visited.insert(node_pair);

        if self.graph.node_weight(self_node) != other.graph.node_weight(other_node) {
            return false;
        }

        let self_edges: IndexMap<EdgeType, Vec<NodeIndex>> = self.graph
            .edges_directed(self_node, Direction::Outgoing)
            .fold(IndexMap::new(), |mut acc, edge| {
                acc.entry(*edge.weight()).or_default().push(edge.target());
                acc
            });

        let other_edges: IndexMap<EdgeType, Vec<NodeIndex>> = other.graph
            .edges_directed(other_node, Direction::Outgoing)
            .fold(IndexMap::new(), |mut acc, edge| {
                acc.entry(*edge.weight()).or_default().push(edge.target());
                acc
            });

        if self_edges.len() != other_edges.len() {
            return false;
        }

        for (edge_weight, self_targets) in &self_edges {
            if let Some(other_targets) = other_edges.get(edge_weight) {
                if self_targets.len() != other_targets.len() {
                    return false;
                }

                for (self_target, other_target) in self_targets.iter().zip(other_targets.iter()) {
                    if !self.is_induced_isomorphic_recursive(*self_target, other, *other_target, visited) {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }

        true
    }
    pub fn process_instruction(&mut self, instruction: &Instruction, type_registry: &TypeRegistry) -> Vec<Instruction> {
        match instruction {
            Instruction::Alloca { target } => {
                let node = Node::stack_alloc(*target);
                let stack_node_idx = self.graph.add_node(node);
                self.stack.insert(*target, stack_node_idx);
                self.allocas.insert(*target);

                generate_nodes_for_type(target.ty(), stack_node_idx, self, type_registry);
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
                    self.graph.add_edge(stack_node_idx, source_node_idx, None);
                } else {
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
                        let pointed_to: Vec<_> = self.graph
                            .edges_directed(source_node_idx, Direction::Outgoing)
                            .map(|x| x.target())
                            .collect();

                        for pointed_node in pointed_to {
                            self.graph.add_edge(target_node_idx, pointed_node, None);
                        }
                    }
                }
                Vec::new()
            }

            Instruction::Store { dest: d, value: v } => {
                match (d, v) {
                    (Operand::Variable(dest_var), value) => {
                        if let Some(&dest_node_idx) = self.stack.get(dest_var) {
                            let edges_to_remove: Vec<_> = self.graph
                                .edges_directed(dest_node_idx, Direction::Outgoing)
                                .map(|e| (e.source(), e.target(), e.id()))
                                .collect();

                            let mut edges_for_gc = Vec::new();
                            for (source, target, edge_id) in edges_to_remove {
                                self.graph.remove_edge(edge_id);
                                edges_for_gc.push((source, target));
                            }

                            if let Operand::Variable(value_var) = value {
                                if let Some(&val_node_idx) = self.stack.get(value_var) {
                                    let targets: Vec<_> = self.graph
                                       .edges_directed(val_node_idx, Direction::Outgoing)
                                       .map(|e| e.target())
                                       .collect();

                                    for target_idx in targets {
                                        self.graph.add_edge(dest_node_idx, target_idx, None);
                                    }
                                }
                            }

                            self.garbage_collect_from_edges(edges_for_gc, Some(Operand::Variable(*dest_var)))
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