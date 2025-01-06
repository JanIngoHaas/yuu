use std::any::Any;

use hashbrown::HashMap;
use petgraph::{graph::DiGraph, prelude::GraphMap, Directed};
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};

use crate::context::Context;

pub trait Pass: Send + Sync {
    fn run(&self, context: &mut Context) -> anyhow::Result<()>;
    fn install(self, schedule: &mut Schedule)
    where
        Self: Sized;
    fn get_name(&self) -> &'static str;
}

pub struct Scheduler;
pub type PassId = &'static str;
pub type ResourceName = &'static str;

pub trait ResourceId: Any + Send + 'static {
    fn resource_name() -> ResourceName;
}

pub struct Schedule {
    passes: HashMap<PassId, Box<dyn Pass>>,
    read_resources: HashMap<PassId, Vec<ResourceName>>,
    write_resources: HashMap<PassId, Vec<ResourceName>>,
    resource_outputs: HashMap<PassId, Vec<ResourceName>>,
}

impl Schedule {
    pub fn new() -> Self {
        Self {
            passes: HashMap::new(),
            read_resources: HashMap::new(),
            write_resources: HashMap::new(),
            resource_outputs: HashMap::new(),
        }
    }

    pub fn add_pass<T: Pass + 'static>(&mut self, pass: T) {
        self.passes.insert(pass.get_name(), Box::new(pass));
    }

    pub fn requires_resource_read<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.read_resources
            .entry(pass.get_name())
            .or_insert(Vec::new())
            .push(R::resource_name());
    }

    pub fn requires_resource_write<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.write_resources
            .entry(pass.get_name())
            .or_insert(Vec::new())
            .push(R::resource_name());
    }

    pub fn produces_resource<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.resource_outputs
            .entry(pass.get_name())
            .or_insert(Vec::new())
            .push(R::resource_name());
    }
}

impl Scheduler {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, schedule: Schedule) -> Result<(), String> {
        // Toposort the passes using the required_resource_inputs and required_resource_outputs
        // We also want to run passes that don't depend on other passes in parallel

        let mut graph = GraphMap::<PassId, Vec<ResourceName>, Directed>::new();

        for (pass_a_name, pass_a) in &schedule.passes {
            // what does 'pass' produce as output?
            let outputs_pass_a = schedule.resource_outputs.get(pass_a_name);

            // Add the pass to the graph
            graph.add_node(pass_a_name);

            // Resolve dependencies

            // If _A_ has no outputs, we can skip the rest of the loop
            if outputs_pass_a
                .and_then(|outputs| Some(outputs.is_empty()))
                .unwrap_or(true)
            {
                continue;
            }

            let outputs_pass_a = outputs_pass_a.unwrap();

            for (pass_b_name, pass_b) in &schedule.passes {
                if pass_a_name == pass_b_name {
                    continue;
                }

                let mut weights = Vec::new();

                // Check read-after-write dependencies (B reads what A writes)
                let writes_pass_a = schedule
                    .write_resources
                    .get(pass_a_name)
                    .unwrap_or(&Vec::new())
                    .to_vec();
                let reads_pass_b = schedule
                    .read_resources
                    .get(pass_b_name)
                    .unwrap_or(&Vec::new())
                    .to_vec();

                for write_a in &writes_pass_a {
                    for read_b in &reads_pass_b {
                        if write_a == read_b {
                            weights.push(*write_a);
                        }
                    }
                }

                // Check write-after-read dependencies (B writes what A reads)
                let reads_pass_a = schedule
                    .read_resources
                    .get(pass_a_name)
                    .unwrap_or(&Vec::new())
                    .to_vec();
                let writes_pass_b = schedule
                    .write_resources
                    .get(pass_b_name)
                    .unwrap_or(&Vec::new())
                    .to_vec();

                for read_a in &reads_pass_a {
                    for write_b in &writes_pass_b {
                        if read_a == write_b {
                            weights.push(*read_a);
                        }
                    }
                }

                // Check write-after-write dependencies
                for write_a in &writes_pass_a {
                    for write_b in &writes_pass_b {
                        if write_a == write_b {
                            weights.push(*write_a);
                        }
                    }
                }

                // Original output dependencies
                let outputs_pass_a = schedule.resource_outputs.get(pass_a_name);
                if let Some(outputs_pass_a) = outputs_pass_a {
                    let inputs_passes_b = schedule
                        .read_resources
                        .get(pass_b_name)
                        .unwrap_or(&Vec::new())
                        .to_vec();
                    for output_pass_a in outputs_pass_a {
                        for input_pass_b in &inputs_passes_b {
                            if output_pass_a == input_pass_b {
                                weights.push(*output_pass_a);
                            }
                        }
                    }
                }

                if !weights.is_empty() {
                    graph.add_edge(pass_a_name, pass_b_name, weights);
                }
            }

            // We now have a graph with the passes and their dependencies
            // We can now toposort the graph
            let topo_order = petgraph::algo::toposort(&graph, None);

            match topo_order {
                Ok(order) => {
                    // Group passes that can be run in parallel (those at the same depth)
                    let mut layers: Vec<Vec<PassId>> = Vec::new();
                    let mut visited: HashMap<PassId, usize> = HashMap::new();

                    // Calculate the layer for each pass
                    for &pass_id in &order {
                        let max_dep_layer = graph
                            .neighbors_directed(pass_id, petgraph::Direction::Incoming)
                            .map(|dep| visited[&dep] + 1)
                            .max()
                            .unwrap_or(0);

                        visited.insert(pass_id, max_dep_layer);

                        // Ensure we have enough layers
                        while layers.len() <= max_dep_layer {
                            layers.push(Vec::new());
                        }
                        let layer = &mut layers[max_dep_layer];
                        layer.push(pass_id);
                    }

                    let mut overall_context = Context::new();
                    // Execute passes layer by layer
                    for layer in layers {
                        // Execute all passes in the current layer in parallel using the global thread pool
                        let layer_result: Result<(), String> = layer
                            .into_par_iter()
                            .map(|pass_id| {
                                let mut splitted_context = overall_context.split();
                                let pass = schedule.passes.get(pass_id).expect("Pass not found");
                                let run_result = pass
                                    .run(&mut splitted_context)
                                    .map_err(|e| format!("Pass '{}' failed: {}", pass_id, e));
                                run_result.map(|_| splitted_context)
                            })
                            .collect::<Result<Vec<_>, String>>()
                            .map(|contexts| {
                                for ctx in contexts {
                                    overall_context.merge(ctx);
                                }
                            });
                        layer_result?;
                    }
                }
                Err(e) => {
                    return Err(format!(
                        "Cannot run schedule because it contains a cycle: {:?}",
                        e
                    ))
                }
            }
        }
        Ok(())
    }
}
