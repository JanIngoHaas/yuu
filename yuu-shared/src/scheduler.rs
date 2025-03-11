use std::any::Any;

use anyhow::{anyhow, bail};
use hashbrown::HashMap;
use petgraph::{
    dot::{Config, Dot},
    prelude::GraphMap,
    Directed,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::context::Context;

pub trait Pass: Send + Sync {
    fn run(&self, context: &mut Context) -> anyhow::Result<()>;
    fn install(self, schedule: &mut Schedule)
    where
        Self: Sized;
    fn get_name(&self) -> &'static str;
}

#[derive(Default)]
pub struct Scheduler;

pub type PassId = &'static str;
pub type ResourceName = &'static str;
pub type PassGraph = GraphMap<PassId, Vec<ResourceName>, Directed>;

pub trait ResourceId: Any + Send + 'static {
    fn resource_name() -> ResourceName;
}

#[derive(Default)]
pub struct Schedule {
    passes: HashMap<PassId, Box<dyn Pass>>,
    read_resources: HashMap<PassId, Vec<ResourceName>>,
    write_resources: HashMap<PassId, Vec<ResourceName>>,
    resource_outputs: HashMap<PassId, Vec<ResourceName>>,
}

impl Schedule {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_pass<T: Pass + 'static>(&mut self, pass: T) {
        self.passes.insert(pass.get_name(), Box::new(pass));
    }

    pub fn requires_resource_read<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.read_resources
            .entry(pass.get_name())
            .or_default()
            .push(R::resource_name());
    }

    pub fn requires_resource_write<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.write_resources
            .entry(pass.get_name())
            .or_default()
            .push(R::resource_name());
    }

    pub fn produces_resource<R: ResourceId + 'static>(&mut self, pass: &impl Pass) {
        self.resource_outputs
            .entry(pass.get_name())
            .or_default()
            .push(R::resource_name());
    }

    pub fn build_dependency_graph(&self) -> PassGraph {
        let mut graph = PassGraph::new();

        // Add all passes as nodes
        for pass_name in self.passes.keys() {
            graph.add_node(pass_name);
        }

        // Track the latest writer (producer or writer) for each resource
        let mut latest_writer: HashMap<ResourceName, PassId> = HashMap::new();

        // First, handle producers as they are the initial writers
        for (producer, produces) in &self.resource_outputs {
            for resource in produces {
                latest_writer.insert(*resource, producer);
            }
        }

        // Then handle writers - they must come after the producer/previous writer
        for (writer, writes) in &self.write_resources {
            for resource in writes {
                if let Some(last_writer) = latest_writer.get(resource) {
                    if last_writer != writer {
                        // Add dependency from last writer to this writer
                        let mut edge_resources = graph
                            .edge_weight_mut(last_writer, writer)
                            .unwrap_or(&mut Vec::new())
                            .clone();
                        edge_resources.push(*resource);
                        graph.add_edge(last_writer, writer, edge_resources);
                    }
                }
                // Update the latest writer
                latest_writer.insert(*resource, writer);
            }
        }

        // Finally handle readers - they must read after the latest writer
        for (reader, reads) in &self.read_resources {
            for resource in reads {
                if let Some(writer) = latest_writer.get(resource) {
                    if writer != reader {
                        // Add dependency from writer to reader
                        let mut edge_resources = graph
                            .edge_weight_mut(writer, reader)
                            .unwrap_or(&mut Vec::new())
                            .clone();
                        edge_resources.push(*resource);
                        graph.add_edge(writer, reader, edge_resources);
                    }
                }
            }
        }

        graph
    }

    pub fn print_dot(&self) {
        let graph = self.build_dependency_graph();

        // Debug prints
        println!("// Debug info:");
        println!("// Reads:");
        for (pass, reads) in &self.read_resources {
            println!("//   {}: {:?}", pass, reads);
        }
        println!("// Writes:");
        for (pass, writes) in &self.write_resources {
            println!("//   {}: {:?}", pass, writes);
        }
        println!("// Produces:");
        for (pass, produces) in &self.resource_outputs {
            println!("//   {}: {:?}", pass, produces);
        }
        println!();

        println!("digraph PassDependencies {{");
        println!("    // Graph attributes");
        println!("    node [shape=box, style=filled, fillcolor=lightgray];"); // Box nodes with gray fill
        println!("    edge [color=blue];"); // Blue edges
        println!("    rankdir=LR;"); // Left to right layout

        // Print the graph content
        print!("    "); // Indent the content
        println!(
            "{:?}",
            Dot::with_config(&graph, &[Config::GraphContentOnly,])
        );

        println!("}}");
    }
}

impl Scheduler {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, schedule: Schedule, mut context: Context) -> anyhow::Result<Context> {
        let graph = schedule.build_dependency_graph();

        // Topologically sort the graph
        let topo_order = petgraph::algo::toposort(&graph, None)
            .map_err(|e| anyhow!("Cannot run schedule because it contains a cycle: {:?}", e))?;

        // Group passes that can be run in parallel (those at the same depth)
        let mut layers: Vec<Vec<PassId>> = Vec::new();
        let mut visited: HashMap<PassId, usize> = HashMap::new();

        // Calculate the layer for each pass
        for &pass_id in &topo_order {
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

        for layer in layers {
            // Execute all passes in the current layer in parallel using the global thread pool
            let layer_result: anyhow::Result<()> = layer
                .into_par_iter()
                .map(|pass_id| {
                    let mut splitted_context = context.split();
                    let pass = schedule.passes.get(pass_id).expect("Pass not found");
                    let run_result = pass.run(&mut splitted_context);
                    match run_result {
                        Ok(_) => Ok(splitted_context),
                        Err(_) => Err(anyhow::anyhow!("Error in pass {}", pass_id)),
                    }
                })
                .collect::<anyhow::Result<Vec<_>>>()
                .map(|contexts| {
                    for ctx in contexts {
                        context.merge(ctx);
                    }
                });
            //panic!("Implement parallel execution");
            layer_result?;
        }

        Ok(context)
    }
}
