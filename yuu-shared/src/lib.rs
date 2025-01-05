pub mod ast;
pub mod binding_info;
pub mod graphviz_output;
pub mod token;
pub mod type_info;
pub mod yir;
pub type Span = logos::Span;
pub type Range = Span;
pub mod block;
pub mod print_yir;
pub mod semantic_error;

use std::any::Any;
use std::ops::Deref;
use std::{cell::RefCell, rc::Rc};

use ast::Node;
use hashbrown::HashMap;

pub struct Context {
    passes_data: HashMap<String, Rc<dyn Any>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            passes_data: HashMap::new(),
        }
    }

    pub fn add_pass_data<T: 'static>(&mut self, pass_data: T) -> bool {
        let key = std::any::type_name::<T>().to_string();
        if self.passes_data.contains_key(&key) {
            return false;
        }
        self.passes_data
            .insert(key, Rc::new(RefCell::new(pass_data)));
        true
    }

    pub fn get_pass_data<T: 'static>(&self) -> Option<Rc<RefCell<T>>> {
        let key = std::any::type_name::<T>().to_string();
        self.passes_data.get(&key).map(|rc| {
            rc.clone()
                .downcast::<RefCell<T>>()
                .expect("Type mismatch in pass data")
        })
    }

    pub fn replace_pass_data<T: 'static>(&mut self, pass_data: T) {
        self.passes_data.insert(
            std::any::type_name::<T>().to_string(),
            Rc::new(RefCell::new(pass_data)),
        );
    }

    pub fn require_pass_data<T: 'static>(&self, pass_name: &'static str) -> Rc<RefCell<T>> {
        let key = std::any::type_name::<T>();
        self.passes_data
            .get(key)
            .map(|rc| {
                rc.clone()
                    .downcast::<RefCell<T>>()
                    .expect("Type mismatch in pass data")
            })
            .unwrap_or_else(|| {
                panic!(
                    "Pass {} requires pass data of type {}",
                    pass_name,
                    std::any::type_name::<T>()
                )
            })
    }
}

pub struct Pipeline {
    passes: Vec<Box<dyn Pass>>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass<T: Pass + 'static>(&mut self, pass: T) {
        self.passes.push(Box::new(pass));
    }

    pub fn run(self, context: &mut Context) {
        for mut pass in self.passes.into_iter() {
            if !pass.run(context) {
                break;
            }
        }
    }
}

pub trait Pass {
    fn run(&mut self, context: &mut Context) -> bool;
    fn install(self, pipeline: &mut Pipeline)
    where
        Self: Sized;
}
