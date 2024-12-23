pub mod ast;
pub mod graphviz_output;
pub mod token;
use ast::Node;
use hashbrown::HashMap;

pub struct Context {
    passes_data: HashMap<String, Box<dyn std::any::Any>>,
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
        self.passes_data.insert(key, Box::new(pass_data));
        true
    }

    pub fn get_pass_data<T: 'static>(&self) -> Option<&T> {
        self.passes_data
            .get(std::any::type_name::<T>())
            .map(|x| x.downcast_ref().unwrap())
    }

    pub fn replace_pass_data<T: 'static>(&mut self, pass_data: T) {
        self.passes_data
            .insert(std::any::type_name::<T>().to_string(), Box::new(pass_data));
    }

    pub fn require_pass_data<T: 'static>(&self, pass_name: &'static str) -> &T {
        self.passes_data
            .get(std::any::type_name::<T>())
            .map(|x| x.downcast_ref().unwrap())
            .expect(
                format!(
                    "Pass {} requires pass data of type {}",
                    pass_name,
                    std::any::type_name::<T>()
                )
                .as_str(),
            )
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
