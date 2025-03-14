use std::{
    any::Any,
    sync::{Arc, Mutex},
};

use indexmap::IndexMap;

use crate::scheduler::{Pass, ResourceId};

pub struct Context {
    passes_data: IndexMap<&'static str, Arc<dyn Any + Send + Sync + 'static>>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            passes_data: IndexMap::new(),
        }
    }

    pub fn split(&self) -> Self {
        Self {
            passes_data: self.passes_data.clone(),
        }
    }

    pub fn merge(&mut self, other: Self) {
        self.passes_data.extend(other.passes_data);
    }

    pub fn add_pass_data<T: ResourceId>(&mut self, pass_data: T) -> bool {
        if self.passes_data.contains_key(T::resource_name()) {
            return false;
        }
        self.passes_data
            .insert(T::resource_name(), Arc::new(Mutex::new(pass_data)));
        true
    }

    pub fn replace_pass_data<T: ResourceId>(&mut self, pass_data: T) {
        self.passes_data
            .insert(T::resource_name(), Arc::new(Mutex::new(pass_data)));
    }

    pub fn get_resource<T: ResourceId>(&self, pass: &impl Pass) -> Arc<Mutex<T>> {
        self.passes_data
            .get(T::resource_name())
            .map(|arc| {
                arc.clone().downcast::<Mutex<T>>().unwrap_or_else(|_| {
                    panic!("Type mismatch in pass data of pass {}", pass.get_name())
                })
            })
            .unwrap_or_else(|| {
                panic!(
                    "Pass {} requires pass data of type {}",
                    pass.get_name(),
                    T::resource_name()
                )
            })
    }
}
