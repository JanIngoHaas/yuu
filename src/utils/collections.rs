use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;
use ustr::{IdentityHasher, Ustr};

pub type FastHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub type UstrHashMap<V> = std::collections::HashMap<Ustr, V, BuildHasherDefault<IdentityHasher>>;
pub type UstrIndexMap<V> = indexmap::IndexMap<Ustr, V, BuildHasherDefault<IdentityHasher>>;
pub type UstrIndexSet = indexmap::IndexSet<Ustr, BuildHasherDefault<IdentityHasher>>;
