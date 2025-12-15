use std::hash::BuildHasherDefault;
use rustc_hash::FxHasher;
use ustr::{IdentityHasher, Ustr};

pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub type IndexSet<T> = indexmap::IndexSet<T, BuildHasherDefault<FxHasher>>;

// For Ustr-keyed collections that benefit from IdentityHasher
pub type UstrIndexMap<V> = indexmap::IndexMap<Ustr, V, BuildHasherDefault<IdentityHasher>>;
pub type UstrIndexSet = indexmap::IndexSet<Ustr, BuildHasherDefault<IdentityHasher>>;

// For lookup-heavy collections that don't need insertion order
pub type FastHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;
