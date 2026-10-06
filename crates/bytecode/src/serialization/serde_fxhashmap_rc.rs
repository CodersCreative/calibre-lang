use rustc_hash::FxHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{hash::Hash, sync::Arc};

pub fn serialize<S, K, V>(map: &FxHashMap<K, Arc<V>>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    K: Serialize + Clone,
    V: Serialize,
{
    map.iter()
        .map(|(k, v)| (k.clone(), v.as_ref()))
        .collect::<Vec<_>>()
        .serialize(serializer)
}

pub fn deserialize<'de, D, K, V>(deserializer: D) -> Result<FxHashMap<K, Arc<V>>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    let entries: Vec<(K, V)> = Vec::deserialize(deserializer)?;
    let mut map = FxHashMap::default();
    map.extend(entries.into_iter().map(|(k, v)| (k, Arc::new(v))));
    Ok(map)
}
