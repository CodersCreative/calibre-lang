use rustc_hash::FxHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::hash::Hash;

pub fn serialize<S, K, V>(map: &FxHashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    K: Serialize,
    V: Serialize,
{
    map.iter().collect::<Vec<_>>().serialize(serializer)
}

pub fn deserialize<'de, D, K, V>(deserializer: D) -> Result<FxHashMap<K, V>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    let entries: Vec<(K, V)> = Vec::deserialize(deserializer)?;
    let mut map = FxHashMap::default();
    map.extend(entries);
    Ok(map)
}
