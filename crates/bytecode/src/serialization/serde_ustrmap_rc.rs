use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::sync::Arc;
use ustr::{Ustr, UstrMap};

pub fn serialize<S, V>(map: &UstrMap<Arc<V>>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    V: Serialize,
{
    map.iter()
        .map(|(k, v)| (*k, v.as_ref()))
        .collect::<Vec<_>>()
        .serialize(serializer)
}

pub fn deserialize<'de, D, V>(deserializer: D) -> Result<UstrMap<Arc<V>>, D::Error>
where
    D: Deserializer<'de>,
    V: Deserialize<'de>,
{
    let entries: Vec<(Ustr, V)> = Vec::deserialize(deserializer)?;
    let mut map = UstrMap::default();
    map.extend(entries.into_iter().map(|(k, v)| (k, Arc::new(v))));
    Ok(map)
}
