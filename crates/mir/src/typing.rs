use crate::environment::MiddleEnvironment;
use calibre_parser::ast::{Node, ObjectMap, ObjectType, ParserDataType, ParserText, TypeDefType};

pub struct Typing {
    pub objects: FxHashMap<String, MiddleObject>,
    pub impls: FxHashMap<ParserInnerType, MiddleImpl>,
    pub type_aliases: FxHashMap<String, ParserDataType>,
    pub trait_defs: FxHashMap<String, MiddleTrait>,
    pub generic_type_templates: FxHashMap<String, (Vec<String>, TypeDefType, Vec<Overload>)>,
    pub type_specializations: FxHashMap<String, String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: FxHashMap<String, (String, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImpl {
    pub data_type: ParserDataType,
    pub generic_params: Vec<String>,
    pub variables: FxHashMap<String, (String, bool)>,
    pub traits: Vec<String>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleTraitMember {
    pub data_type: ParserDataType,
    pub default: Option<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleTrait {
    pub implied_traits: Vec<String>,
    pub members: FxHashMap<String, MiddleTraitMember>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MiddleTypeDefType {
    Enum {
        variants: Vec<(ParserText, Option<ParserDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<Node>>,
    },
    Struct(ObjectMap<(ParserDataType, Option<Box<Node>>)>),
    NewType(ParserDataType),
    Trait,
}

impl MiddleTypeDefType {
    pub fn from_type_def_type(
        env: &mut MiddleEnvironment,
        scope: &u64,
        value: TypeDefType,
    ) -> Self {
        match value {
            TypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => MiddleTypeDefType::Enum {
                variants: {
                    let mut lst = Vec::new();

                    for (k, v) in variants {
                        lst.push((
                            env.resolve_dollar_ident_only(scope, &k)
                                .unwrap_or_else(|| ParserText::from(k.to_string()).into()),
                            if let Some(v) = v {
                                Some(env.resolve_potential_new_type(scope, v))
                            } else {
                                None
                            },
                        ));
                    }
                    lst
                },
                default_variant,
                default_value,
            },
            TypeDefType::Struct { fields } => MiddleTypeDefType::Struct({
                let mut map = Vec::new();

                match fields {
                    ObjectType::Map(field_map) => {
                        for (k, (t, v)) in field_map {
                            let resolved_type = env.resolve_potential_new_type(scope, t);
                            map.push((k, (resolved_type, v.map(Box::new))));
                        }
                    }
                    ObjectType::Tuple(types) => {
                        for (t, v) in types {
                            let resolved_type = env.resolve_potential_new_type(scope, t);
                            map.push((format!("{}", map.len()), (resolved_type, v.map(Box::new))));
                        }
                    }
                }

                ObjectMap(map)
            }),
            TypeDefType::NewType(x) => {
                MiddleTypeDefType::NewType(env.resolve_potential_new_type(scope, *x))
            }
        }
    }
}
