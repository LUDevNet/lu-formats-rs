use std::{collections::BTreeMap, fmt};

use serde::Deserialize;

use crate::Identifier;

/// An enum specification
#[derive(Deserialize, Debug)]
pub struct EnumSpec(pub BTreeMap<String, EnumValueSpec>);

/// An enum value specification
#[derive(Debug)]
pub struct EnumValueSpec {
    /// Identifier
    pub id: Identifier,
}

impl<'de> serde::Deserialize<'de> for EnumValueSpec {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct __Visitor;

        impl<'de> serde::de::Visitor<'de> for __Visitor {
            type Value = EnumValueSpec;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "string or object")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec {
                    id: Identifier(v.to_string()),
                })
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec { id: Identifier(v) })
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec {
                    id: Identifier(match v {
                        true => "true".to_string(),
                        false => "false".to_string(),
                    }),
                })
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut id = None;
                while let Some(key) = map.next_key::<&str>()? {
                    match key {
                        "id" => {
                            id = Some(map.next_value()?);
                        }
                        // FIXME: doc, doc-ref
                        _ => {
                            map.next_value::<serde::de::IgnoredAny>()?;
                        }
                    }
                }
                Ok(EnumValueSpec { id: id.unwrap() })
            }
        }

        deserializer.deserialize_any(__Visitor)
    }
}
