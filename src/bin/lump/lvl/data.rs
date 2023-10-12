use lu_formats::files::lvl::{EditorSettings, LightingInfo, ObjectInfo, Particle, SkydomeInfo};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Environment<'a> {
    pub lighting_info: LightingInfo,
    pub skydome_info: SkydomeInfo<'a>,
    pub editor_settings: EditorSettings,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Level<'a> {
    pub version: u32,
    pub revision: u32,
    pub environment: Option<Environment<'a>>,
    pub objects: Option<Vec<ObjectInfo<'a>>>,
    pub particles: Option<Vec<Particle<'a>>>,
}
