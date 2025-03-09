use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(clap::ValueEnum, Clone, Default, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all="camelCase")]
pub enum PlatformId {
    #[default]
    OriginalChip8,
    HybridVIP,
    ModernChip8,
    Chip8x,
    Chip48,
    Superchip1,
    Superchip,
    Megachip8,
    Xochip,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all="camelCase")]
pub enum Quirk {
    Shift,
    MemoryIncrementByX,
    MemoryLeaveIUnchanged,
    Wrap,
    Jump,
    Vblank,
    Logic,
}

pub fn deserialize_display_resolutions<'de, D>(deserializer: D) -> Result<Vec<(u32, u32)>, D::Error>
where D: Deserializer<'de> {
    let buf: Vec<String> = Vec::deserialize(deserializer)?;
    let display_resolutions = buf.iter().map(|resolution| {
        let resolution: Vec<u32> = resolution
            .splitn(2, "x")
            .flat_map(|i| i.parse())
            .collect();
        let width = resolution[0];
        let height = resolution[1];
        (width, height)
    }).collect();
    Ok(display_resolutions)
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all="camelCase")]
pub struct Platform {
    pub id: PlatformId,
    pub name: String,
    pub description: String,
    #[serde(deserialize_with="deserialize_display_resolutions")]
    pub display_resolutions: Vec<(u32, u32)>,
    pub default_tickrate: u64,
    pub quirks: HashMap<Quirk, bool>,
}

impl Platform {
    pub fn from_id(platform_id: &PlatformId) -> Result<Self, Box<dyn Error>> {
        let file = include_str!("../chip-8-database/database/platforms.json");
        let platforms: Vec<Platform> = serde_json::from_str(file)?;
        let platform = platforms
            .iter()
            .find(|&platform| platform.id == *platform_id)
            .ok_or(format!("Failed to initialize platform {:?}", platform_id))?
            .clone();
        Ok(platform)
    }
}
