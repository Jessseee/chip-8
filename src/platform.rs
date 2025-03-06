use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use serde::{Deserialize, Serialize};

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

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all="camelCase")]
pub struct Platform {
    pub id: PlatformId,
    pub name: String,
    pub description: String,
    pub display_resolutions: Vec<String>,
    pub default_tickrate: u64,
    pub quirks: HashMap<Quirk, bool>
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

    pub fn resolutions(&self) -> Vec<(u32, u32)> {
        self.display_resolutions.iter().map(|resolution| {
            let resolution: Vec<u32> = resolution.splitn(2, "x").flat_map(|i| i.parse()).collect();
            let width = resolution[0];
            let height = resolution[1];
            (width, height)
        }).collect()
    }
}
