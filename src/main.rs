mod screen;
mod keypad;
mod interpreter;
mod platform;
mod audio;

#[macro_use]
extern crate paris;

use clap::Parser;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use crate::platform::{Platform, PlatformId};
use crate::interpreter::Interpreter;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    #[arg(short, long)]
    #[clap(help="To beep or not to beep?")]
    pub quiet: bool,
    #[arg(short, long)]
    #[clap(default_value="12", help="Screen pixel scale.")]
    pub scale: f32,
    #[arg(long, short)]
    #[clap(default_value="original-chip8", help="CHIP-8 implementation.")]
    pub platform: PlatformId,
}

fn main() {
    // Parse command line arguments
    let args = Args::parse();
    let platform = Platform::from_id(&args.platform).unwrap();

    // Create flag to handle Ctrl-C gracefully
    let run_flag: Arc<AtomicBool> = Arc::new(AtomicBool::new(true));
    let run_flag_shared = Arc::clone(&run_flag);
    ctrlc::set_handler(move || run_flag_shared.store(false, Ordering::Relaxed))
        .expect("Could not set Ctrl-C handler");

    // Start interpreter
    Interpreter::new(&args, platform).run(run_flag);
}
