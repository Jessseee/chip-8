mod screen;
mod keypad;
mod interpreter;

#[macro_use]
extern crate paris;

use clap::Parser;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::interpreter::Interpreter;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    #[clap(help="To beep or not to beep?")]
    quiet: bool,
    #[arg(short, long)]
    #[clap(default_value="500", help="Clock speed in Hz.")]
    speed: u64,
    #[arg(long)]
    #[clap(default_value="12")]
    scale: f32
}

fn main() {
    // Parse command line arguments
    let args = Args::parse();

    // Create flag to handle Ctrl-C gracefully
    let run_flag: Arc<AtomicBool> = Arc::new(AtomicBool::new(true));
    let run_flag_shared = Arc::clone(&run_flag);
    ctrlc::set_handler(move || run_flag_shared.store(false, Ordering::Relaxed))
        .expect("Could not set Ctrl-C handler");

    // Start interpreter
    Interpreter::new(&args).run(run_flag, &args);
}
