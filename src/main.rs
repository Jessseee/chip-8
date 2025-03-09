use clap::Parser;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use chip8::platform::Platform;
use chip8::interpreter::{Args, Interpreter};

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
