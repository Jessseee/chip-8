#[macro_use]
extern crate paris;

#[cfg(target_os = "windows")]
use std::str::FromStr;

use clap::Parser;
use minifb::{Icon, Key, KeyRepeat, Menu, MenuItem, Scale, ScaleMode, Window, WindowOptions};
use rand::random;
use rfd::FileDialog;
use rodio::source::SineWave;
use rodio::{OutputStream, Sink, Source};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use strum::FromRepr;

const SCREEN_WIDTH: usize = 64;
const SCREEN_HEIGHT: usize = 32;
const MEMORY_SIZE: usize = 4096;
const NUM_VAR_REGS: usize = 16;
const PROGRAM_ADDR: usize = 0x200;
const FONT_ADDR: usize = 0x050;
const FONT_SIZE: usize = 80;
const FONT: [u8; FONT_SIZE] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

#[derive(FromRepr, Debug, PartialEq)]
#[repr(usize)]
enum MenuItemId {
    OpenFile = 1,
}

fn open_window(width: usize, height: usize) -> Window {
    // Initialize window with menu
    let mut window = Window::new(
        "CHIP-8",
        width,
        height,
        WindowOptions {
            resize: false,
            scale: Scale::X16,
            scale_mode: ScaleMode::Stretch,
            ..WindowOptions::default()
        },
    )
    .expect("Failed to create window");

    #[cfg(target_os = "windows")]
    window.set_icon(Icon::from_str("logo.ico").expect("Failed to set window icon"));

    let mut menu = Menu::new("file").expect("Failed to create window menu.");
    let menu_item = MenuItem::new("open", MenuItemId::OpenFile as usize);
    menu.add_menu_item(&menu_item);
    window.add_menu(&menu);

    window
}

fn get_rom() -> Option<PathBuf> {
    FileDialog::new()
        .add_filter("chip-8 rom", &["ch8"])
        .pick_file()
}

struct Instruction {
    nibbles: (u8, u8, u8, u8),
    x: usize,
    y: usize,
    n: u8,
    nn: u8,
    nnn: u16,
    opcode: u16,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.opcode)
    }
}

impl Instruction {
    fn new(int: u16) -> Self {
        let nibbles = (
            ((int & 0xF000) >> 12) as u8,
            ((int & 0x0F00) >> 8) as u8,
            ((int & 0x00F0) >> 4) as u8,
            (int & 0x000F) as u8,
        );
        let x = nibbles.1 as usize;
        let y = nibbles.2 as usize;
        let n = nibbles.3;
        let nn = (int & 0x00FF) as u8;
        let nnn = int & 0x0FFF;
        let opcode = int;
        Self { nibbles, x, y, n, nn, nnn, opcode }
    }
}

struct Interpreter {
    program: Option<Vec<u8>>,
    window: Window,
    screen_buffer: Vec<u32>,
    memory: [u8; MEMORY_SIZE],
    pc: usize,
    index: usize,
    stack: Vec<usize>,
    delay_timer: u8,
    sound_timer: u8,
    vars: [u8; NUM_VAR_REGS],
    paused: bool,
    wait_input: bool,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            program: None,
            window: open_window(SCREEN_WIDTH, SCREEN_HEIGHT),
            screen_buffer: vec![0; SCREEN_WIDTH * SCREEN_HEIGHT],
            memory: [0; MEMORY_SIZE],
            pc: PROGRAM_ADDR,
            index: 0,
            stack: Vec::new(),
            delay_timer: 0,
            sound_timer: 0,
            vars: [0u8; NUM_VAR_REGS],
            paused: false,
            wait_input: false,
        }
    }
}

impl Interpreter {
    fn insert_data(&mut self, addr: usize, data: &[u8]) -> &mut Interpreter {
        self.memory[addr..addr + data.len()].copy_from_slice(&data);
        self
    }

    fn new() -> Self {
        let mut interpreter = Self::default();
        interpreter.insert_data(FONT_ADDR, &FONT);
        interpreter
    }

    fn decrement_timers(&mut self) {
        if self.delay_timer > 0 {
            info!("XXX: DELAY = {} - 1", self.delay_timer);
            self.delay_timer -= 1;
        }
        if self.sound_timer > 0 {
            info!("XXX: SOUND = {} - 1", self.sound_timer);
            self.sound_timer -= 1;
        }
    }

    fn fetch_instruction(&mut self) -> Instruction {
        let int = Instruction::new(
            ((self.memory[self.pc] as u16) << 8) | self.memory[self.pc + 1] as u16,
        );
        int
    }

    fn key_enum_to_keycode(&self, key: Key) -> Option<u8> {
        match key {
            Key::Key1 => Some(0x1),
            Key::Key2 => Some(0x2),
            Key::Key3 => Some(0x3),
            Key::Key4 => Some(0xC),
            Key::Q => Some(0x4),
            Key::W => Some(0x5),
            Key::E => Some(0x6),
            Key::R => Some(0xD),
            Key::A => Some(0x7),
            Key::S => Some(0x8),
            Key::D => Some(0x9),
            Key::F => Some(0xE),
            Key::Z => Some(0xA),
            Key::X => Some(0x0),
            Key::C => Some(0xB),
            Key::V => Some(0xF),
            _ => None
        }
    }

    fn execute_instruction(&mut self, int: Instruction) {
        let (vx, vy) = (self.vars[int.x], self.vars[int.y]);
        let from = self.pc; // Hold on to the position of the current instruction to detect loops
        self.pc += 2; // Increment program counter to next instruction

        // Get pressed keys
        let keys: Vec<u8> = self.window.get_keys()
            .iter()
            .filter_map(|&key| self.key_enum_to_keycode(key))
            .collect();

        match int.nibbles {
            /* 0NNN: Machine language routines */
            /* 0000: NOP, do nothing */
            (0x0, 0x0, 0x0, 0x0) => {
                info!("{}: {} -> NOP", from, int);
            }
            /* 00E0: Clear screen */
            (0x0, 0x0, 0xE, 0x0) => {
                info!("{}: {} -> Clear screen", from, int);
                self.clear_screen();
                return; // return to skip window.update()
            }
            /* 00EE: Return from subroutine */
            (0x0, 0x0, 0xE, 0xE) => {
                let to = self.stack.pop()
                    .expect("00EE -> Error: There should be an address on the stack to return to");
                info!("{}: {} -> Return to line {} from subroutine", from, int, to);
                self.pc = to;
            }

            /* 1NNN: Jump */
            (0x1, _, _, _) => {
                let to = int.nnn as usize;

                if from != to {
                    info!("{}: {} -> Jump to line {}", from, int, to);
                } else {
                    info!("{}: {} -> Jump to line {} (Entered infinite loop)", from, int, to);
                    self.paused = true;
                }
                self.pc = to;
            }

            /* 2NNN: Enter subroutine */
            (0x2, _, _, _) => {
                info!("{}: {} -> Enter subroutine at line {}", from, int, int.nnn);
                self.stack.push(self.pc);
                self.pc = int.nnn as usize;
            }

            /* 3XNN: Skip if VX == NN */
            (0x3, _, _, _) => {
                info!("{}: {} -> Skip if {} == {}", from, int, vx, int.nn);
                if vx == int.nn {
                    self.pc += 2;
                }
            }

            /* 4XNN: Skip if VX != NN */
            (0x4, _, _, _) => {
                info!("{}: {} -> Skip if {} != {}", from, int, vx, int.nn);
                if vx != int.nn {
                    self.pc += 2;
                }
            }

            /* 5XY0: Skip if VX == VY */
            (0x5, _, _, _) => {
                info!("{}: {} -> Skip if {} == {}", from, int, vx, vy);
                if vx == vy {
                    self.pc += 2;
                }
            }

            /* 9XY0: Skip if VX != VY */
            (0x9, _, _, _) => {
                info!("{}: {} -> Skip if {} != {}", from, int, vx, vy);
                if vx != vy {
                    self.pc += 2;
                }
            }

            /* 6XNN: Set */
            (0x6, _, _, _) => {
                info!("{}: {} -> Set V{:X} = {}", from, int, int.x, int.nn);
                self.vars[int.x] = int.nn;
            }

            /* 7XNN: Add */
            (0x7, _, _, _) => {
                info!("{}: {} -> Add V{:X}<{}> += {}", from, int, int.x, vx, int.nn);
                self.vars[int.x] = vx.wrapping_add(int.nn);
            }

            /* 8XYN: Logical and arithmetic instructions */
            /* 8XY0: Set */
            (0x8, _, _, 0x0) => {
                info!("{}: {} -> Set V{:X} = V{:X}<{}>", from, int, int.x, int.y, vy);
                self.vars[int.x] = vy;
            }
            /* 8XY1: Binary OR */
            (0x8, _, _, 0x1) => {
                info!("{}: {} -> OR V{:X}<{}> |= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] |= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY2: Binary AND */
            (0x8, _, _, 0x2) => {
                info!("{}: {} -> AND V{:X}<{}> &= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] &= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY3: Binary XOR */
            (0x8, _, _, 0x3) => {
                info!("{}: {} -> XOR V{:X}<{}> ^= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] ^= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY4: Add */
            (0x8, _, _, 0x4) => {
                info!("{}: {} -> Add V{:X}<{}> += V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                let (res, flag) = vx.overflowing_add(vy);
                self.vars[int.x] = res;
                self.vars[0xF] = flag as u8;
            }
            /* 8XY5: Subtract VX - VY */
            (0x8, _, _, 0x5) => {
                info!("{}: {} -> Subtract V{:X}>{}> -= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                let (res, flag) = vx.overflowing_sub(vy);
                self.vars[int.x] = res;
                self.vars[0xF] = !flag as u8;
            }
            /* 8XY7: Subtract VY - VX */
            (0x8, _, _, 0x7) => {
                info!("{}: {} -> Subtract V{:X} = V{:X}<{}> - V{2:X}<{}>", from, int, int.x, int.y, vy, vx);
                let (res, flag) = vy.overflowing_sub(vx);
                self.vars[int.x] = res;
                self.vars[0xF] = !flag as u8;
            }
            /* 8XY6: Shift right */
            (0x8, _, _, 0x6) => {
                info!("{}: {} -> Shift right V{:X} = {} >> 1", from, int, int.x, vy);
                self.vars[int.x] = vy >> 1;
                self.vars[0xF] = vy & 1;
            }
            /* 8XYE: Shift left */
            (0x8, _, _, 0xE) => {
                info!("{}: {} -> Shift left V{:X} = {} << 1", from, int, int.x, vy);
                self.vars[int.x] = vy << 1;
                self.vars[0xF] = (vy >> 7) & 1;
            }
            /* ANNN: Set index */
            (0xA, _, _, _) => {
                info!("{}: {} -> Set index = {}", from, int, int.nnn);
                self.index = int.nnn as usize
            }

            /* BNNN: Jump with offset */
            (0xB, _, _, _) => {
                let to = (int.nnn + self.vars[0] as u16) as usize;
                info!("{}: {} -> Jump with offset to line {} + {} = {}", from, int, int.nnn, self.vars[0], to);
                self.pc = to;
            }

            /* CXNN: Random */
            (0xC, _, _, _) => {
                info!("{}: {} -> Random V{:X} = R + {}", from, int, int.x, int.nn);
                self.vars[int.x] = random::<u8>() & int.nn;
            }

            /* DXYN: Display */
            (0xD, _, _, _) => {
                info!("{}: {} -> Display sprite at {}, {}", from, int, vx, vy);
                let x = vx as u32 % SCREEN_WIDTH as u32;
                let y = vy as u32 % SCREEN_HEIGHT as u32;
                self.vars[0xF] = 0;
                let rows = &self.memory[self.index..self.index + int.n as usize];
                for (dy, row) in (0..int.n as u32).zip(rows) {
                    if y + dy >= (SCREEN_HEIGHT as u32) {
                        break;
                    }
                    for dx in 0..8 {
                        let i = ((x + dx) + (y + dy) * SCREEN_WIDTH as u32) as usize;
                        if x + dx >= (SCREEN_WIDTH as u32) {
                            break;
                        }
                        let pixel_on = (row >> (7 - dx)) & 1 == 1;
                        if pixel_on {
                            self.screen_buffer[i] =
                                if self.screen_buffer[i] > 0 { 0x0 } else { 0xFFFFFFFF }
                        }
                    }
                }
                self.window
                    .update_with_buffer(&self.screen_buffer, SCREEN_WIDTH, SCREEN_HEIGHT)
                    .expect("Failed to update window.");
            }

            /* EXNN: Skip if key (non-blocking) */
            /* EX9E: Skip if pressed */
            (0xE, _, 0x9, 0xE) => {
                if keys.contains(&vx) {
                    info!("{}: {} -> Skipping, key {} is pressed", from, int, vx);
                    self.pc += 2;
                } else {
                    info!("{}: {} -> Not skipping, key {} is not pressed", from, int, vx);
                }
            }
            /* EXA1: Skip if not pressed */
            (0xE, _, 0xA, 0x1) => {
                if keys.contains(&vx)  {
                    info!("{}: {} -> Not skipping, key {} is pressed", from, int, vx);
                } else {
                    info!("{}: {} -> Skipping, key {} is not pressed", from, int, vx);
                    self.pc += 2;
                }
            }

            /* FXNN: Miscellaneous */
            /* FX07: Set VX to delay timer */
            (0xF, _, 0x0, 0x7) => {
                info!(
                    "{}: {} -> Set V{:X} = {} (delay)",
                    self.pc, int, int.x, self.delay_timer
                );
                self.vars[int.x] = self.delay_timer
            }
            /* FX15: Set delay timer to VX */
            (0xF, _, 0x1, 0x5) => {
                info!("{}: {} -> Set delay = {}", from, int, vx);
                self.sound_timer = vx
            }
            /* FX18: Set sound timer to VX */
            (0xF, _, 0x1, 0x8) => {
                info!("{}: {} -> Set sound = {}", from, int, vx);
                self.delay_timer = vx
            }
            /* FX1E: Add to index */
            (0xF, _, 0x1, 0xE) => {
                info!("{}: {} -> Add to index += {}", from, int, vx);
                self.index += vx as usize
            }
            /* FX0A: Get key (blocking) */
            (0xF, _, 0x0, 0xA) => {
                let keys = self.window.get_keys_released();
                if keys.is_empty() {
                    if !self.wait_input {
                        info!("{}: {} -> Wait for key release", from, int);
                        self.wait_input = true;
                    }
                    self.pc -= 2;
                } else if let Some(key) = self.key_enum_to_keycode(keys[0]) {
                    info!("{}: {} -> Key {:01X} released", from, int, int.x);
                    self.vars[int.x] = key;
                    self.wait_input = false;
                }
            }
            /* FX29: Font character */
            (0xF, _, 0x2, 0x9) => {
                let addr = FONT_ADDR + vx as usize;
                info!("{}: {} -> Set index to font character at {:04X}", from, int, addr);
                self.index = addr;
            }
            /* FX33: Binary-coded decimal conversion */
            (0xF, _, 0x3, 0x3) => {
                info!("{}: {} -> Convert {:04X} to decimal {2}", from, int, vx);
                let byte = vx;
                self.memory[self.index] = (byte / 100) % 10;
                self.memory[self.index + 1] = (byte / 10) % 10;
                self.memory[self.index + 2] = byte % 10;
            }
            /* FX55: Store variable registers into memory */
            (0xF, _, 0x5, 0x5) => {
                info!("{}: {} -> Store V0..{} into memory[{:04X}..{:04X}]", from, int, int.x, self.index, self.index + int.x);
                (0..=int.x).for_each(|i| self.memory[self.index + i] = self.vars[i]);
                self.index += int.x + 1;
            }
            /* FX55: Store variable registers into memory */
            (0xF, _, 0x6, 0x5) => {
                info!("{}: {} -> Store memory[{:04X}..{:04X}] into V0..{}", from, int, self.index, self.index + int.x, int.x);
                (0..=int.x).for_each(|i| self.vars[i] = self.memory[self.index + i]);
                self.index += int.x + 1;
            }

            /* Unknown instruction */
            _ => panic!("Unknown instruction at {}: {}", self.pc, int),
        }

        self.window.update();
    }

    fn clear_screen(&mut self) {
        self.screen_buffer = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT];
        self.window
            .update_with_buffer(&self.screen_buffer, SCREEN_WIDTH, SCREEN_HEIGHT)
            .expect("Failed to update window.");
    }

    fn run(mut self, run_flag: Arc<AtomicBool>, args: Args) {
        // Initialize 60Hz delay timer
        let timer_interval = Duration::from_secs_f32(1. / 60.);
        let mut next_timer_tick = Instant::now() + timer_interval;

        // Initialize 500Hz clock timer
        let clock_interval = Duration::from_millis(1000/args.speed);
        let mut next_clock_tick = Instant::now() + clock_interval;

        // Initialize audio stream
        let (_stream, stream_handle) = OutputStream::try_default().unwrap();
        let audio_stream = Sink::try_new(&stream_handle).unwrap();
        let beep = SineWave::new(440.0)
            .take_duration(Duration::from_secs(1))
            .amplify(0.20);

        // Event loop
        while run_flag.load(Ordering::Relaxed)
            && self.window.is_open()
            && !self.window.is_key_down(Key::Escape)
        {
            // Handle window events
            if let Some(menu_id) = self.window.is_menu_pressed() {
                match MenuItemId::from_repr(menu_id) {
                    Some(MenuItemId::OpenFile) => {
                        if let Some(file_path) = get_rom() {
                            let program =
                                std::fs::read(file_path).expect("Failed to read selected program.");
                            self = Interpreter::new();
                            self.insert_data(PROGRAM_ADDR, &program);
                            self.program = Some(program);
                            self.clear_screen();
                            continue;
                        }
                    }
                    _ => (),
                }
            }

            // pause/unpause program if space is pressed
            if self.window.is_key_pressed(Key::Space, KeyRepeat::Yes) {
                self.paused = !self.paused;
            }

            // Stop executing if no program is loaded or interpreter is paused, allow stepping with right arrow
            if self.program == None || self.paused && !self.window.is_key_pressed(Key::Right, KeyRepeat::Yes) {
                self.window.update();
                continue;
            }

            // Decrement timers and play sound at set interval
            if Instant::now() >= next_timer_tick {
                next_timer_tick = Instant::now() + timer_interval;
                self.decrement_timers();
                if !args.quiet {
                    if self.sound_timer > 0 {
                        audio_stream.append(beep.clone());
                    } else if !audio_stream.empty() {
                        audio_stream.stop();
                    }
                }
            }

            // Fetch and execute instructions at set interval
            if Instant::now() >= next_clock_tick {
                next_clock_tick = Instant::now() + clock_interval;
                let int = self.fetch_instruction();
                self.execute_instruction(int);
            }
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    #[clap(help="To beep or not to beep?")]
    quiet: bool,
    #[arg(short, long)]
    #[clap(default_value="500", help="Clock speed in Hz.")]
    speed: u64,
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
    Interpreter::new().run(run_flag, args);
}
