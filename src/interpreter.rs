use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::sleep;
use std::time::{Duration, Instant};
use rand::random;
use sdl3::dialog::{show_open_file_dialog, DialogCallback, DialogFileFilter};
use sdl3::event::Event;
use sdl3::{AudioSubsystem, EventPump};
use sdl3::keyboard::Keycode;
use sdl3::video::Window;
use crate::Args;
use crate::platform::{Platform, Quirk};
use crate::keypad::Keypad;
use crate::screen::Screen;
use crate::audio::Audio;

const MEMORY_SIZE: usize = 4096;
const NUM_VAR_REGS: usize = 16;
const PROGRAM_ADDR: usize = 0x200;
const FONT_ADDR: usize = 0x050;
const FONT: [u8; 80] = [
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

struct Interrupt {
    interval: Duration,
    next: Instant,
}

impl Interrupt {
    pub fn new(hertz: u64) -> Self {
        let interval = Duration::from_millis(1000 / hertz);
        let next = Instant::now() + interval;
        Self { interval, next }
    }

    pub fn irq(&mut self) -> bool {
        if Instant::now() < self.next {
            false
        } else {
            self.next = Instant::now() + self.interval;
            true
        }
    }

    pub fn sleep_until_next_irq(&mut self) {
        sleep(self.next.duration_since(Instant::now()));
        self.next = Instant::now() + self.interval;
    }
}

#[derive(PartialEq)]
enum Debug {
    ShouldStep,
    HasStepped,
    CanStep
}

pub struct Interpreter {
    quirks: HashMap<Quirk, bool>,
    program: Arc<Mutex<Option<Vec<u8>>>>,
    program_changed: Arc<AtomicBool>,
    events: EventPump,
    keypad: Keypad,
    screen: Screen,
    audio: Audio,
    memory: [u8; MEMORY_SIZE],
    vars: [u8; NUM_VAR_REGS],
    stack: Vec<usize>,
    pc: usize,
    index: usize,
    delay_timer: u8,
    sound_timer: u8,
    paused: bool,
    wait_input: bool,
    quiet: bool,
    display_interrupt: Interrupt,
    timer_interrupt: Interrupt,
    clock_interrupt: Interrupt,
}

impl Interpreter {
    pub fn new(args: &Args, platform: Platform) -> Self {
        let resolutions = platform.resolutions();
        let (width, height) = resolutions.last().unwrap();
        let window_title = format!("CHIP-8 | {:?}", platform.id);
        let (window, event_pump, audio) = Self::open_window(width, height, args.scale, window_title);
        let mut interpreter = Self {
            quirks: platform.quirks,
            program: Arc::new(Mutex::new(None)),
            program_changed: Arc::new(AtomicBool::new(false)),
            events: event_pump,
            keypad: Keypad::new(),
            screen: Screen::new(window, resolutions, args.scale),
            audio: Audio::new(audio),
            memory: [0; MEMORY_SIZE],
            vars: [0u8; NUM_VAR_REGS],
            stack: Vec::new(),
            pc: PROGRAM_ADDR,
            index: 0,
            delay_timer: 0,
            sound_timer: 0,
            paused: false,
            wait_input: false,
            quiet: args.quiet,
            display_interrupt: Interrupt::new(60),
            timer_interrupt: Interrupt::new(60),
            clock_interrupt: Interrupt::new(platform.default_tickrate * 60),
        };
        interpreter.insert_data(FONT_ADDR, &FONT);
        interpreter
    }

    fn reset(&mut self) {
        self.memory = [0; MEMORY_SIZE];
        self.insert_data(FONT_ADDR, &FONT);
        self.pc = PROGRAM_ADDR;
        self.vars = [0u8; NUM_VAR_REGS];
        self.stack = Vec::new();
        self.delay_timer = 0;
        self.sound_timer = 0;
        self.paused = false;
        self.wait_input = false;
        self.screen.clear();
    }

    fn open_window(width: &u32, height: &u32, scale: f32, window_title: String) -> (Window, EventPump, AudioSubsystem) {
        let sdl_context = sdl3::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window(&window_title, width * scale as u32, height * scale as u32)
            .position_centered()
            .build()
            .unwrap();
        let event_pump = sdl_context.event_pump().unwrap();
        let audio = sdl_context.audio().unwrap();
        (window, event_pump, audio)
    }

    fn get_rom(&self) {
        let program_clone = Arc::clone(&self.program);
        let program_changed = Arc::clone(&self.program_changed);
        let callback: DialogCallback = Box::new(move |result, _filters| {
            let paths = result.ok();
            *program_clone.lock().unwrap() = Some(
                match paths {
                    Some(paths) => std::fs::read(paths.first().unwrap())
                        .expect("Failed to read selected program."),
                    _ => include_bytes!("../roms/ibm-logo.ch8").to_vec(),
                }
            );
            program_changed.store(true, Ordering::Relaxed);
        });
        let filters = [
            DialogFileFilter { name: "CHIP-8 ROM", pattern: "ch8" }
        ];
        show_open_file_dialog(
            &filters,
            None::<PathBuf>,
            false,
            self.screen.canvas.window(),
            callback
        ).expect("Failed to show open file dialog")
    }

    fn insert_data(&mut self, addr: usize, data: &[u8]) -> &mut Interpreter {
        self.memory[addr..addr + data.len()].copy_from_slice(&data);
        self
    }

    fn fetch_instruction(&mut self) -> Instruction {
        Instruction::new(((self.memory[self.pc] as u16) << 8) | self.memory[self.pc + 1] as u16)
    }

    fn execute_instruction(&mut self, int: Instruction) {
        let (vx, vy) = (self.vars[int.x], self.vars[int.y]);
        let from = self.pc; // Hold on to the position of the current instruction to detect loops
        self.pc += 2; // Increment program counter to next instruction
        let keys = self.keypad.get_keys();

        match int.nibbles {
            /* 0000: NOP, do nothing */
            (0x0, 0x0, 0x0, 0x0) => {
                info!("{:04}: {} -> NOP", from, int);
            }
            /* 00E0: Clear screen */
            (0x0, 0x0, 0xE, 0x0) => {
                info!("{:04}: {} -> Clear screen", from, int);
                self.screen.clear();
            }
            /* 00EE: Return from subroutine */
            (0x0, 0x0, 0xE, 0xE) => {
                let to = self.stack.pop()
                    .expect("XXXX: 00EE -> Error: There should be an address on the stack to return to");
                info!("{:04}: {} -> Return to line {} from subroutine", from, int, to);
                self.pc = to;
            }
            /* 1NNN: Jump */
            (0x1, _, _, _) => {
                let to = int.nnn as usize;

                if from != to {
                    info!("{:04}: {} -> Jump to line {}", from, int, to);
                } else {
                    info!("{:04}: {} -> Jump to line {} (Entered infinite loop)", from, int, to);
                    self.paused = true;
                }
                self.pc = to;
            }
            /* 2NNN: Enter subroutine */
            (0x2, _, _, _) => {
                info!("{:04}: {} -> Enter subroutine at line {}", from, int, int.nnn);
                self.stack.push(self.pc);
                self.pc = int.nnn as usize;
            }
            /* 3XNN: Skip if VX == NN */
            (0x3, _, _, _) => {
                info!("{:04}: {} -> Skip if {} == {}", from, int, vx, int.nn);
                if vx == int.nn {
                    self.pc += 2;
                }
            }
            /* 4XNN: Skip if VX != NN */
            (0x4, _, _, _) => {
                info!("{:04}: {} -> Skip if {} != {}", from, int, vx, int.nn);
                if vx != int.nn {
                    self.pc += 2;
                }
            }
            /* 5XY0: Skip if VX == VY */
            (0x5, _, _, _) => {
                info!("{:04}: {} -> Skip if {} == {}", from, int, vx, vy);
                if vx == vy {
                    self.pc += 2;
                }
            }
            /* 9XY0: Skip if VX != VY */
            (0x9, _, _, _) => {
                info!("{:04}: {} -> Skip if {} != {}", from, int, vx, vy);
                if vx != vy {
                    self.pc += 2;
                }
            }
            /* 6XNN: Set */
            (0x6, _, _, _) => {
                info!("{:04}: {} -> Set V{:X} = {}", from, int, int.x, int.nn);
                self.vars[int.x] = int.nn;
            }
            /* 7XNN: Add */
            (0x7, _, _, _) => {
                info!("{:04}: {} -> Add V{:X}<{}> += {}", from, int, int.x, vx, int.nn);
                self.vars[int.x] = vx.wrapping_add(int.nn);
            }
            /* 8XY0: Set */
            (0x8, _, _, 0x0) => {
                info!("{:04}: {} -> Set V{:X} = V{:X}<{}>", from, int, int.x, int.y, vy);
                self.vars[int.x] = vy;
            }
            /* 8XY1: Binary OR - Logic quirk */
            (0x8, _, _, 0x1) if self.quirks[&Quirk::Logic] => {
                info!("{:04}: {} -> OR V{:X}<{}> |= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] |= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY1: Binary OR */
            (0x8, _, _, 0x1) => {
                info!("{:04}: {} -> OR V{:X}<{}> |= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] |= vy;
            }
            /* 8XY2: Binary AND - Logic quirk */
            (0x8, _, _, 0x2) if self.quirks[&Quirk::Logic] => {
                info!("{:04}: {} -> AND V{:X}<{}> &= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] &= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY2: Binary AND */
            (0x8, _, _, 0x2) => {
                info!("{:04}: {} -> AND V{:X}<{}> &= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] &= vy;
            }
            /* 8XY3: Binary XOR - Logic quirk */
            (0x8, _, _, 0x3) if self.quirks[&Quirk::Logic] => {
                info!("{:04}: {} -> XOR V{:X}<{}> ^= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] ^= vy;
                self.vars[0xF] = 0;
            }
            /* 8XY3: Binary XOR */
            (0x8, _, _, 0x3) => {
                info!("{:04}: {} -> XOR V{:X}<{}> ^= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                self.vars[int.x] ^= vy;
            }
            /* 8XY4: Add */
            (0x8, _, _, 0x4) => {
                info!("{:04}: {} -> Add V{:X}<{}> += V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                let (res, flag) = vx.overflowing_add(vy);
                self.vars[int.x] = res;
                self.vars[0xF] = flag as u8;
            }
            /* 8XY5: Subtract VX - VY */
            (0x8, _, _, 0x5) => {
                info!("{:04}: {} -> Subtract V{:X}<{}> -= V{:X}<{}>", from, int, int.x, vx, int.y, vy);
                let (res, flag) = vx.overflowing_sub(vy);
                self.vars[int.x] = res;
                self.vars[0xF] = !flag as u8;
            }
            /* 8XY7: Subtract VY - VX */
            (0x8, _, _, 0x7) => {
                info!("{:04}: {} -> Subtract V{:X} = V{:X}<{}> - V{2:X}<{}>", from, int, int.x, int.y, vy, vx);
                let (res, flag) = vy.overflowing_sub(vx);
                self.vars[int.x] = res;
                self.vars[0xF] = !flag as u8;
            }
            /* 8XY6: Shift right - Shift quirk */
            (0x8, _, _, 0x6) if self.quirks[&Quirk::Shift] => {
                info!("{:04}: {} -> Shift right V{:X} = V{2:X}<{}> >> 1", from, int, int.x, vx);
                self.vars[int.x] = vx >> 1;
                self.vars[0xF] = vx & 1;
            }
            /* 8XY6: Shift right */
            (0x8, _, _, 0x6) => {
                info!("{:04}: {} -> Shift right V{:X} = V{:X}<{}> >> 1", from, int, int.x, int.y, vy);
                self.vars[int.x] = vy >> 1;
                self.vars[0xF] = vy & 1;
            }
            /* 8XYE: Shift left - Shift quirk */
            (0x8, _, _, 0xE) if self.quirks[&Quirk::Shift] => {
                info!("{:04}: {} -> Shift left V{:X} = V{2:X}<{}> << 1", from, int, int.x, vx);
                self.vars[int.x] = vx << 1;
                self.vars[0xF] = (vx >> 7) & 1;
            }
            /* 8XYE: Shift left */
            (0x8, _, _, 0xE) => {
                info!("{:04}: {} -> Shift left V{:X} = V{:X}<{}> << 1", from, int, int.x, int.y, vy);
                self.vars[int.x] = vy << 1;
                self.vars[0xF] = (vy >> 7) & 1;
            }
            /* ANNN: Set index */
            (0xA, _, _, _) => {
                info!("{:04}: {} -> Set index = {}", from, int, int.nnn);
                self.index = int.nnn as usize
            }

            /* BNNN: Jump with offset - Jump quirk */
            (0xB, _, _, _) if self.quirks[&Quirk::Jump] => {
                let to = (int.nnn + vx as u16) as usize;
                info!("{:04}: {} -> Jump with offset to line {} + V{:X}<{}> = {}", from, int, int.x, vx, self.vars[0], to);
                self.pc = to;
            }
            /* BNNN: Jump with offset */
            (0xB, _, _, _) => {
                let to = (int.nnn + self.vars[0] as u16) as usize;
                info!("{:04}: {} -> Jump with offset to line {} + {} = {}", from, int, int.nnn, self.vars[0], to);
                self.pc = to;
            }
            /* CXNN: Random */
            (0xC, _, _, _) => {
                info!("{:04}: {} -> Random V{:X} = R + {}", from, int, int.x, int.nn);
                self.vars[int.x] = random::<u8>() & int.nn;
            }
            /* DXYN: Display - Wrap quirk */
            (0xD, _, _, _) if self.quirks[&Quirk::Wrap] => {
                info!("{:04}: {} -> Display sprite at {}, {}", from, int, vx, vy);
                let x = vx as u32 % self.screen.width;
                let y = vy as u32 % self.screen.height;
                self.vars[0xF] = 0;
                let rows = &self.memory[self.index..self.index + int.n as usize];
                for (dy, row) in (0..int.n as u32).zip(rows) {
                    for dx in 0..8 {
                        if (row >> (7 - dx)) & 1 == 1 {
                            if self.screen.draw_point(
                                (x + dx) % self.screen.width,
                                (y + dy) % self.screen.height
                            ) {
                                self.vars[0xF] = 1;
                            }
                        }
                    }
                }
                self.screen.redraw_requested = true;
            }
            /* DXYN: Display */
            (0xD, _, _, _) => {
                info!("{:04}: {} -> Display sprite at {}, {}", from, int, vx, vy);
                let x = vx as u32 % self.screen.width;
                let y = vy as u32 % self.screen.height;
                self.vars[0xF] = 0;
                let rows = &self.memory[self.index..self.index + int.n as usize];
                for (dy, row) in (0..int.n as u32).zip(rows) {
                    if y + dy >= (self.screen.height) { break; }
                    for dx in 0..8 {
                        if x + dx >= (self.screen.width) { break; }
                        if (row >> (7 - dx)) & 1 == 1 {
                            if self.screen.draw_point(
                                x + dx,
                                y + dy
                            ) {
                                self.vars[0xF] = 1;
                            }
                        }
                    }
                }
                self.screen.redraw_requested = true;
            }
            /* EX9E: Skip if pressed */
            (0xE, _, 0x9, 0xE) => {
                if keys.contains(&vx) {
                    info!("{:04}: {} -> Skipping, key {} is pressed", from, int, vx);
                    self.pc += 2;
                } else {
                    info!("{:04}: {} -> Not skipping, key {} is not pressed", from, int, vx);
                }
            }
            /* EXA1: Skip if not pressed */
            (0xE, _, 0xA, 0x1) => {
                if keys.contains(&vx)  {
                    info!("{:04}: {} -> Not skipping, key {} is pressed", from, int, vx);
                } else {
                    info!("{:04}: {} -> Skipping, key {} is not pressed", from, int, vx);
                    self.pc += 2;
                }
            }
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
                info!("{:04}: {} -> Set delay = {}", from, int, vx);
                self.delay_timer = vx
            }
            /* FX18: Set sound timer to VX */
            (0xF, _, 0x1, 0x8) => {
                info!("{:04}: {} -> Set sound = {}", from, int, vx);
                self.sound_timer = vx
            }
            /* FX1E: Add to index */
            (0xF, _, 0x1, 0xE) => {
                info!("{:04}: {} -> Add to index += {}", from, int, vx);
                self.index += vx as usize
            }
            /* FX0A: Get key (blocking) */
            (0xF, _, 0x0, 0xA) => {
                if keys.is_empty() {
                    if !self.wait_input {
                        info!("{:04}: {} -> Wait for key release", from, int);
                        self.wait_input = true;
                    }
                    self.pc -= 2;
                } else {
                    info!("{:04}: {} -> Key {:01X} released", from, int, int.x);
                    self.vars[int.x] = keys[0];
                    self.wait_input = false;
                }
            }
            /* FX29: Font character */
            (0xF, _, 0x2, 0x9) => {
                let addr = FONT_ADDR + vx as usize * 5;
                info!("{:04}: {} -> Set index to font character at {:04X}", from, int, addr);
                self.index = addr;
            }
            /* FX33: Binary-coded decimal conversion */
            (0xF, _, 0x3, 0x3) => {
                info!("{:04}: {} -> Convert {:04X} to decimal {2}", from, int, vx);
                let byte = vx;
                self.memory[self.index] = (byte / 100) % 10;
                self.memory[self.index + 1] = (byte / 10) % 10;
                self.memory[self.index + 2] = byte % 10;
            }
            /* FX55: Store variable registers into memory - Leave index unchanged quirk */
            (0xF, _, 0x5, 0x5) if self.quirks[&Quirk::MemoryLeaveIUnchanged] => {
                info!("{:04}: {} -> Store V0..{} into memory[{:04X}..{:04X}]", from, int, int.x, self.index, self.index + int.x);
                (0..=int.x).for_each(|i| self.memory[self.index + i] = self.vars[i]);
            }
            /* FX55: Store variable registers into memory - Increment index by X quirk */
            (0xF, _, 0x5, 0x5) if self.quirks[&Quirk::MemoryIncrementByX] => {
                info!("{:04}: {} -> Store V0..{} into memory[{:04X}..{:04X}]", from, int, int.x, self.index, self.index + int.x);
                (0..=int.x).for_each(|i| self.memory[self.index + i] = self.vars[i]);
                self.index += int.x;
            }
            /* FX55: Store variable registers into memory */
            (0xF, _, 0x5, 0x5) => {
                info!("{:04}: {} -> Store V0..{} into memory[{:04X}..{:04X}]", from, int, int.x, self.index, self.index + int.x);
                (0..=int.x).for_each(|i| self.memory[self.index + i] = self.vars[i]);
                self.index += int.x + 1;
            }
            /* FX65: Store memory into variable registers - Leave index unchanged quirk */
            (0xF, _, 0x6, 0x5) if self.quirks[&Quirk::MemoryLeaveIUnchanged] => {
                info!("{:04}: {} -> Store memory[{:04X}..{:04X}] into V0..{}", from, int, self.index, self.index + int.x, int.x);
                (0..=int.x).for_each(|i| self.vars[i] = self.memory[self.index + i]);
            }
            /* FX65: Store memory into variable registers - Increment index by X quirk */
            (0xF, _, 0x6, 0x5) if self.quirks[&Quirk::MemoryIncrementByX] => {
                info!("{:04}: {} -> Store memory[{:04X}..{:04X}] into V0..{}", from, int, self.index, self.index + int.x, int.x);
                (0..=int.x).for_each(|i| self.vars[i] = self.memory[self.index + i]);
                 self.index += int.x;
            }
            /* FX65: Store memory into variable registers */
            (0xF, _, 0x6, 0x5) => {
                info!("{:04}: {} -> Store memory[{:04X}..{:04X}] into V0..{}", from, int, self.index, self.index + int.x, int.x);
                (0..=int.x).for_each(|i| self.vars[i] = self.memory[self.index + i]);
                self.index += int.x + 1;
            }
            /* Unknown instruction */
            _ => panic!("Unknown instruction at {}: {}", self.pc, int),
        }
    }

    pub fn run(mut self, run_flag: Arc<AtomicBool>) {
        // Load program
        self.get_rom();

        let mut step = Debug::CanStep;

        // Event loop
        'running: while run_flag.load(Ordering::Relaxed)
        {
            // Slow down execution to platform clock speed
            self.clock_interrupt.sleep_until_next_irq();

            // Handle window events
            if let Some(event) = self.events.poll_event() {
                match event {
                    Event::Quit {..} |
                    Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                        break 'running
                    },
                    Event::KeyDown { keycode: Some(Keycode::Space), .. } => {
                        self.paused = !self.paused;
                        step = Debug::CanStep;
                    }
                    Event::KeyDown { keycode: Some(Keycode::Right), .. } => {
                        if step == Debug::CanStep { step = Debug::ShouldStep }
                    }
                    Event::KeyUp { keycode: Some(Keycode::Right), .. } => {
                        step = Debug::CanStep
                    }
                    Event::KeyDown { keycode: Some(Keycode::O), .. } => {
                        self.get_rom();
                    }
                    Event::KeyDown { keycode: Some(key), .. } => {
                        self.keypad.handle_keydown(key);
                    }
                    Event::KeyUp { keycode: Some(key), .. } => {
                        self.keypad.handle_keyup(key);
                    }
                    _ => ()
                }
            }

            // Insert new program and reset interpreter if program was changed
            if self.program_changed.load(Ordering::Relaxed) {
                let program = self.program.lock().unwrap().clone().unwrap();
                self.reset();
                self.insert_data(PROGRAM_ADDR, &program);
                self.program_changed.store(false, Ordering::Relaxed);
            }

            // Redraw screen
            if self.display_interrupt.irq() {
                self.screen.redraw();
            }

            // Wait for screen redraw before executing next instruction
            if self.quirks[&Quirk::Vblank] && self.screen.redraw_requested {
                continue;
            }

            // Stop executing if no program is loaded or paused, allow debug stepping
            if step != Debug::ShouldStep && (self.program.lock().unwrap().is_none() || self.paused) {
                continue;
            }

            // Decrement timers and play sound
            if self.timer_interrupt.irq() {
                if self.delay_timer > 0 {
                    self.delay_timer -= 1;
                    info!("XXXX: DELAY -= 1 = {}", self.delay_timer);
                }
                if self.sound_timer > 0 {
                    self.sound_timer -= 1;
                    info!("XXXX: SOUND -= 1 = {}", self.sound_timer);
                }
                if !self.quiet {
                    if self.sound_timer > 0 {
                        self.audio.start_beep();
                    } else {
                        self.audio.stop_beep();
                    }
                }
            }

            // Fetch and execute instructions
            let int = self.fetch_instruction();
            self.execute_instruction(int);

            step = Debug::HasStepped;
        }
    }
}
