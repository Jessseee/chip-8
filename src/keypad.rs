use sdl3::keyboard::Scancode;

pub struct Keypad {
    keys: [bool; 16]
}

impl Keypad {
    pub fn new() -> Self{
        Self { 
            keys: [false; 16]
        }
    }
    
    pub fn handle_keydown(&mut self, scancode: Scancode) {
        if let Some(key) = self.scancode_to_key(scancode) {
            self.keys[key] = true;
        }
    }

    pub fn handle_keyup(&mut self, scancode: Scancode) {
        if let Some(key) = self.scancode_to_key(scancode) {
            self.keys[key] = false;
        }
    }
    
    pub fn get_keys(&mut self) -> Vec<u8> {
        self.keys.iter().enumerate().filter_map(|(i, &down)| {
            if down { Some(i as u8) } else { None }
        }).collect()
    }

    fn scancode_to_key(&self, scancode: Scancode) -> Option<usize> {
        match scancode {
            Scancode::_1 => Some(0x1),
            Scancode::_2 => Some(0x2),
            Scancode::_3 => Some(0x3),
            Scancode::_4 => Some(0xC),
            Scancode::Q => Some(0x4),
            Scancode::W => Some(0x5),
            Scancode::E => Some(0x6),
            Scancode::R => Some(0xD),
            Scancode::A => Some(0x7),
            Scancode::S => Some(0x8),
            Scancode::D => Some(0x9),
            Scancode::F => Some(0xE),
            Scancode::Z => Some(0xA),
            Scancode::X => Some(0x0),
            Scancode::C => Some(0xB),
            Scancode::V => Some(0xF),
            _ => None
        }
    }
}
