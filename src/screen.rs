use sdl3::pixels::Color;
use sdl3::render::{Canvas, FPoint};
use sdl3::video::Window;

pub struct Screen {
    pub(crate) canvas: Canvas<Window>,
    buffer: Vec<bool>,
    resolutions: Vec<(u32, u32)>,
    current_resolution: usize,
    pub width: u32,
    pub height: u32,
    pub redraw_requested: bool,
}

const ON: Color = Color::RGBA(0xFC, 0x6A, 0x21, 0xFF);
const OFF: Color = Color::RGBA(0x08, 0x05, 0x04, 0xFF);

impl Screen {
    pub fn new(window: Window, resolutions: Vec<(u32, u32)>, scale: f32) -> Self {
        let (width, height) = *resolutions.first().unwrap();
        let mut canvas = window.into_canvas();
        canvas.set_scale(scale, scale)
            .expect("Failed to set canvas scale");
        let buffer = vec![false; (width * height) as usize];
        let redraw_requested = false;
        let current_resolution = 0;
        Self { canvas, buffer, resolutions, current_resolution, width, height, redraw_requested }
    }

    pub fn redraw(&mut self) -> bool {
        if !self.redraw_requested { return false }
        self.canvas.set_draw_color(OFF);
        self.canvas.clear();
        let (max_width, max_height) = self.resolutions.last().unwrap();
        let offset_x = (max_width - self.width) / 2;
        let offset_y = (max_height - self.height) / 2;
        let points: Vec<FPoint> = self.buffer.iter()
            .enumerate()
            .filter_map(|(i, &pixel)| {
                if !pixel { return None }
                let x = i % self.width as usize + offset_x as usize;
                let y = i / self.width as usize + offset_y as usize;
                Some(FPoint::new(x as f32, y as f32))
            }).collect();
        self.canvas.set_draw_color(ON);
        self.canvas.draw_points(points.as_slice()).unwrap();
        self.canvas.present();
        self.redraw_requested = false;
        true
    }

    pub fn enable_hires(&mut self) {
        if self.current_resolution < self.resolutions.len() {
            self.current_resolution += 1;
            (self.width, self.height) = self.resolutions[self.current_resolution];
            self.clear();
        }
    }

    pub fn disable_hires(&mut self) {
        if self.current_resolution > 0 {
            self.current_resolution -= 1;
            (self.width, self.height) = self.resolutions[self.current_resolution];
            self.clear();
        }
    }

    pub fn scroll_down(&mut self, n: u8) {
        let mut buffer = vec![false; (self.width * self.height) as usize];
        for y in 0..(self.height - n as u32) {
            for x in 0..self.width {
                let i = (x + y * self.width) as usize;
                let ni = (x + (y + n as u32) * self.width) as usize;
                buffer[ni] = self.buffer[i]
            }
        }
        self.buffer = buffer;
    }

    pub fn scroll_right(&mut self) {
        let mut buffer = vec![false; (self.width * self.height) as usize];
        for y in 0..self.height {
            for x in 0..(self.width - 4) {
                let i = (x + y * self.width) as usize;
                let ni = ((x + 4) + y * self.width) as usize;
                buffer[ni] = self.buffer[i]
            }
        }
        self.buffer = buffer;
    }

    pub fn scroll_left(&mut self) {
        let mut buffer = vec![false; (self.width * self.height) as usize];
        for y in 0..self.height {
            for x in 4..self.width {
                let i = (x + y * self.width) as usize;
                let ni = ((x - 4) + y * self.width) as usize;
                buffer[ni] = self.buffer[i]
            }
        }
        self.buffer = buffer;
    }

    pub fn toggle_pixel(&mut self, x: u32, y: u32) -> bool {
        let i = (x + y * self.width) as usize;
        let flag = self.buffer[i];
        self.buffer[i] = !self.buffer[i];
        self.redraw_requested = true;
        flag
    }

    pub fn clear(&mut self) {
        self.buffer = vec![false; (self.width * self.height) as usize];
        self.canvas.set_draw_color(OFF);
        self.canvas.clear();
        self.redraw_requested = true;
    }
}
