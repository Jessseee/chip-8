use sdl3::pixels::Color;
use sdl3::render::{Canvas, FPoint};
use sdl3::video::Window;

pub struct Screen {
    pub(crate) canvas: Canvas<Window>,
    buffer: Vec<bool>,
    resolutions: Vec<(u32, u32)>,
    pub width: u32,
    pub height: u32,
    pub redraw_requested: bool,
}

const ON: Color = Color::RGBA(0xFC, 0x6A, 0x21, 0xFF);
const OFF: Color = Color::RGBA(0x08, 0x05, 0x04, 0xFF);

impl Screen {
    pub fn new(window: Window, resolutions: Vec<(u32, u32)>, scale: f32) -> Self {
        let mut canvas = window.into_canvas();
        canvas.set_scale(scale, scale)
            .expect("Failed to set canvas scale");
        let (width, height) = resolutions[0];
        let buffer = vec![false; (width * height) as usize];
        let redraw_requested = false;
        Self { canvas, buffer, width, height, resolutions, redraw_requested }
    }

    pub fn redraw(&mut self) {
        if !self.redraw_requested { return }
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
    }

    pub fn draw_point(&mut self, x: u32, y: u32) -> bool {
        let i = (x + y * self.width) as usize;
        let flag = self.buffer[i];
        self.buffer[i] = !self.buffer[i];
        flag
    }

    pub fn clear(&mut self) {
        for pixel in &mut self.buffer { *pixel = false }
        self.canvas.set_draw_color(OFF);
        self.canvas.clear();
    }
}
