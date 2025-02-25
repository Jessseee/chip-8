use sdl3::pixels::Color;
use sdl3::render::{Canvas, FPoint};
use sdl3::video::Window;

pub(crate) struct Screen {
    canvas: Canvas<Window>,
    buffer: Vec<bool>,
    width: u32,
    height: u32,
    redraw_requested: bool,
}

const ON: Color = Color::RGBA(0xFF, 0xFF, 0xFF, 0xFF);
const OFF: Color = Color::RGBA(0x0, 0x0, 0x0, 0xFF);

impl Screen {
    pub fn new(mut canvas: Canvas<Window>, scale: f32) -> Self {
        canvas.set_scale(scale, scale)
            .expect("Failed to set canvas scale");
        let (width, height) = canvas.window().size();
        let buffer = vec![false; (width * height) as usize];
        let redraw_requested = false;
        Self { canvas, buffer, width, height, redraw_requested }
    }

    pub fn redraw(&mut self) {
        if !self.redraw_requested { return }
        self.canvas.set_draw_color(OFF);
        self.canvas.clear();
        let points: Vec<FPoint> = self.buffer.iter()
            .enumerate()
            .filter_map(|(i, &pixel)| {
                if !pixel { return None }
                let x = i % self.width as usize;
                let y = i / self.width as usize;
                Some(FPoint::new(x as f32, y as f32))
            }).collect();
        self.canvas.set_draw_color(ON);
        self.canvas.draw_points(points.as_slice()).unwrap();
        self.canvas.present();
        self.redraw_requested = false;
    }

    pub fn draw_point(&mut self, x: u32, y: u32) {
        let i = (x + y * self.width) as usize;
        self.buffer[i] = !self.buffer[i];
        self.redraw_requested = true;
    }

    pub fn clear(&mut self) {
        for pixel in &mut self.buffer { *pixel = false }
        self.canvas.set_draw_color(OFF);
        self.canvas.clear();
    }
}
