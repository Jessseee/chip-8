use sdl3::pixels::Color;
use sdl3::render::{Canvas, FPoint};
use sdl3::video::Window;

pub struct Screen {
    pub width: u32,
    pub height: u32,
    pub redraw_requested: bool,
    pub canvas: Canvas<Window>,
    pub planes: [Vec<bool>; 4],
    pub selected_planes: Vec<usize>,
    resolutions: Vec<(u32, u32)>,
    current_resolution: usize,
}

impl Screen {
    pub fn new(window: Window, resolutions: Vec<(u32, u32)>, scale: f32) -> Self {
        let (width, height) = *resolutions.first().unwrap();
        let mut canvas = window.into_canvas();
        canvas.set_scale(scale, scale)
            .expect("Failed to set canvas scale");
        let planes = core::array::from_fn(|_| vec![false; (width * height) as usize]);
        let redraw_requested = false;
        let current_resolution = 0;
        let selected_planes = vec![0];
        Self { canvas, planes, selected_planes, resolutions, current_resolution, width, height, redraw_requested }
    }

    pub fn color_from_bitmask(bitmask: u8) -> Color {
        match bitmask {
            0b0000 => Color::RGB(0x13, 0x16, 0x26),
            0b0001 => Color::RGB(0x4D, 0x4D, 0x80),
            0b0010 => Color::RGB(0xE6, 0xA1, 0xCF),
            0b0011 => Color::RGB(0xFF, 0xE6, 0xEA),
            _ => panic!("Unknown color!")
        }
    }

    pub fn redraw(&mut self) -> bool {
        if !self.redraw_requested { return false }
        self.canvas.set_draw_color(Self::color_from_bitmask(0b0000));
        self.canvas.clear();
        let (max_width, max_height) = self.resolutions.last().unwrap();
        let (width, height) = (self.width, self.height);
        let (offset_x, offset_y) = ((max_width - width) / 2, (max_height - height) / 2);
        for i in 0..width * height {
            let color_bitmask: u8 = (0..4)
                .fold(0b0000, |acc, n| {
                    let pixel = self.planes[n][i as usize] as u8;
                    acc | (pixel << n)
                });
            if color_bitmask > 0 {
                let x = i % width + offset_x;
                let y = i / width + offset_y;
                let point = FPoint::new(x as f32, y as f32);
                self.canvas.set_draw_color(Self::color_from_bitmask(color_bitmask));
                self.canvas.draw_point(point).expect("Failed to draw pixel.")
            }
        }
        self.canvas.present();
        self.redraw_requested = false;
        true
    }

    pub fn enable_hires(&mut self) {
        if self.current_resolution < self.resolutions.len() {
            self.current_resolution += 1;
            (self.width, self.height) = self.resolutions[self.current_resolution];
            self.planes = core::array::from_fn(|_| vec![false; (self.width * self.height) as usize]);
        }
    }

    pub fn disable_hires(&mut self) {
        if self.current_resolution > 0 {
            self.current_resolution -= 1;
            (self.width, self.height) = self.resolutions[self.current_resolution];
            self.planes = core::array::from_fn(|_| vec![false; (self.width * self.height) as usize]);
        }
    }

    pub fn select_planes(&mut self, bitmask: u16) {
        self.selected_planes = (0..self.planes.len()).enumerate().filter_map(|(i, _)| {
            if bitmask & (1 << i) as u16 == 1 { Some(i) }
            else { None }
        }).collect()
    }

    pub fn scroll_down(&mut self, n: u8) {
        let (width, height) = (self.width, self.height);
        for &plane_index in self.selected_planes.iter() {
            let plane = &mut self.planes[plane_index];
            let mut new_plane = vec![false; (width * height) as usize];
            for y in 0..(height - n as u32) {
                for x in 0..width {
                    let i = (x + y * width) as usize;
                    let ni = (x + (y + n as u32) * width) as usize;
                    new_plane[ni] = plane[i]
                }
            }
            *plane = new_plane;
        }
        self.redraw_requested = true;
    }

    pub fn scroll_up(&mut self, n: u8) {
        let (width, height) = (self.width, self.height);
        for &plane_index in self.selected_planes.iter() {
            let plane = &mut self.planes[plane_index];
            let mut new_plane = vec![false; (width * height) as usize];
            for y in (n as u32)..height {
                for x in 0..width {
                    let i = (x + y * width) as usize;
                    let ni = (x + (y - n as u32) * width) as usize;
                    new_plane[ni] = plane[i]
                }
            }
            *plane = new_plane;
        }
        self.redraw_requested = true;
    }

    pub fn scroll_right(&mut self) {
        let (width, height) = (self.width, self.height);
        for &plane_index in self.selected_planes.iter() {
            let plane = &mut self.planes[plane_index];
            let mut new_plane = vec![false; (width * height) as usize];
            for y in 0..height {
                for x in 0..(width - 4) {
                    let i = (x + y * width) as usize;
                    let ni = ((x + 4) + y * width) as usize;
                    new_plane[ni] = plane[i]
                }
            }
            *plane = new_plane;
        }
        self.redraw_requested = true;
    }

    pub fn scroll_left(&mut self) {
        let (width, height) = (self.width, self.height);
        for &plane_index in self.selected_planes.iter() {
            let plane = &mut self.planes[plane_index];
            let mut new_plane = vec![false; (width * height) as usize];
            for y in 0..height {
                for x in 4..width {
                    let i = (x + y * width) as usize;
                    let ni = ((x - 4) + y * width) as usize;
                    new_plane[ni] = plane[i]
                }
            }
            *plane = new_plane;
        }
        self.redraw_requested = true;
    }

    pub fn draw_sprite(&mut self, x: u32, y: u32, sprite: &[u8], plane: usize) -> bool {
        let mut flag = false;
        for dy in 0..sprite.len() as u32 {
            let row = sprite[dy as usize];
            if y + dy >= (self.height) { break; }
            for dx in 0..8 {
                if x + dx >= (self.width) { break; }
                if (row >> (7 - dx)) & 1 == 1 {
                    flag = self.toggle_pixel(
                        x + dx,
                        y + dy,
                        plane
                    ) || flag;
                }
            }
        }
        self.redraw_requested = true;
        flag
    }

    pub fn draw_sprite_wrap(&mut self, x: u32, y: u32, sprite: &[u8], plane: usize) -> bool {
        let mut flag = false;
        for dy in 0..sprite.len() as u32 {
            let row = sprite[dy as usize];
            for dx in 0..8 {
                if (row >> (7 - dx)) & 1 == 1 {
                    flag = self.toggle_pixel(
                        (x + dx) % self.width,
                        (y + dy) % self.height,
                        plane
                    ) || flag;
                }
            }
        }
        self.redraw_requested = true;
        flag
    }

    pub fn draw_sprite_large(&mut self, x: u32, y: u32, sprite: &[u16], plane: usize) -> bool {
        let mut flag = false;
        for dy in 0..sprite.len() as u32 {
            let row = sprite[dy as usize];
            println!("{:08b}", row);
            for dx in 0..16 {
                if (row >> (15 - dx)) & 1 == 1 {
                    flag = self.toggle_pixel(
                        (x + dx) % self.width,
                        (y + dy) % self.height,
                        plane
                    ) || flag;
                }
            }
        }
        self.redraw_requested = true;
        flag
    }

    fn toggle_pixel(&mut self, x: u32, y: u32, plane: usize) -> bool {
        let i = (x + y * self.width) as usize;
        let plane = &mut self.planes[plane];
        let flag = plane[i];
        plane[i] = !plane[i];
        flag
    }

    pub fn clear(&mut self) {
        let (width, height) = (self.width, self.height);
        for &plane_index in self.selected_planes.iter() {
            self.planes[plane_index] = vec![false; (width * height) as usize];
        }
        self.redraw_requested = true;
    }
}
