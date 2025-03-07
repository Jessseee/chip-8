use sdl3::audio::{AudioCallback, AudioFormat, AudioSpec, AudioStreamWithCallback};
use sdl3::AudioSubsystem;

pub struct Audio {
    device: AudioStreamWithCallback<SquareWave>,
    beeping: bool,
}

struct SquareWave {
    phase_inc: f32,
    phase: f32,
    volume: f32,
}

impl AudioCallback<f32> for SquareWave {
    fn callback(&mut self, out: &mut [f32]) {
        for x in out.iter_mut() {
            *x = if self.phase <= 0.5 {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

impl Audio {
    pub fn new(audio_subsystem: AudioSubsystem) -> Self {
        let desired_spec = AudioSpec {
            freq: Some(44_100),
            channels: Some(1),
            format: Some(AudioFormat::f32_sys()),
        };

        let device = audio_subsystem.open_playback_stream(
            &desired_spec,
            SquareWave {
                phase_inc: 220.0 / desired_spec.freq.unwrap() as f32,
                phase: 0.0,
                volume: 0.05,
            },
        ).expect("Failed to open audio playback device.");
        let beeping = false;
        Self { device, beeping }
    }

    pub fn start_beep(&mut self) {
        if !self.beeping {
            self.device.resume().expect("Failed to resume audio.");
            self.beeping = true;
        }
    }

    pub fn stop_beep(&mut self) {
        if self.beeping {
            self.device.pause().expect("Failed to pause audio.");
            self.beeping = false;
        }
    }
}
