/// Flag to switch colorized output on or off.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorFlag {
    ColorYes,
    ColorNo,
}

impl Default for ColorFlag {
    fn default() -> Self {
        ColorFlag::ColorNo
    }
}
