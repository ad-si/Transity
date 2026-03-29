/// Flag to switch colorized output on or off
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColorFlag {
    Yes,
    No,
}

pub struct Config {
    pub color: ColorFlag,
}

impl Default for Config {
    fn default() -> Self {
        Config { color: ColorFlag::Yes }
    }
}
