use serde::{Deserialize, Serialize};

/// An economic good or service identifier (e.g. "€", "USD", "cows").
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Commodity(pub String);

impl Commodity {
    pub fn new(s: impl Into<String>) -> Self {
        Commodity(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Commodity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
