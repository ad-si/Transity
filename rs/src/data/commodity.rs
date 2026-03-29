use serde::{Deserialize, Serialize};

/// Economic good or service, e.g. €, cows, minutes
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Commodity(pub String);

impl Commodity {
    pub fn new(s: impl Into<String>) -> Self {
        Commodity(s.into())
    }
}

impl std::fmt::Display for Commodity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
