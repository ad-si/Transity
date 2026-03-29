/// Flexible NaiveDateTime deserializer supporting multiple formats used in journal YAML:
/// - '2017-12-24'            (date only → midnight)
/// - '2017-12-24 15:00'      (no seconds)
/// - '2017-12-24 15:00:00'   (full)

use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use serde::{Deserialize, Deserializer, Serializer};

fn parse_flexible(s: &str) -> Option<NaiveDateTime> {
    const FORMATS: &[&str] = &["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"];
    for fmt in FORMATS {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }
    // Date-only: treat as midnight
    if let Ok(d) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        return Some(d.and_time(NaiveTime::MIN));
    }
    None
}

pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<NaiveDateTime, D::Error> {
    let s = String::deserialize(deserializer)?;
    parse_flexible(&s).ok_or_else(|| {
        serde::de::Error::custom(format!(
            "Cannot parse datetime {:?}; expected e.g. '2017-12-24' or '2017-12-24 15:00'",
            s
        ))
    })
}

fn format_dt(dt: &NaiveDateTime) -> String {
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

pub fn serialize<S: Serializer>(dt: &NaiveDateTime, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&format_dt(dt))
}

pub mod option {
    use super::*;

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<NaiveDateTime>, D::Error> {
        let opt = Option::<String>::deserialize(deserializer)?;
        match opt {
            None => Ok(None),
            Some(s) => parse_flexible(&s)
                .map(Some)
                .ok_or_else(|| serde::de::Error::custom(format!("Cannot parse datetime {:?}", s))),
        }
    }

    pub fn serialize<S: Serializer>(
        dt: &Option<NaiveDateTime>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match dt {
            None => serializer.serialize_none(),
            Some(dt) => serializer.serialize_str(&super::format_dt(dt)),
        }
    }
}
