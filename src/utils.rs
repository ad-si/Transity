use anyhow::{anyhow, Result};
use chrono::{DateTime, NaiveDate, NaiveDateTime, TimeZone, Utc};
use serde::{de, Deserialize, Deserializer};

pub fn date_show_pretty(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d %H:%M").to_string()
}

pub fn indent_subsequent(indentation: usize, s: &str) -> String {
    let pad = " ".repeat(indentation);
    s.replace('\n', &format!("\n{}", pad))
}

pub fn parse_datetime_str(s: &str) -> Result<DateTime<Utc>> {
    let formats = [
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M",
    ];
    for fmt in &formats {
        if let Ok(ndt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Ok(Utc.from_utc_datetime(&ndt));
        }
    }
    if let Ok(nd) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        let ndt = nd.and_hms_opt(0, 0, 0).unwrap();
        return Ok(Utc.from_utc_datetime(&ndt));
    }
    Err(anyhow!("Cannot parse datetime: {}", s))
}

pub fn deserialize_optional_utc<'de, D>(
    d: D,
) -> std::result::Result<Option<DateTime<Utc>>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Option<String> = Option::deserialize(d)?;
    match opt {
        None => Ok(None),
        Some(s) => parse_datetime_str(&s).map(Some).map_err(de::Error::custom),
    }
}
