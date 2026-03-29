use chrono::NaiveDateTime;

#[derive(Debug, Clone, PartialEq)]
pub enum SortOrder {
    CustomSort,
    Alphabetically,
}

pub fn date_show_pretty_long(dt: &NaiveDateTime) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S").to_string()
}

pub fn utc_to_iso_date_string(dt: &NaiveDateTime) -> String {
    dt.format("%Y-%m-%d").to_string()
}

/// Parse an ISO datetime string (various formats) into NaiveDateTime
pub fn parse_datetime(s: &str) -> Option<NaiveDateTime> {
    let formats = [
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M",
    ];
    for fmt in &formats {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }
    // Date-only: NaiveDateTime::parse_from_str requires a time component
    chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d")
        .ok()
        .map(|d| d.and_hms_opt(0, 0, 0).unwrap())
}
