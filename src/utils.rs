use chrono::{DateTime, NaiveDate, NaiveDateTime, TimeZone, Utc};
use colored::Colorize;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::str::FromStr;

use crate::data::config::ColorFlag;

/// Flag to switch between different ways of sorting the output
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SortOrder {
    CustomSort,
    Alphabetically,
}

/// Column widths for alignment
#[derive(Debug, Clone, Default)]
pub struct WidthRecord {
    pub account: usize,
    pub integer: usize,
    pub fraction: usize,
    pub commodity: usize,
}

pub fn merge_width_records(a: WidthRecord, b: WidthRecord) -> WidthRecord {
    WidthRecord {
        account: a.account.max(b.account),
        integer: a.integer.max(b.integer),
        fraction: a.fraction.max(b.fraction),
        commodity: a.commodity.max(b.commodity),
    }
}

/// Parse a decimal string into an exact rational number.
///
/// All characters must be digits, '.', '-', or '+'.
/// Mirrors PureScript's `digitsToRational`: removes the dot to form the numerator,
/// then sets denominator = 10^(digits_after_dot).
pub fn digits_to_rational(s: &str) -> Option<BigRational> {
    if s.is_empty() {
        return None;
    }
    if !s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+') {
        return None;
    }

    let numerator_str: String = s.chars().filter(|&c| c != '.').collect();
    let dot_index = s.find('.').unwrap_or(s.len());
    let exp = numerator_str.len() - dot_index;
    let denom = BigInt::from(10u32).pow(exp as u32);
    let numer = BigInt::from_str(&numerator_str).ok()?;

    Some(BigRational::new(numer, denom))
}

/// Right-align a string by padding the left with spaces.
pub fn pad_start(target: usize, s: &str) -> String {
    let len = s.chars().count();
    if len >= target {
        s.to_string()
    } else {
        format!("{}{}", " ".repeat(target - len), s)
    }
}

/// Left-align a string by padding the right with spaces.
pub fn pad_end(target: usize, s: &str) -> String {
    let len = s.chars().count();
    if len >= target {
        s.to_string()
    } else {
        format!("{}{}", s, " ".repeat(target - len))
    }
}

/// Returns `(int_chars, frac_chars)` where `frac_chars` includes the '.' character.
///
/// Mirrors PureScript's `lengthOfNumParts`.
pub fn length_of_num_parts(n: f64) -> (usize, usize) {
    let s = format_f64_like_purescript(n);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_len = parts[0].chars().count();
    let frac_len = match parts.get(1) {
        Some(&frac) if frac != "0" => frac.chars().count() + 1,
        _ => 0,
    };
    (int_len, frac_len)
}

/// Format a number aligned to given column widths.
///
/// `int_w`: total width of the integer part, right-aligned.
/// `frac_w`: total width of the fractional part (including '.'), left-aligned.
/// Mirrors PureScript's `alignNumber`.
pub fn align_number(color: ColorFlag, int_w: usize, frac_w: usize, n: f64) -> String {
    let s = format_f64_like_purescript(n);
    let parts: Vec<&str> = s.splitn(2, '.').collect();

    let int_str = parts[0];
    let int_part = pad_start(int_w, int_str);

    let frac_part = match parts.get(1) {
        Some(&frac) if frac != "0" => {
            let with_dot = format!(".{}", frac);
            pad_end(frac_w, &with_dot)
        }
        _ => " ".repeat(frac_w),
    };

    if color == ColorFlag::No {
        format!("{}{}", int_part, frac_part)
    } else if n >= 0.0 {
        format!("{}{}", int_part.green(), frac_part.bright_black())
    } else {
        format!("{}{}", int_part.red(), frac_part.bright_black())
    }
}

/// Indent all lines after the first by `indent` spaces.
pub fn indent_subsequent(indent: usize, s: &str) -> String {
    s.replace('\n', &format!("\n{}", " ".repeat(indent)))
}

/// Format as "YYYY-MM-DDTHH:MM:SS"
pub fn utc_to_iso_string(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S").to_string()
}

/// Format as "YYYY-MM-DD"
pub fn utc_to_iso_date_string(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d").to_string()
}

/// Format as "YYYY-MM-DD HH:MM"
pub fn date_show_pretty(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d %H:%M").to_string()
}

/// Format as "YYYY-MM-DD HH:MM:SS"
pub fn date_show_pretty_long(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Parse a datetime string in UTC. Accepts ISO 8601 with T or space separator,
/// with or without seconds, or date-only (treated as midnight UTC).
pub fn parse_datetime(s: &str) -> Option<DateTime<Utc>> {
    let formats = [
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
    ];
    for fmt in &formats {
        if let Ok(ndt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(Utc.from_utc_datetime(&ndt));
        }
    }
    // Date-only: midnight UTC
    if let Ok(nd) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        let ndt = nd.and_hms_opt(0, 0, 0)?;
        return Some(Utc.from_utc_datetime(&ndt));
    }
    None
}

/// Format an f64 the same way PureScript's `show` does for Number.
///
/// Integers always include ".0" (e.g. "42.0"), decimals keep minimal digits (e.g. "3.14").
/// Rust's Display for f64 omits ".0" for whole numbers, so we add it back.
fn format_f64_like_purescript(n: f64) -> String {
    let s = format!("{}", n);
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{}.0", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::BigInt;
    use num_rational::BigRational;

    fn ratio(numer: i64, denom: i64) -> BigRational {
        BigRational::new(BigInt::from(numer), BigInt::from(denom))
    }

    #[test]
    fn test_digits_to_rational_integer() {
        assert_eq!(digits_to_rational("137"), Some(ratio(137, 1)));
    }

    #[test]
    fn test_digits_to_rational_13() {
        assert_eq!(digits_to_rational("13"), Some(ratio(13, 1)));
    }

    #[test]
    fn test_digits_to_rational_3() {
        assert_eq!(digits_to_rational("3"), Some(ratio(3, 1)));
    }

    #[test]
    fn test_digits_to_rational_zero() {
        assert_eq!(digits_to_rational("0"), Some(ratio(0, 1)));
    }

    #[test]
    fn test_digits_to_rational_0_3() {
        assert_eq!(digits_to_rational("0.3"), Some(ratio(3, 10)));
    }

    #[test]
    fn test_digits_to_rational_dot_3() {
        assert_eq!(digits_to_rational(".3"), Some(ratio(3, 10)));
    }

    #[test]
    fn test_digits_to_rational_0_300() {
        assert_eq!(digits_to_rational("0.300"), Some(ratio(3, 10)));
    }

    #[test]
    fn test_digits_to_rational_2_1() {
        assert_eq!(digits_to_rational("2.1"), Some(ratio(21, 10)));
    }

    #[test]
    fn test_digits_to_rational_3_21() {
        assert_eq!(digits_to_rational("3.21"), Some(ratio(321, 100)));
    }

    #[test]
    fn test_digits_to_rational_66_05() {
        assert_eq!(digits_to_rational("66.05"), Some(ratio(6605, 100)));
    }

    #[test]
    fn test_digits_to_rational_12_3456() {
        assert_eq!(digits_to_rational("12.3456"), Some(ratio(123456, 10000)));
    }

    #[test]
    fn test_digits_to_rational_neg_0_3() {
        assert_eq!(digits_to_rational("-0.3"), Some(ratio(-3, 10)));
    }

    #[test]
    fn test_digits_to_rational_neg_7_8() {
        assert_eq!(digits_to_rational("-7.8"), Some(ratio(-78, 10)));
    }

    #[test]
    fn test_digits_to_rational_abc() {
        assert_eq!(digits_to_rational("abc"), None);
    }

    #[test]
    fn test_digits_to_rational_big() {
        // "1.5555555555" (1. followed by 10 fives)
        let digits = format!("1.{}", "5".repeat(10));
        let result = digits_to_rational(&digits);
        assert!(result.is_some());
        let expected = BigRational::new(
            BigInt::from_str("15555555555").unwrap(),
            BigInt::from_str("10000000000").unwrap(),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_pad_start() {
        assert_eq!(pad_start(5, "ab"), "   ab");
    }

    #[test]
    fn test_pad_end() {
        assert_eq!(pad_end(5, "ab"), "ab   ");
    }

    #[test]
    fn test_pad_start_no_op() {
        assert_eq!(pad_start(2, "hello"), "hello");
    }

    #[test]
    fn test_pad_end_no_op() {
        assert_eq!(pad_end(2, "hello"), "hello");
    }

    #[test]
    fn test_length_of_num_parts_integer() {
        let (i, f) = length_of_num_parts(42.0);
        assert_eq!(i, 2);
        assert_eq!(f, 0);
    }

    #[test]
    fn test_length_of_num_parts_decimal() {
        let (i, f) = length_of_num_parts(3.14);
        assert_eq!(i, 1);
        assert_eq!(f, 3); // ".14" = 3 chars
    }

    #[test]
    fn test_align_number_positive() {
        let result = align_number(ColorFlag::No, 5, 3, 42.5);
        assert!(result.contains("42"), "expected '42' in '{}'", result);
        assert!(result.contains(".5"), "expected '.5' in '{}'", result);
    }

    #[test]
    fn test_align_number_negative() {
        let result = align_number(ColorFlag::No, 5, 3, -7.0);
        assert!(result.contains("-7"), "expected '-7' in '{}'", result);
    }

    #[test]
    fn test_align_number_66_05() {
        let result = align_number(ColorFlag::No, 5, 3, 66.05);
        assert_eq!(result, "   66.05");
    }

    #[test]
    fn test_align_number_7_8_frac4() {
        let result = align_number(ColorFlag::No, 5, 4, 7.8);
        assert_eq!(result, "    7.8  ");
    }

    #[test]
    fn test_align_number_integer_no_frac() {
        let result = align_number(ColorFlag::No, 5, 3, 42.0);
        assert_eq!(result, "   42   ");
    }

    #[test]
    fn test_indent_subsequent() {
        let result = indent_subsequent(2, "line1\nline2\nline3");
        assert_eq!(result, "line1\n  line2\n  line3");
    }

    #[test]
    fn test_utc_to_iso_string() {
        let dt = parse_datetime("2014-12-24 10:30:45").unwrap();
        assert_eq!(utc_to_iso_string(&dt), "2014-12-24T10:30:45");
    }

    #[test]
    fn test_utc_to_iso_date_string() {
        let dt = parse_datetime("2014-12-24 10:30:45").unwrap();
        assert_eq!(utc_to_iso_date_string(&dt), "2014-12-24");
    }

    #[test]
    fn test_date_show_pretty() {
        let dt = parse_datetime("2014-12-24 10:30:00").unwrap();
        assert_eq!(date_show_pretty(&dt), "2014-12-24 10:30");
    }

    #[test]
    fn test_date_show_pretty_long() {
        let dt = parse_datetime("2014-12-24 10:30:45").unwrap();
        assert_eq!(date_show_pretty_long(&dt), "2014-12-24 10:30:45");
    }

    #[test]
    fn test_parse_datetime_date_only() {
        let dt = parse_datetime("2017-02-17").unwrap();
        assert_eq!(utc_to_iso_date_string(&dt), "2017-02-17");
    }

    #[test]
    fn test_merge_width_records() {
        let a = WidthRecord { account: 5, integer: 3, fraction: 0, commodity: 0 };
        let b = WidthRecord { account: 2, integer: 7, fraction: 0, commodity: 0 };
        let m = merge_width_records(a, b);
        assert_eq!(m.account, 5);
        assert_eq!(m.integer, 7);
    }
}
