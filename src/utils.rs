use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::ToPrimitive;
use std::str::FromStr;

use crate::data::config::ColorFlag;
use colored::Colorize;

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

/// Convert a decimal string like "66.05" to an exact BigRational (6605/100).
/// Handles negative numbers and integers without decimal points.
pub fn digits_to_rational(s: &str) -> Option<BigRational> {
    // Validate: only digits, '.', '-', '+'
    if !s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+') {
        return None;
    }

    let dot_pos = s.find('.');
    let numerator_str = s.replace('.', "");
    let numerator = BigInt::from_str(&numerator_str).ok()?;

    let num_str_len = numerator_str
        .trim_start_matches('-')
        .trim_start_matches('+')
        .len();
    let index = match dot_pos {
        Some(i) => {
            let sign_offset = if s.starts_with('-') || s.starts_with('+') { 1 } else { 0 };
            i - sign_offset
        }
        None => num_str_len,
    };

    let exp = num_str_len - index;
    let denominator = BigInt::from(10u32).pow(exp as u32);

    Some(BigRational::new(numerator, denominator))
}

pub fn pad_start(target: usize, s: &str) -> String {
    let len = s.chars().count();
    if len >= target {
        s.to_string()
    } else {
        format!("{}{}", " ".repeat(target - len), s)
    }
}

pub fn pad_end(target: usize, s: &str) -> String {
    let len = s.chars().count();
    if len >= target {
        s.to_string()
    } else {
        format!("{}{}", s, " ".repeat(target - len))
    }
}

/// Returns (int_chars, frac_chars_including_dot)
/// Mirrors the PureScript `lengthOfNumParts`.
pub fn length_of_num_parts(n: f64) -> (usize, usize) {
    let s = format!("{}", n);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_len = parts[0].chars().count();
    let frac_len = match parts.get(1) {
        Some(frac) if *frac != "0" => frac.chars().count() + 1, // +1 for the dot
        _ => 0,
    };
    (int_len, frac_len)
}

pub fn align_number(color: ColorFlag, int_w: usize, frac_w: usize, n: f64) -> String {
    let s = format!("{}", n);
    let parts: Vec<&str> = s.splitn(2, '.').collect();

    let int_part = pad_start(int_w, parts[0]);
    let frac_part = match parts.get(1) {
        Some(frac) if *frac != "0" => pad_end(frac_w, &format!(".{}", frac)),
        _ => " ".repeat(frac_w),
    };

    match color {
        ColorFlag::No => format!("{}{}", int_part, frac_part),
        ColorFlag::Yes => {
            if n >= 0.0 {
                format!(
                    "{}{}",
                    int_part.green(),
                    frac_part.bright_black()
                )
            } else {
                format!(
                    "{}{}",
                    int_part.red(),
                    frac_part.bright_black()
                )
            }
        }
    }
}

/// Convert a BigRational to f64 for display purposes.
pub fn rational_to_f64(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or(0.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::BigInt;

    #[test]
    fn test_digits_to_rational() {
        let r = digits_to_rational("66.05").unwrap();
        // BigRational reduces: 6605/100 == 1321/20
        use num_rational::BigRational;
        assert_eq!(r, BigRational::new(BigInt::from(6605), BigInt::from(100)));
    }

    #[test]
    fn test_digits_to_rational_negative() {
        let r = digits_to_rational("-7.8").unwrap();
        // -78/10 == -39/5
        use num_rational::BigRational;
        assert_eq!(r, BigRational::new(BigInt::from(-78), BigInt::from(10)));
    }

    #[test]
    fn test_digits_to_rational_integer() {
        let r = digits_to_rational("20").unwrap();
        assert_eq!(*r.numer(), BigInt::from(20));
        assert_eq!(*r.denom(), BigInt::from(1));
    }

    #[test]
    fn test_pad_start() {
        assert_eq!(pad_start(5, "abc"), "  abc");
        assert_eq!(pad_start(2, "abc"), "abc");
    }

    #[test]
    fn test_pad_end() {
        assert_eq!(pad_end(5, "abc"), "abc  ");
        assert_eq!(pad_end(2, "abc"), "abc");
    }

    #[test]
    fn test_length_of_num_parts() {
        assert_eq!(length_of_num_parts(66.05), (2, 3)); // "66" → 2, ".05" → 3
        assert_eq!(length_of_num_parts(7.0), (1, 0)); // integer
        assert_eq!(length_of_num_parts(-7.8), (2, 2)); // "-7" → 2, ".8" → 2
    }
}
