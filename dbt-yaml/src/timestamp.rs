use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

/// Represents a YAML 1.1 timestamp.
///
/// A timestamp is either a date (`2001-12-15`) or a date and time-of-day
/// (`2001-12-15 02:59:43.1 -05:00`) with an optional UTC offset. Like the
/// rest of `Value`, this is an unvalidated container: components are stored
/// exactly as constructed and it is up to the consumer to reject out-of-range
/// values.
///
/// Per the YAML 1.1 spec, a missing time-of-day means midnight and a missing
/// UTC offset means UTC, so every `Timestamp` denotes a single instant.
/// Equality, ordering and hashing operate on this UTC-normalized instant, so
/// different spellings of the same instant, such as `2001-12-15` and
/// `2001-12-15 00:00:00 Z`, compare equal.
///
/// The [Display] implementation emits a canonicalized format: `YYYY-MM-DD` for
/// date-only values and `YYYY-MM-DD HH:MM:SS[.ffffff]` otherwise, with the zone
/// suffix as specified: nothing when omitted, `Z` for a zero offset and
/// `±HH:MM` otherwise.
///
/// ```
/// # use dbt_yaml::{TimeOfDay, Timestamp};
/// let date = Timestamp::new(2001, 12, 15, None, None);
/// let midnight = Timestamp::new(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
/// assert_eq!(date, midnight);
/// assert_eq!(date.to_string(), "2001-12-15");
/// assert_eq!(midnight.to_string(), "2001-12-15 00:00:00Z");
/// ```
#[derive(Clone, Copy)]
pub struct Timestamp {
    year: i32,
    month: u8,
    day: u8,
    /// Time-of-day; `None` for date-only values.
    time: Option<TimeOfDay>,
    /// Timezone, represented as an offset from UTC in minutes; `None` if no
    /// zone suffix was given.
    tz_minutes: Option<i32>,
}

/// The time-of-day component of a [`Timestamp`].
///
/// Like [`Timestamp`], this is an unvalidated container: fields are stored
/// exactly as constructed and may be out of range.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TimeOfDay {
    /// Hour, 0-23.
    pub hour: u8,
    /// Minute, 0-59.
    pub minute: u8,
    /// Second, 0-59.
    pub second: u8,
    /// Nanosecond, 0-999_999_999.
    pub nanosecond: u32,
}

impl TimeOfDay {
    /// Midnight, the spec-mandated time-of-day for date-only timestamps.
    pub const MIDNIGHT: TimeOfDay = TimeOfDay::new(0, 0, 0, 0);

    /// Constructs a time-of-day from its components. No validation is
    /// performed; out-of-range components are stored as given.
    pub const fn new(hour: u8, minute: u8, second: u8, nanosecond: u32) -> TimeOfDay {
        TimeOfDay {
            hour,
            minute,
            second,
            nanosecond,
        }
    }
}

impl Timestamp {
    /// Constructs a timestamp from its components. No validation is
    /// performed; out-of-range components are stored as given.
    pub const fn new(
        year: i32,
        month: u8,
        day: u8,
        time: Option<TimeOfDay>,
        tz_minutes: Option<i32>,
    ) -> Timestamp {
        Timestamp {
            year,
            month,
            day,
            time,
            tz_minutes,
        }
    }

    /// The year, month and day of the timestamp as written.
    pub fn date(&self) -> (i32, u8, u8) {
        (self.year, self.month, self.day)
    }

    /// The time-of-day, or `None` for date-only timestamps.
    pub fn time(&self) -> Option<TimeOfDay> {
        self.time
    }

    /// True if the timestamp has no time-of-day component.
    pub fn is_date_only(&self) -> bool {
        self.time.is_none()
    }

    /// The offset from UTC in minutes, or `None` if the timestamp did not
    /// specify a zone suffix. Per the YAML 1.1 spec an omitted zone means
    /// UTC, i.e. an offset of zero.
    pub fn tz_minutes(&self) -> Option<i32> {
        self.tz_minutes
    }

    /// The instant as (minutes since the Unix epoch in UTC, second,
    /// nanosecond), applying the spec's defaults of midnight for a missing
    /// time-of-day and UTC for a missing zone.
    fn normalized(&self) -> (i64, u8, u32) {
        let days = days_from_civil(self.year, self.month, self.day);
        let time = self.time.unwrap_or(TimeOfDay::MIDNIGHT);
        let offset = i64::from(self.tz_minutes.unwrap_or(0));
        let minutes = days * 1440 + i64::from(time.hour) * 60 + i64::from(time.minute) - offset;
        (minutes, time.second, time.nanosecond)
    }
}

impl PartialEq for Timestamp {
    fn eq(&self, other: &Self) -> bool {
        self.normalized() == other.normalized()
    }
}

impl Eq for Timestamp {}

impl PartialOrd for Timestamp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Timestamp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.normalized().cmp(&other.normalized())
    }
}

impl Hash for Timestamp {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.normalized().hash(state);
    }
}

impl Display for Timestamp {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{:04}-{:02}-{:02}",
            self.year, self.month, self.day
        )?;
        let Some(time) = self.time else {
            return Ok(());
        };
        write!(
            formatter,
            " {:02}:{:02}:{:02}",
            time.hour, time.minute, time.second
        )?;
        let micros = time.nanosecond / 1000;
        if micros != 0 {
            write!(formatter, ".{:06}", micros)?;
        }
        match self.tz_minutes {
            None => {}
            Some(0) => formatter.write_str("Z")?,
            Some(offset) => {
                let sign = if offset < 0 { '-' } else { '+' };
                let abs = offset.unsigned_abs();
                write!(formatter, "{}{:02}:{:02}", sign, abs / 60, abs % 60)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Timestamp {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "Timestamp({})", self)
    }
}

/// Days since the Unix epoch for a date in the proleptic Gregorian calendar.
/// Howard Hinnant's `days_from_civil` algorithm.
fn days_from_civil(year: i32, month: u8, day: u8) -> i64 {
    let year = if month <= 2 {
        i64::from(year) - 1
    } else {
        i64::from(year)
    };
    let era = if year >= 0 { year } else { year - 399 } / 400;
    let yoe = year - era * 400; // [0, 399]
    let mp = (i64::from(month) + 9) % 12; // [0, 11]
    let doy = (153 * mp + 2) / 5 + i64::from(day) - 1; // [0, 365]
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy; // [0, 146096]
    era * 146097 + doe - 719468
}


#[cfg(test)]
mod tests {
    use super::*;

    fn ts(
        year: i32,
        month: u8,
        day: u8,
        time: Option<TimeOfDay>,
        tz_minutes: Option<i32>,
    ) -> Timestamp {
        Timestamp::new(year, month, day, time, tz_minutes)
    }

    #[test]
    fn omitted_time_and_zone_use_spec_defaults() {
        let date = ts(2001, 12, 15, None, None);
        let midnight_naive = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), None);
        let midnight_zulu = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        assert_eq!(date, midnight_naive);
        assert_eq!(date, midnight_zulu);
        assert_eq!(date.cmp(&midnight_zulu), Ordering::Equal);
    }

    #[test]
    fn comparison_normalizes_offsets() {
        let plus_two = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let zulu = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        let minus_five = ts(2001, 12, 14, Some(TimeOfDay::new(19, 0, 0, 0)), Some(-5 * 60));
        assert_eq!(plus_two, zulu);
        assert_eq!(plus_two, minus_five);
        assert!(ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 1)), None) > zulu);
        assert!(ts(2001, 12, 14, Some(TimeOfDay::new(23, 59, 59, 0)), None) < zulu);
    }

    #[test]
    fn hash_is_consistent_with_eq() {
        use std::collections::hash_map::DefaultHasher;
        fn hash(t: &Timestamp) -> u64 {
            let mut hasher = DefaultHasher::new();
            t.hash(&mut hasher);
            hasher.finish()
        }
        let a = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let b = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), None);
        assert_eq!(a, b);
        assert_eq!(hash(&a), hash(&b));
    }

    #[test]
    fn display_canonicalizes_format_but_retains_zone() {
        assert_eq!(ts(2001, 12, 15, None, None).to_string(), "2001-12-15");
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), None).to_string(),
            "2001-12-15 02:59:43"
        );
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 100_000_000)), None).to_string(),
            "2001-12-15 02:59:43.100000"
        );
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), Some(0)).to_string(),
            "2001-12-15 02:59:43Z"
        );
        // The zone is displayed as specified; the instant is not normalized.
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), Some(-5 * 60)).to_string(),
            "2001-12-15 02:59:43-05:00"
        );
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 30, 0, 0)), Some(5 * 60 + 30)).to_string(),
            "2001-12-15 02:30:00+05:30"
        );
        // Timestamps denoting the same instant can display differently.
        let a = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let b = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        assert_eq!(a, b);
        assert_ne!(a.to_string(), b.to_string());
    }
}
