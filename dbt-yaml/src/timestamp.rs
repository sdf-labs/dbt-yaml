use crate::value::tagged::TagStringVisitor;
use serde::de::value::{I32Deserializer, SeqDeserializer, StrDeserializer, U8Deserializer};
use serde::de::{
    self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, Unexpected, VariantAccess, Visitor,
};
use serde::ser;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

/// The enum variant name used to transport a resolved timestamp through the
/// serde data model, which has no timestamp type. The deserializer presents
/// the timestamp as an enum with this token as the variant name and the
/// components as the variant's struct fields; `ValueVisitor` recognizes the
/// token and produces a `Value::Timestamp`. This mirrors the pattern
/// serde_json uses for arbitrary-precision numbers.
pub(crate) const TOKEN: &str = "$dbt_yaml::private::Timestamp";

/// The struct field names of a timestamp's component form, in order.
pub(crate) const FIELDS: &[&str] = &["year", "month", "day", "time", "tz_minutes"];

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
/// A `Timestamp` also compares equal to strings that parse as a timestamp for
/// the same normalized instant: [`str`], `&str` and [`String`] are supported in
/// either operand order, and strings that do not parse are never equal. Note
/// that [`Value`](crate::Value) does not use this comparison — its
/// [`PartialEq`](crate::Value#impl-PartialEq-for-Value) keeps timestamps
/// strictly distinct from strings. Lenient comparison is available on `Value`
/// through the explicit [`Value::lenient_eq`](crate::Value::lenient_eq) method.
///
/// The [Display] implementation emits a canonicalized format: `YYYY-MM-DD` for
/// date-only values and `YYYY-MM-DDTHH:MM:SS[.fffffffff]` otherwise, using the
/// RFC-3339 / ISO-8601 `T` separator, which the YAML 1.1 timestamp grammar
/// allows alongside whitespace. The fractional second is given at 3, 6 or 9
/// digits as its precision requires, and the zone suffix as specified: nothing
/// when omitted, `Z` for a zero offset and `±HH:MM` otherwise. The canonical
/// form preserves nanosecond precision, so a `Timestamp` round-trips through
/// `Display` and [`Timestamp::parse`].
///
/// ```
/// # use dbt_yaml::{TimeOfDay, Timestamp};
/// let date = Timestamp::new(2001, 12, 15, None, None);
/// let midnight = Timestamp::new(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
/// assert_eq!(date, midnight);
/// assert_eq!(date.to_string(), "2001-12-15");
/// assert_eq!(midnight.to_string(), "2001-12-15T00:00:00Z");
/// assert_eq!(date, "2001-12-15");
/// assert_eq!("2001-12-15 02:00:00+02:00", date);
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

    /// Parses a YAML 1.1 timestamp scalar, following the grammar in
    /// <https://yaml.org/type/timestamp.html>. Returns `None` if the scalar
    /// does not match the grammar. Components are not range-checked: like
    /// [`Timestamp::new`], parsing stores out-of-range components as given,
    /// and it is up to the consumer to reject them. This also means every
    /// parsed `Timestamp` round-trips through [`Display`] and back.
    ///
    /// The fractional second is kept to nanosecond precision; digits beyond
    /// the ninth are truncated.
    pub fn parse(input: &str) -> Option<Timestamp> {
        let bytes = input.as_bytes();
        let mut pos = 0;

        let year = parse_digits(bytes, &mut pos, 4, 4)? as i32;
        parse_byte(bytes, &mut pos, b'-')?;
        let month_start = pos;
        let month = parse_digits(bytes, &mut pos, 1, 2)?;
        let month_width = pos - month_start;
        parse_byte(bytes, &mut pos, b'-')?;
        let day_start = pos;
        let day = parse_digits(bytes, &mut pos, 1, 2)?;
        let day_width = pos - day_start;

        if pos == bytes.len() {
            // The date-only form requires two-digit month and day.
            if month_width == 2 && day_width == 2 {
                return Some(Timestamp::new(year, month as u8, day as u8, None, None));
            }
            return None;
        }

        // The date and time are separated by 'T', 't', or whitespace.
        match bytes[pos] {
            b'T' | b't' => pos += 1,
            b' ' | b'\t' => skip_whitespace(bytes, &mut pos),
            _ => return None,
        }

        let hour = parse_digits(bytes, &mut pos, 1, 2)?;
        parse_byte(bytes, &mut pos, b':')?;
        let minute = parse_digits(bytes, &mut pos, 2, 2)?;
        parse_byte(bytes, &mut pos, b':')?;
        let second = parse_digits(bytes, &mut pos, 2, 2)?;

        let mut nanos = 0;
        if bytes.get(pos) == Some(&b'.') {
            pos += 1;
            let mut digits = 0;
            while let Some(digit) = bytes.get(pos).filter(|b| b.is_ascii_digit()) {
                if digits < 9 {
                    nanos = nanos * 10 + u32::from(*digit - b'0');
                    digits += 1;
                }
                pos += 1;
            }
            nanos *= 10u32.pow(9 - digits);
        }

        skip_whitespace(bytes, &mut pos);
        let tz_minutes = if pos == bytes.len() {
            None
        } else {
            match bytes[pos] {
                b'Z' => {
                    pos += 1;
                    Some(0)
                }
                sign @ (b'+' | b'-') => {
                    pos += 1;
                    let hours = parse_digits(bytes, &mut pos, 1, 2)?;
                    let minutes = if bytes.get(pos) == Some(&b':') {
                        pos += 1;
                        parse_digits(bytes, &mut pos, 2, 2)?
                    } else {
                        0
                    };
                    let offset = (hours * 60 + minutes) as i32;
                    Some(if sign == b'-' { -offset } else { offset })
                }
                _ => return None,
            }
        };
        if pos != bytes.len() {
            return None;
        }

        Some(Timestamp::new(
            year,
            month as u8,
            day as u8,
            Some(TimeOfDay::new(
                hour as u8,
                minute as u8,
                second as u8,
                nanos,
            )),
            tz_minutes,
        ))
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

    /// True if this [`Timestamp`] is naive, i.e. it did not specify a zone
    /// suffix.
    pub fn is_naive(&self) -> bool {
        self.tz_minutes.is_none()
    }

    /// The instant denoted by this [`Timestamp`] as a tuple of (minutes since
    /// the Unix epoch in UTC, second, nanosecond), applying Yaml 1.1 spec's
    /// defaults of midnight for a missing time-of-day and UTC for a missing
    /// zone.
    pub fn instant(&self) -> (i64, u8, u32) {
        let days = days_from_civil(self.year, self.month, self.day);
        let time = self.time.unwrap_or(TimeOfDay::MIDNIGHT);
        let offset = i64::from(self.tz_minutes.unwrap_or(0));
        let minutes = days * 1440 + i64::from(time.hour) * 60 + i64::from(time.minute) - offset;
        (minutes, time.second, time.nanosecond)
    }

    /// Returns a [`Timestamp`] denoting the same instant as `self`, with
    /// the spec's defaults made explicit: a missing time-of-day becomes
    /// midnight and a missing zone becomes UTC. Components that are present
    /// are preserved as is.
    ///
    /// ```
    /// # use dbt_yaml::{TimeOfDay, Timestamp};
    /// let date = Timestamp::new(2001, 12, 15, None, None);
    /// let explicit = date.with_defaults();
    /// assert_eq!(explicit.to_string(), "2001-12-15T00:00:00Z");
    /// assert_eq!(explicit.time(), Some(TimeOfDay::new(0, 0, 0, 0)));
    /// assert_eq!(explicit.tz_minutes(), Some(0));
    ///
    /// // A zone that was given is preserved, not folded into UTC.
    /// let offset = Timestamp::new(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(120));
    /// assert_eq!(offset.with_defaults().to_string(), "2001-12-15T02:00:00+02:00");
    /// ```
    pub fn with_defaults(&self) -> Self {
        Timestamp {
            time: Some(self.time.unwrap_or(TimeOfDay::MIDNIGHT)),
            tz_minutes: Some(self.tz_minutes.unwrap_or(0)),
            ..*self
        }
    }

    /// Returns the current date and time in UTC, at nanosecond precision.
    ///
    /// The returned timestamp has a zero UTC offset, so its canonical form
    /// ends in `Z`.
    ///
    /// ```
    /// # #[cfg(not(miri))] {
    /// # use dbt_yaml::Timestamp;
    /// let now = Timestamp::utc_now();
    /// assert_eq!(now.tz_minutes(), Some(0));
    /// assert!(!now.is_date_only());
    /// # }
    /// ```
    pub fn utc_now() -> Timestamp {
        let duration = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock is set before 1970");
        let seconds = duration.as_secs() as i64;
        let day_seconds = seconds.rem_euclid(86400);
        let (year, month, day) = civil_from_days(seconds.div_euclid(86400));
        Timestamp::new(
            year,
            month,
            day,
            Some(TimeOfDay::new(
                (day_seconds / 3600) as u8,
                (day_seconds / 60 % 60) as u8,
                (day_seconds % 60) as u8,
                duration.subsec_nanos(),
            )),
            Some(0),
        )
    }
}

impl PartialEq for Timestamp {
    fn eq(&self, other: &Self) -> bool {
        self.instant() == other.instant()
    }
}

impl Eq for Timestamp {}

/// Semantic equality with strings.
///
/// A timestamp compares equal to a string if the string parses as a YAML 1.1
/// timestamp for the same instant, matching the normalization rules of
/// [`Timestamp`]'s own [`PartialEq`]. Strings that do not parse are never
/// equal.
impl PartialEq<str> for Timestamp {
    fn eq(&self, other: &str) -> bool {
        Timestamp::parse(other).is_some_and(|parsed| parsed == *self)
    }
}

impl PartialEq<&str> for Timestamp {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl PartialEq<String> for Timestamp {
    fn eq(&self, other: &String) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<Timestamp> for str {
    fn eq(&self, other: &Timestamp) -> bool {
        other == self
    }
}

impl PartialEq<Timestamp> for &str {
    fn eq(&self, other: &Timestamp) -> bool {
        other == *self
    }
}

impl PartialEq<Timestamp> for String {
    fn eq(&self, other: &Timestamp) -> bool {
        other == self.as_str()
    }
}

impl PartialOrd for Timestamp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Timestamp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.instant().cmp(&other.instant())
    }
}

impl Hash for Timestamp {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.instant().hash(state);
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
        // Always use the RFC 3339 / ISO 8601 'T' separator, which the
        // YAML 1.1 timestamp grammar allows alongside whitespace.
        write!(
            formatter,
            "T{:02}:{:02}:{:02}",
            time.hour, time.minute, time.second
        )?;
        if time.nanosecond != 0 {
            // Use the smallest of 3, 6 or 9 fraction digits that keeps full
            // precision, so the width shows whether the value has milli-,
            // micro- or nanosecond precision.
            let width = if time.nanosecond % 1_000_000 == 0 {
                3
            } else if time.nanosecond % 1_000 == 0 {
                6
            } else {
                9
            };
            let fraction = time.nanosecond / 10u32.pow(9 - width);
            write!(formatter, ".{:01$}", fraction, width as usize)?;
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

/// The proleptic Gregorian date `days` days after the Unix epoch. Inverse of
/// `days_from_civil`; Howard Hinnant's `civil_from_days` algorithm.
fn civil_from_days(days: i64) -> (i32, u8, u8) {
    let days = days + 719468;
    let era = if days >= 0 { days } else { days - 146096 } / 146097;
    let doe = days - era * 146097; // [0, 146096]
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365; // [0, 399]
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let day = doy - (153 * mp + 2) / 5 + 1; // [1, 31]
    let month = if mp < 10 { mp + 3 } else { mp - 9 }; // [1, 12]
    let year = yoe + era * 400 + i64::from(month <= 2);
    (year as i32, month as u8, day as u8)
}

/// Parses between `min` and `max` ASCII digits. Digits beyond `max` are left
/// unconsumed so that the caller's next match fails on them.
fn parse_digits(bytes: &[u8], pos: &mut usize, min: u32, max: u32) -> Option<u32> {
    let start = *pos;
    let mut value = 0;
    while *pos - start < max as usize {
        let Some(digit) = bytes.get(*pos).filter(|b| b.is_ascii_digit()) else {
            break;
        };
        value = value * 10 + u32::from(*digit - b'0');
        *pos += 1;
    }
    if *pos - start < min as usize {
        return None;
    }
    Some(value)
}

fn parse_byte(bytes: &[u8], pos: &mut usize, byte: u8) -> Option<()> {
    if bytes.get(*pos) == Some(&byte) {
        *pos += 1;
        return Some(());
    }
    None
}

fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while matches!(bytes.get(*pos), Some(b' ' | b'\t')) {
        *pos += 1;
    }
}

/// Generates the trait methods that [`ExtractString`] rejects.
macro_rules! unsupported_serializer_methods {
    ($ret:ty, $($method:ident($($arg:ident: $argty:ty),*)),* $(,)?) => {
        $(
            fn $method(self, $($arg: $argty),*) -> Result<$ret, crate::Error> {
                Err(unsupported_payload())
            }
        )*
    };
}

impl serde::Serialize for Timestamp {
    /// Serializes as a private newtype-struct token whose payload is the
    /// canonical string form (see [Display]). This crate's own serializers
    /// recognize the token: the text serializer emits the payload as a plain
    /// (unquoted) scalar, and the `Value` serializer rebuilds a
    /// `Value::Timestamp`. Other serializers see a newtype struct wrapping a
    /// plain string.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_newtype_struct(TOKEN, &self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for Timestamp {
    /// Accepts the canonical string form, the flat tuple form (3, 7 or 8
    /// elements), a struct with matching fields, and the crate
    /// deserializer's private token protocol.
    fn deserialize<D>(deserializer: D) -> Result<Timestamp, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(TimestampVisitor)
    }
}

/// Deserializes a [`Timestamp`] from the shapes listed on its [`Deserialize`]
/// implementation.
///
/// [`Deserialize`]: serde::Deserialize
pub(crate) struct TimestampVisitor;

impl<'de> Visitor<'de> for TimestampVisitor {
    type Value = Timestamp;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a YAML 1.1 timestamp, as a string, tuple or struct")
    }

    fn visit_str<E>(self, value: &str) -> Result<Timestamp, E>
    where
        E: de::Error,
    {
        Timestamp::parse(value)
            .ok_or_else(|| de::Error::invalid_value(Unexpected::Str(value), &self))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Timestamp, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let year: i32 = next_element(&mut seq, 0, &self)?;
        let month: u8 = next_element(&mut seq, 1, &self)?;
        let day: u8 = next_element(&mut seq, 2, &self)?;

        let mut time = None;
        if let Some(hour) = seq.next_element()? {
            let minute: u8 = next_element(&mut seq, 4, &self)?;
            let second: u8 = next_element(&mut seq, 5, &self)?;
            let nanosecond: u32 = next_element(&mut seq, 6, &self)?;
            time = Some(TimeOfDay::new(hour, minute, second, nanosecond));
        }

        let tz_minutes = if time.is_some() {
            seq.next_element()?
        } else {
            None
        };
        if seq.next_element::<de::IgnoredAny>()?.is_some() {
            return Err(de::Error::custom(
                "expected a timestamp tuple of 3, 7 or 8 elements",
            ));
        }
        Ok(Timestamp::new(year, month, day, time, tz_minutes))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Timestamp, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut year = None;
        let mut month = None;
        let mut day = None;
        let mut time = None;
        let mut tz_minutes = None;

        while let Some(field) = map.next_key::<Field>()? {
            match field {
                Field::Year => year = Some(map.next_value()?),
                Field::Month => month = Some(map.next_value()?),
                Field::Day => day = Some(map.next_value()?),
                Field::Time => time = map.next_value()?,
                Field::TzMinutes => tz_minutes = Some(map.next_value()?),
                _ => {
                    map.next_value::<de::IgnoredAny>()?;
                }
            }
        }

        Ok(Timestamp::new(
            year.ok_or_else(|| de::Error::missing_field("year"))?,
            month.ok_or_else(|| de::Error::missing_field("month"))?,
            day.ok_or_else(|| de::Error::missing_field("day"))?,
            time,
            tz_minutes,
        ))
    }

    fn visit_enum<A>(self, data: A) -> Result<Timestamp, A::Error>
    where
        A: EnumAccess<'de>,
    {
        // The crate deserializer's private token protocol for resolved
        // timestamp scalars: the variant name is the token and the variant is
        // the component struct.
        let (tag, contents) = data.variant_seed(TagStringVisitor)?;
        if tag != TOKEN {
            return Err(de::Error::custom(format_args!(
                "invalid timestamp tag: {tag}"
            )));
        }
        contents.struct_variant(FIELDS, self)
    }
}

/// A field name of the struct-shaped timestamp form.
enum Field {
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Second,
    Nanosecond,
    Time,
    TzMinutes,
    Other,
}

impl<'de> de::Deserialize<'de> for Field {
    fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_identifier(FieldVisitor)
    }
}

struct FieldVisitor;

impl Visitor<'_> for FieldVisitor {
    type Value = Field;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a timestamp field")
    }

    fn visit_str<E>(self, value: &str) -> Result<Field, E>
    where
        E: de::Error,
    {
        Ok(match value {
            "year" => Field::Year,
            "month" => Field::Month,
            "day" => Field::Day,
            "hour" => Field::Hour,
            "minute" => Field::Minute,
            "second" => Field::Second,
            "nanosecond" => Field::Nanosecond,
            "time" => Field::Time,
            "tz_minutes" => Field::TzMinutes,
            _ => Field::Other,
        })
    }
}

/// Presents the components of a resolved [`Timestamp`] as a struct, for the
/// crate deserializer's private token protocol and for deserializing a
/// [`Value::Timestamp`](crate::Value) into other types. The optional `time`
/// and `tz_minutes` fields are omitted when absent.
pub(crate) struct TimestampFields {
    timestamp: Timestamp,
    state: u8,
}

impl TimestampFields {
    pub(crate) fn new(timestamp: Timestamp) -> Self {
        TimestampFields {
            timestamp,
            state: 0,
        }
    }
}

impl<'de> MapAccess<'de> for TimestampFields {
    type Error = crate::Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, crate::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            let index = usize::from(self.state);
            let Some(name) = FIELDS.get(index) else {
                return Ok(None);
            };
            self.state += 1;
            let present = match *name {
                "time" => self.timestamp.time.is_some(),
                "tz_minutes" => self.timestamp.tz_minutes.is_some(),
                _ => true,
            };
            if present {
                return seed
                    .deserialize(StrDeserializer::<crate::Error>::new(name))
                    .map(Some);
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, crate::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let timestamp = self.timestamp;
        match FIELDS[usize::from(self.state) - 1] {
            "year" => seed.deserialize(I32Deserializer::<crate::Error>::new(timestamp.year)),
            "month" => seed.deserialize(U8Deserializer::<crate::Error>::new(timestamp.month)),
            "day" => seed.deserialize(U8Deserializer::<crate::Error>::new(timestamp.day)),
            "time" => {
                let time = timestamp.time.expect("absent time field");
                let components = [
                    u64::from(time.hour),
                    u64::from(time.minute),
                    u64::from(time.second),
                    u64::from(time.nanosecond),
                ];
                seed.deserialize(SomeDeserializer(SeqDeserializer::new(
                    components.into_iter(),
                )))
            }
            _ => {
                let tz_minutes = timestamp.tz_minutes.expect("absent tz_minutes field");
                seed.deserialize(I32Deserializer::<crate::Error>::new(tz_minutes))
            }
        }
    }
}

/// Wraps a deserializer so that its value is presented as `Some`.
struct SomeDeserializer<D>(D);

impl<'de, D> de::Deserializer<'de> for SomeDeserializer<D>
where
    D: de::Deserializer<'de>,
{
    type Error = D::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, D::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self.0)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, D::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct newtype_struct seq tuple tuple_struct
        map struct enum identifier ignored_any
    }
}

impl<'de> serde::Deserialize<'de> for TimeOfDay {
    /// Accepts a 4-element tuple `(hour, minute, second, nanosecond)` or a
    /// struct with matching fields.
    fn deserialize<D>(deserializer: D) -> Result<TimeOfDay, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(TimeOfDayVisitor)
    }
}

struct TimeOfDayVisitor;

impl<'de> Visitor<'de> for TimeOfDayVisitor {
    type Value = TimeOfDay;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .write_str("a time of day, as a tuple or struct of hour, minute, second and nanosecond")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<TimeOfDay, A::Error>
    where
        A: SeqAccess<'de>,
    {
        Ok(TimeOfDay::new(
            next_element(&mut seq, 0, &self)?,
            next_element(&mut seq, 1, &self)?,
            next_element(&mut seq, 2, &self)?,
            next_element(&mut seq, 3, &self)?,
        ))
    }

    fn visit_map<A>(self, mut map: A) -> Result<TimeOfDay, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut hour = None;
        let mut minute = None;
        let mut second = None;
        let mut nanosecond = None;

        while let Some(field) = map.next_key::<Field>()? {
            match field {
                Field::Hour => hour = Some(map.next_value()?),
                Field::Minute => minute = Some(map.next_value()?),
                Field::Second => second = Some(map.next_value()?),
                Field::Nanosecond => nanosecond = Some(map.next_value()?),
                _ => {
                    map.next_value::<de::IgnoredAny>()?;
                }
            }
        }

        Ok(TimeOfDay::new(
            hour.ok_or_else(|| de::Error::missing_field("hour"))?,
            minute.ok_or_else(|| de::Error::missing_field("minute"))?,
            second.ok_or_else(|| de::Error::missing_field("second"))?,
            nanosecond.ok_or_else(|| de::Error::missing_field("nanosecond"))?,
        ))
    }
}

/// Deserializes one tuple element, reporting its index if the tuple is too
/// short.
fn next_element<'de, A, T>(
    seq: &mut A,
    index: usize,
    expected: &dyn de::Expected,
) -> Result<T, A::Error>
where
    A: SeqAccess<'de>,
    T: serde::Deserialize<'de>,
{
    seq.next_element()?
        .ok_or_else(|| <A::Error as de::Error>::invalid_length(index, expected))
}

/// Reads the canonical string payload of a serialized [`Timestamp`] without
/// quoting it. The crate's text serializer uses this to recognize the private
/// token issued by [`Serialize for Timestamp`] and emit a plain scalar.
///
/// [`Serialize for Timestamp`]: serde::Serialize
pub(crate) struct ExtractString;

impl serde::Serializer for ExtractString {
    type Ok = String;
    type Error = crate::Error;

    type SerializeSeq = ser::Impossible<String, crate::Error>;
    type SerializeTuple = ser::Impossible<String, crate::Error>;
    type SerializeTupleStruct = ser::Impossible<String, crate::Error>;
    type SerializeTupleVariant = ser::Impossible<String, crate::Error>;
    type SerializeMap = ser::Impossible<String, crate::Error>;
    type SerializeStruct = ser::Impossible<String, crate::Error>;
    type SerializeStructVariant = ser::Impossible<String, crate::Error>;

    fn serialize_str(self, value: &str) -> Result<String, crate::Error> {
        Ok(value.to_owned())
    }

    fn serialize_some<T>(self, _value: &T) -> Result<String, crate::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Err(unsupported_payload())
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<String, crate::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Err(unsupported_payload())
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<String, crate::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Err(unsupported_payload())
    }

    unsupported_serializer_methods! { String,
        serialize_bool(_value: bool),
        serialize_i8(_value: i8),
        serialize_i16(_value: i16),
        serialize_i32(_value: i32),
        serialize_i64(_value: i64),
        serialize_i128(_value: i128),
        serialize_u8(_value: u8),
        serialize_u16(_value: u16),
        serialize_u32(_value: u32),
        serialize_u64(_value: u64),
        serialize_u128(_value: u128),
        serialize_f32(_value: f32),
        serialize_f64(_value: f64),
        serialize_char(_value: char),
        serialize_bytes(_value: &[u8]),
        serialize_none(),
        serialize_unit(),
        serialize_unit_struct(_name: &'static str),
        serialize_unit_variant(
            _name: &'static str,
            _variant_index: u32,
            _variant: &'static str
        ),
    }

    unsupported_serializer_methods! {
        Self::SerializeSeq,
        serialize_seq(_len: Option<usize>),
    }

    unsupported_serializer_methods! {
        Self::SerializeTuple,
        serialize_tuple(_len: usize),
    }

    unsupported_serializer_methods! {
        Self::SerializeTupleStruct,
        serialize_tuple_struct(_name: &'static str, _len: usize),
    }

    unsupported_serializer_methods! {
        Self::SerializeTupleVariant,
        serialize_tuple_variant(
            _name: &'static str,
            _variant_index: u32,
            _variant: &'static str,
            _len: usize
        ),
    }

    unsupported_serializer_methods! {
        Self::SerializeMap,
        serialize_map(_len: Option<usize>),
    }

    unsupported_serializer_methods! {
        Self::SerializeStruct,
        serialize_struct(_name: &'static str, _len: usize),
    }

    unsupported_serializer_methods! {
        Self::SerializeStructVariant,
        serialize_struct_variant(
            _name: &'static str,
            _variant_index: u32,
            _variant: &'static str,
            _len: usize
        ),
    }
}
fn unsupported_payload() -> crate::Error {
    <crate::Error as ser::Error>::custom("expected a timestamp string")
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for Timestamp {
    fn schema_name() -> String {
        "Timestamp".into()
    }

    /// A timestamp serializes as a plain string scalar, so its schema is a
    /// string schema. Technically, we could further restrict the schema by
    /// specifying `format`, but it's probably not worth the complexity.
    fn json_schema(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(generator)
    }

    fn is_referenceable() -> bool {
        false
    }
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
    fn compares_equal_to_timestamp_strings() {
        let zulu = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        assert_eq!(zulu, "2001-12-15");
        assert_eq!(zulu, "2001-12-15 00:00:00Z");
        assert_eq!(zulu, "2001-12-15T02:00:00+02:00");
        assert_eq!(zulu, String::from("2001-12-15"));
        assert_eq!("2001-12-15", zulu);
        assert_eq!(String::from("2001-12-15"), zulu);
        assert_ne!(zulu, "2001-12-16");
        assert_ne!(zulu, "not a timestamp");
        assert_ne!("not a timestamp", zulu);
    }

    #[test]
    fn with_defaults_fills_missing_components() {
        let date = ts(2001, 12, 15, None, None);
        let explicit = date.with_defaults();
        assert_eq!(explicit.time(), Some(TimeOfDay::new(0, 0, 0, 0)));
        assert_eq!(explicit.tz_minutes(), Some(0));
        assert!(!explicit.is_date_only());
        assert!(!explicit.is_naive());
        assert_eq!(explicit, date);
        assert_eq!(explicit.to_string(), "2001-12-15T00:00:00Z");
    }

    #[test]
    fn with_defaults_preserves_existing_components() {
        let offset = ts(
            2001,
            12,
            15,
            Some(TimeOfDay::new(2, 59, 43, 123_456_789)),
            Some(2 * 60),
        );
        let explicit = offset.with_defaults();
        assert_eq!(explicit.time(), offset.time());
        assert_eq!(explicit.tz_minutes(), Some(2 * 60));
        assert_eq!(explicit.to_string(), "2001-12-15T02:59:43.123456789+02:00");

        // A naive time-of-day gets the default UTC zone, keeping the local
        // time as written.
        let naive = ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), None);
        assert_eq!(naive.with_defaults().to_string(), "2001-12-15T02:59:43Z");
    }

    // Miri's isolation blocks the realtime clock, so this test cannot run
    // there.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn utc_now_is_zulu_and_close_to_system_clock() {
        let before = std::time::SystemTime::now();
        let now = Timestamp::utc_now();
        let after = std::time::SystemTime::now();

        assert_eq!(now.tz_minutes(), Some(0));
        assert!(!now.is_date_only());
        assert!(now.to_string().ends_with('Z'));

        let (minutes, second, nanosecond) = now.instant();
        let micros = (minutes * 60 + i64::from(second)) * 1_000_000 + i64::from(nanosecond / 1_000);
        let to_micros = |time: std::time::SystemTime| {
            time.duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_micros() as i64
        };
        assert!(to_micros(before) <= micros && micros <= to_micros(after));
    }

    #[test]
    fn civil_from_days_inverts_days_from_civil() {
        assert_eq!(civil_from_days(0), (1970, 1, 1));
        assert_eq!(civil_from_days(-1), (1969, 12, 31));
        // Miri interprets far slower than native code, so sweep a smaller
        // range there.
        #[cfg(not(miri))]
        let sweep = -100_000..=100_000;
        #[cfg(miri)]
        let sweep = -1_000..=1_000;
        for days in sweep {
            let (year, month, day) = civil_from_days(days);
            assert_eq!(days_from_civil(year, month, day), days);
        }
    }

    #[test]
    fn comparison_normalizes_offsets() {
        let plus_two = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let zulu = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        let minus_five = ts(
            2001,
            12,
            14,
            Some(TimeOfDay::new(19, 0, 0, 0)),
            Some(-5 * 60),
        );
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
            "2001-12-15T02:59:43"
        );
        assert_eq!(
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
                None
            )
            .to_string(),
            "2001-12-15T02:59:43.100"
        );
        // The fraction width reflects the precision: 3, 6 or 9 digits.
        assert_eq!(
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 1_000_000)),
                None
            )
            .to_string(),
            "2001-12-15T02:59:43.001"
        );
        assert_eq!(
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 123_456_000)),
                None
            )
            .to_string(),
            "2001-12-15T02:59:43.123456"
        );
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 1)), None).to_string(),
            "2001-12-15T02:59:43.000000001"
        );
        assert_eq!(
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), Some(0)).to_string(),
            "2001-12-15T02:59:43Z"
        );
        // The zone is displayed as specified; the instant is not normalized.
        assert_eq!(
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 0)),
                Some(-5 * 60)
            )
            .to_string(),
            "2001-12-15T02:59:43-05:00"
        );
        assert_eq!(
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 30, 0, 0)),
                Some(5 * 60 + 30)
            )
            .to_string(),
            "2001-12-15T02:30:00+05:30"
        );
        // Two syntactically different Timestamps could denote the same semantic
        // instant:
        let a = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let b = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        assert_eq!(a, b);
        assert_ne!(a.to_string(), b.to_string());
    }

    #[test]
    fn parse_accepts_the_spec_grammar() {
        // The examples from https://yaml.org/type/timestamp.html.
        assert_eq!(
            Timestamp::parse("2001-12-15T02:59:43.1Z"),
            Some(ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
                Some(0)
            ))
        );
        assert_eq!(
            Timestamp::parse("2001-12-14t21:59:43.10-05:00"),
            Some(ts(
                2001,
                12,
                14,
                Some(TimeOfDay::new(21, 59, 43, 100_000_000)),
                Some(-300)
            ))
        );
        assert_eq!(
            Timestamp::parse("2001-12-14 21:59:43.10 -5"),
            Some(ts(
                2001,
                12,
                14,
                Some(TimeOfDay::new(21, 59, 43, 100_000_000)),
                Some(-300)
            ))
        );
        assert_eq!(
            Timestamp::parse("2001-12-15 2:59:43.10"),
            Some(ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
                None
            ))
        );
        assert_eq!(
            Timestamp::parse("2002-12-14"),
            Some(ts(2002, 12, 14, None, None))
        );
        // One-digit month and day are allowed in the date-time form.
        assert_eq!(
            Timestamp::parse("2001-2-4 2:59:43"),
            Some(ts(2001, 2, 4, Some(TimeOfDay::new(2, 59, 43, 0)), None))
        );
        // A zone minute is optional; fraction digits beyond the ninth are
        // truncated.
        assert_eq!(
            Timestamp::parse("2001-12-15 02:59:43.1234567894 +05:30"),
            Some(ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 123_456_789)),
                Some(330)
            ))
        );
        // All four spec examples denote the same instant.
        let a = Timestamp::parse("2001-12-15T02:59:43.1Z").unwrap();
        let b = Timestamp::parse("2001-12-14t21:59:43.10-05:00").unwrap();
        let c = Timestamp::parse("2001-12-14 21:59:43.10 -5").unwrap();
        let d = Timestamp::parse("2001-12-15 2:59:43.10").unwrap();
        assert!(a == b && b == c && c == d);
    }

    #[test]
    fn display_round_trips_through_parse() {
        for timestamp in [
            ts(2001, 12, 15, None, None),
            ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0)),
            ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 1)), None),
            ts(
                2001,
                12,
                15,
                Some(TimeOfDay::new(2, 59, 43, 123_456_789)),
                Some(330),
            ),
            ts(
                2001,
                12,
                14,
                Some(TimeOfDay::new(21, 59, 43, 999_999_999)),
                Some(-300),
            ),
        ] {
            let string = timestamp.to_string();
            let parsed = Timestamp::parse(&string).expect(string.as_str());
            assert_eq!(parsed, timestamp);
            // The canonical form should be stable:
            assert_eq!(parsed.to_string(), string);
        }
    }

    #[test]
    fn parse_accepts_out_of_range_components() {
        // Components are grammar-checked only; out-of-range values are
        // stored as given, so they round-trip through the canonical form.
        for (input, timestamp) in [
            ("2001-13-01", ts(2001, 13, 1, None, None)),
            ("2001-00-01", ts(2001, 0, 1, None, None)),
            ("2001-02-29", ts(2001, 2, 29, None, None)),
            ("2000-02-30", ts(2000, 2, 30, None, None)),
            ("2001-04-31", ts(2001, 4, 31, None, None)),
            (
                "2001-12-15T24:00:00",
                ts(2001, 12, 15, Some(TimeOfDay::new(24, 0, 0, 0)), None),
            ),
            (
                "2001-12-15 02:60:61 +25:99",
                ts(2001, 12, 15, Some(TimeOfDay::new(2, 60, 61, 0)), Some(1599)),
            ),
        ] {
            let parsed = Timestamp::parse(input).expect(input);
            assert_eq!(parsed, timestamp, "{input:?}");
            assert_eq!(Timestamp::parse(&parsed.to_string()), Some(parsed));
        }
    }

    #[test]
    fn parse_rejects_non_timestamps() {
        for input in [
            "",
            "2001",
            "2001-12",
            // One-digit month or day in the date-only form.
            "2001-2-15",
            "2001-12-5",
            // Grammar violations.
            "2001-12-15 2:59:43UTC",
            "2001-12-15 2:59:43 z",
            "2001-12-15 2:59",
            "2001-12-15T2:59:43+",
            "2001-12-15junk",
            "2001-12-15 02:59:43 Z ",
        ] {
            assert_eq!(Timestamp::parse(input), None, "{input:?}");
        }
    }
}
