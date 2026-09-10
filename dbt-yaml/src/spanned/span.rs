use std::fmt::{self, Debug, Display};
use std::ops::Range;
#[cfg(feature = "filename")]
use std::path::PathBuf;
#[cfg(feature = "filename")]
use std::sync::Arc;

use crate::libyaml::error::Mark;

/// A source span.
#[derive(Clone)]
pub struct Span {
    /// The start of the span.
    pub start: Marker,

    /// The end of the span.
    pub end: Marker,

    #[cfg(feature = "filename")]
    /// An optional filename.
    pub filename: Option<Arc<PathBuf>>,

    #[cfg(feature = "yaml_11")]
    /// True if the scalar this span refers to was written in YAML's plain
    /// (unquoted) style, as opposed to being explicitly quoted.
    ///
    /// Always `false` for spans that don't correspond to a single scalar
    /// (sequences, mappings, tagged values) and for values constructed
    /// without going through the deserializer (e.g. `Value::string`).
    ///
    /// This distinguishes a bare `2026-08-26` from `"2026-08-26"`: only the
    /// former could be construed as a YAML 1.1 timestamp rather than a
    /// string. dbt-yaml itself always resolves both to `Value::String` (see
    /// [crate::Value]); this flag is the only place that distinction
    /// survives, for callers that need to make their own type inference
    /// decision based on it.
    pub was_plain: bool,
}

impl Span {
    /// Create a new span.
    pub fn new(start: Marker, end: Marker) -> Self {
        Span {
            start,
            end,
            #[cfg(feature = "filename")]
            filename: None,
            #[cfg(feature = "yaml_11")]
            was_plain: false,
        }
    }

    /// True if this span is valid.
    pub fn is_valid(&self) -> bool {
        self.start.index <= self.end.index
            && self.start.line > 0
            && self.start.column > 0
            && self.end.line > 0
            && self.end.column > 0
    }

    /// Construct an empty (invalid) span.
    pub const fn zero() -> Self {
        Span {
            start: Marker::zero(),
            end: Marker::zero(),
            #[cfg(feature = "filename")]
            filename: None,
            #[cfg(feature = "yaml_11")]
            was_plain: false,
        }
    }
}

#[cfg(feature = "yaml_11")]
impl Span {
    /// Get whether the scalar this span refers to was written in YAML's
    /// plain (unquoted) style. See the field docs on [`Span::was_plain`].
    pub fn was_plain(&self) -> bool {
        self.was_plain
    }

    /// Replace the `was_plain` flag on this span.
    pub(crate) fn with_was_plain(self, was_plain: bool) -> Self {
        Span { was_plain, ..self }
    }
}

// `was_plain` is metadata about how a scalar was spelled, not part of a
// span's identity, so it's deliberately left out of comparisons below:
// two spans covering the same source range are equal (and order the same)
// regardless of it. These impls are otherwise equivalent to what
// `#[derive(PartialEq, Eq, PartialOrd, Ord)]` would produce over `start`,
// `end`, and (with the `filename` feature) `filename`.
impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        let eq = self.start == other.start && self.end == other.end;
        #[cfg(feature = "filename")]
        let eq = eq && self.filename == other.filename;
        eq
    }
}

impl Eq for Span {}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let ord = self
            .start
            .cmp(&other.start)
            .then_with(|| self.end.cmp(&other.end));
        #[cfg(feature = "filename")]
        let ord = ord.then_with(|| self.filename.cmp(&other.filename));
        ord
    }
}

#[cfg(feature = "filename")]
impl Span {
    /// Create a new span with the specified filename.
    pub fn new_with_filename(
        start: impl Into<Marker>,
        end: impl Into<Marker>,
        filename: impl Into<Arc<PathBuf>>,
    ) -> Self {
        Span {
            start: start.into(),
            end: end.into(),
            filename: Some(filename.into()),
            #[cfg(feature = "yaml_11")]
            was_plain: false,
        }
    }

    /// Replace the filename in this span with the given filename.
    pub fn with_filename(self, filename: impl Into<Arc<PathBuf>>) -> Self {
        Span {
            filename: Some(filename.into()),
            ..self
        }
    }

    /// Get the filename in this span.
    pub fn get_filename(&self) -> Option<&std::path::Path> {
        self.filename.as_deref().map(|f| f.as_ref())
    }

    pub(crate) fn maybe_capture_filename(self) -> Self {
        if let Some(filename) = crate::spanned::get_filename() {
            Self {
                filename: Some(filename),
                ..self
            }
        } else {
            self
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::zero()
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<(Marker, Marker)> for Span {
    fn from((start, end): (Marker, Marker)) -> Self {
        Span::new(start, end)
    }
}

impl From<Range<Option<Marker>>> for Span {
    fn from(range: Range<Option<Marker>>) -> Self {
        let start = range.start.unwrap_or_default();
        let end = range.end.unwrap_or_default();
        Span::new(start, end)
    }
}

impl From<Marker> for Span {
    fn from(marker: Marker) -> Self {
        Span::new(marker, marker)
    }
}

/// A location in the source string.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Marker {
    /// Offset in bytes from the start of the source string.
    pub index: usize,

    /// Line number in the source string.
    pub line: usize,

    /// Column number in the source string.
    pub column: usize,
}

impl Marker {
    /// Create a new location.
    pub fn new(index: usize, line: usize, column: usize) -> Self {
        Marker {
            index,
            line,
            column,
        }
    }

    /// Create a location pointing to the start of the source string.
    pub const fn start() -> Self {
        Marker {
            index: 0,
            line: 1,
            column: 1,
        }
    }

    /// Create an empty location.
    pub const fn zero() -> Self {
        Marker {
            index: 0,
            line: 0,
            column: 0,
        }
    }

    /// Return the line number of this location.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Return the column number of this location.
    pub fn column(&self) -> usize {
        self.column
    }

    /// Return the index of this location.
    pub fn index(&self) -> usize {
        self.index
    }
}

impl Default for Marker {
    fn default() -> Self {
        Marker::zero()
    }
}

impl From<Mark> for Marker {
    fn from(mark: Mark) -> Self {
        Marker {
            index: mark.index() as usize,
            // `line` and `column` returned from libyaml are 0-indexed
            line: mark.line() as usize + 1,
            column: mark.column() as usize + 1,
        }
    }
}

impl Debug for Marker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}[{}]", self.line, self.column, self.index)
    }
}

impl Display for Marker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {} column {}", self.line, self.column)
    }
}
