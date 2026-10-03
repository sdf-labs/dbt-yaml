//! A structural representation of a YAML document in which every scalar is
//! kept as a [`String`], without applying YAML's implicit type resolution.
//!
//! Unlike [`Value`](crate::Value), which resolves plain scalars to null, bool,
//! or number according to the YAML core schema, [`StringNode`] preserves the
//! scalar text exactly as parsed: `0x10` stays `"0x10"`, `~` stays `"~"`, and
//! so on. Node structure (sequence vs mapping vs scalar) is taken from the
//! parser's event stream, so no schema is required. Tags and scalar styles are
//! discarded: a scalar node's value is the scalar's content after the parser
//! has processed quoting and escape sequences.
//!
//! Like [`Value`](crate::Value), every node also carries the [`Span`] of the
//! source region it was parsed from; see [`StringNode::span`]. Spans are
//! metadata only: they do not take part in equality or hashing.

use crate::de::{Event, Progress};
use crate::error::{self, ErrorImpl, Result};
use crate::libyaml::error::Mark;
use crate::loader::{Document, Loader};
use crate::path::Path;
use crate::{spanned, Marker, Span};
use std::hash::{Hash, Hasher};
use std::io;
use std::mem;
use std::sync::Arc;

/// A YAML node in which every scalar is kept as a string.
///
/// See the [module-level documentation](self) for details.
#[derive(Debug, Clone)]
pub enum StringNode {
    /// A scalar node. The string is the scalar's content as parsed, with no
    /// implicit type resolution applied. An empty document is represented as
    /// `Scalar("")`. The span covers the scalar in the source.
    Scalar(String, Span),
    /// A sequence node, in document order. The span covers the sequence from
    /// its first token to the start of the next node in the source.
    Sequence(Vec<StringNode>, Span),
    /// A mapping node, as key-value pairs in document order. Keys are full
    /// nodes because YAML permits non-scalar mapping keys. The span covers
    /// the mapping from its first token to the start of the next node in the
    /// source.
    Mapping(Vec<(StringNode, StringNode)>, Span),
}

impl StringNode {
    /// Parses a YAML document from a string into a string tree.
    ///
    /// Fails with [`ErrorImpl::MoreThanOneDocument`](crate::Error) if the
    /// input contains more than one document.
    pub fn from_str(s: &str) -> Result<Self> {
        parse(Progress::Str(s))
    }

    /// Parses a YAML document from a byte slice into a string tree.
    pub fn from_slice(v: &[u8]) -> Result<Self> {
        parse(Progress::Slice(v))
    }

    /// Parses a YAML document from an IO stream into a string tree.
    pub fn from_reader<R>(rdr: R) -> Result<Self>
    where
        R: io::Read,
    {
        parse(Progress::Read(Box::new(rdr)))
    }

    /// Returns the recorded source [`Span`] of this node.
    ///
    /// The span runs from the node's first token to the start of the next
    /// node in the source. A node produced from an alias carries the span of
    /// the alias reference, not of the anchored definition.
    pub fn span(&self) -> &Span {
        match self {
            StringNode::Scalar(_, span)
            | StringNode::Sequence(_, span)
            | StringNode::Mapping(_, span) => span,
        }
    }

    fn set_span(&mut self, span: Span) {
        match self {
            StringNode::Scalar(_, s)
            | StringNode::Sequence(_, s)
            | StringNode::Mapping(_, s) => *s = span,
        }
    }
}

impl PartialEq for StringNode {
    /// Two nodes are equal if their structure and scalar contents are equal.
    /// Spans do not take part in equality.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StringNode::Scalar(a, _), StringNode::Scalar(b, _)) => a == b,
            (StringNode::Sequence(a, _), StringNode::Sequence(b, _)) => a == b,
            (StringNode::Mapping(a, _), StringNode::Mapping(b, _)) => a == b,
            _ => false,
        }
    }
}

impl Eq for StringNode {}

impl Hash for StringNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            StringNode::Scalar(v, _) => v.hash(state),
            StringNode::Sequence(v, _) => v.hash(state),
            StringNode::Mapping(v, _) => v.hash(state),
        }
    }
}

fn parse(progress: Progress) -> Result<StringNode> {
    spanned::set_marker(spanned::Marker::start());
    let result = parse_inner(progress);
    spanned::reset_marker();
    result
}

fn parse_inner(progress: Progress) -> Result<StringNode> {
    let mut loader = Loader::new(progress)?;
    let document = match loader.next_document() {
        Some(document) => document,
        None => return Err(error::new(ErrorImpl::EndOfStream)),
    };
    if let Some(parse_error) = &document.error {
        return Err(error::shared(Arc::clone(parse_error)));
    }
    let mut builder = Builder {
        document: &document,
        pos: 0,
        jumpcount: 0,
        remaining_depth: 128,
    };
    let node = builder.build()?;
    if loader.next_document().is_some() {
        return Err(error::new(ErrorImpl::MoreThanOneDocument));
    }
    Ok(node)
}

struct Builder<'document, 'de> {
    document: &'document Document<'de>,
    pos: usize,
    jumpcount: usize,
    remaining_depth: u8,
}

impl<'document, 'de> Builder<'document, 'de> {
    fn peek_event_mark(&self) -> Result<(&'document Event<'de>, Mark)> {
        match self.document.events.get(self.pos) {
            Some((event, mark)) => Ok((event, *mark)),
            None => Err(match &self.document.error {
                Some(parse_error) => error::shared(Arc::clone(parse_error)),
                None => error::new(ErrorImpl::EndOfStream),
            }),
        }
    }

    fn next_event_mark(&mut self) -> Result<(&'document Event<'de>, Mark)> {
        let (event, mark) = self.peek_event_mark()?;
        self.pos += 1;
        Ok((event, mark))
    }

    fn build_nested(&mut self, mark: Mark) -> Result<StringNode> {
        let previous_depth = self.remaining_depth;
        self.remaining_depth = match previous_depth.checked_sub(1) {
            Some(depth) => depth,
            None => return Err(error::new(ErrorImpl::RecursionLimitExceeded(mark.into()))),
        };
        let result = self.build();
        self.remaining_depth = previous_depth;
        result
    }

    fn build(&mut self) -> Result<StringNode> {
        let (event, mark) = self.next_event_mark()?;
        let mut node = match event {
            Event::Void => StringNode::Scalar(String::new(), Span::zero()),
            Event::Scalar(scalar) => match String::from_utf8(scalar.value.to_vec()) {
                Ok(v) => StringNode::Scalar(v, Span::zero()),
                Err(err) => {
                    return Err(error::fix_mark(
                        error::new(ErrorImpl::FromUtf8(err)),
                        mark,
                        Path::Root,
                    ));
                }
            },
            Event::Alias(id) => {
                self.jumpcount += 1;
                if self.jumpcount > self.document.events.len() * 100 {
                    return Err(error::new(ErrorImpl::RepetitionLimitExceeded));
                }
                match self.document.aliases.get(id) {
                    Some(found) => {
                        let saved = self.pos;
                        self.pos = *found;
                        let result = self.build();
                        self.pos = saved;
                        result?
                    }
                    None => panic!("unresolved alias: {}", *id),
                }
            }
            Event::SequenceStart(_) => {
                let mut items = Vec::new();
                loop {
                    match self.peek_event_mark()?.0 {
                        Event::SequenceEnd | Event::Void => {
                            self.pos += 1;
                            break;
                        }
                        _ => items.push(self.build_nested(mark)?),
                    }
                }
                StringNode::Sequence(items, Span::zero())
            }
            Event::MappingStart(_) => {
                let mut pairs = Vec::new();
                loop {
                    match self.peek_event_mark()?.0 {
                        Event::MappingEnd | Event::Void => {
                            self.pos += 1;
                            break;
                        }
                        _ => {
                            let key = self.build_nested(mark)?;
                            let value = self.build_nested(mark)?;
                            pairs.push((key, value));
                        }
                    }
                }
                StringNode::Mapping(pairs, Span::zero())
            }
            Event::SequenceEnd => panic!("unexpected end of sequence"),
            Event::MappingEnd => panic!("unexpected end of mapping"),
        };

        let start = Marker::from(mark);
        // The end of a node is the start of the next unconsumed event. At the
        // end of the event stream there is no next event, so the span closes
        // at its own start.
        let end = match self.document.events.get(self.pos) {
            Some((_, mark)) => Marker::from(*mark),
            None => start,
        };
        let span = Span::new(start, end);
        #[cfg(feature = "filename")]
        let span = span.maybe_capture_filename();
        node.set_span(span);
        Ok(node)
    }
}
