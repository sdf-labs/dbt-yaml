//! A structural representation of a YAML document in which every scalar is
//! kept as a [`String`], without applying YAML's implicit type resolution.
//!
//! Unlike [`Value`](crate::Value), which resolves plain scalars to null,
//! bool, or number according to the YAML core schema, [`StringNode`]
//! preserves the scalar text exactly as parsed: `0x10` stays `"0x10"`, `~`
//! stays `"~"`, and so on. Node structure (sequence vs mapping vs scalar) is
//! taken from the parser's event stream, so no schema is required.
//!
//! Tags and scalar styles are discarded: a scalar node's value is the
//! scalar's content after the parser has processed quoting and escape
//! sequences.

use crate::de::{Event, Progress};
use crate::error::{self, ErrorImpl, Result};
use crate::libyaml::error::Mark;
use crate::loader::{Document, Loader};
use crate::path::Path;
use crate::spanned;
use std::io;
use std::sync::Arc;

/// A YAML node in which every scalar is kept as a string.
///
/// See the [module-level documentation](self) for details.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringNode {
    /// A scalar node. The string is the scalar's content as parsed, with no
    /// implicit type resolution applied. An empty document is represented as
    /// `Scalar("")`.
    Scalar(String),
    /// A sequence node, in document order.
    Sequence(Vec<StringNode>),
    /// A mapping node, as key-value pairs in document order. Keys are full
    /// nodes because YAML permits non-scalar mapping keys.
    Mapping(Vec<(StringNode, StringNode)>),
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
        match event {
            Event::Void => Ok(StringNode::Scalar(String::new())),
            Event::Scalar(scalar) => match String::from_utf8(scalar.value.to_vec()) {
                Ok(v) => Ok(StringNode::Scalar(v)),
                Err(err) => Err(error::fix_mark(
                    error::new(ErrorImpl::FromUtf8(err)),
                    mark,
                    Path::Root,
                )),
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
                        result
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
                Ok(StringNode::Sequence(items))
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
                Ok(StringNode::Mapping(pairs))
            }
            Event::SequenceEnd => panic!("unexpected end of sequence"),
            Event::MappingEnd => panic!("unexpected end of mapping"),
        }
    }
}
