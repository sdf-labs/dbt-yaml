use crate::de::{Event, Progress};
use crate::error::{self, Error, ErrorImpl, Result};
use crate::libyaml::error::Mark;
use crate::libyaml::parser::{Event as YamlEvent, Parser};
use crate::spanned;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::sync::Arc;

pub(crate) struct Loader<'input> {
    parser: Option<Parser<'input>>,
    document_count: usize,
}

pub(crate) struct Document<'input> {
    pub events: Vec<(Event<'input>, Mark)>,
    pub error: Option<Arc<ErrorImpl>>,
    /// Map from alias id to index in events.
    pub aliases: BTreeMap<usize, usize>,
}

impl<'input> Loader<'input> {
    pub fn new(progress: Progress<'input>) -> Result<Self> {
        let input = match progress {
            Progress::Str(s) => Cow::Borrowed(s.as_bytes()),
            Progress::Slice(bytes) => Cow::Borrowed(bytes),
            Progress::Read(mut rdr) => {
                let mut buffer = Vec::new();
                if let Err(io_error) = rdr.read_to_end(&mut buffer) {
                    return Err(error::new(ErrorImpl::Io(io_error)));
                }
                Cow::Owned(buffer)
            }
            Progress::Iterable(_) | Progress::Document(_) => unreachable!(),
            Progress::Fail(err) => return Err(error::shared(err)),
        };

        Ok(Loader {
            parser: Some(Parser::new(input)),
            document_count: 0,
        })
    }

    pub fn next_document(&mut self) -> Option<Document<'input>> {
        let mut document = self.next_document_inner()?;
        drop_self_referential_merge_aliases(&mut document);
        if let Some((_event, mark)) = document.events.first() {
            spanned::set_marker(*mark);
        }

        Some(document)
    }

    fn next_document_inner(&mut self) -> Option<Document<'input>> {
        let parser = match &mut self.parser {
            Some(parser) => parser,
            None => return None,
        };

        let first = self.document_count == 0;
        self.document_count += 1;

        let mut anchors: BTreeMap<_, (usize, Mark)> = BTreeMap::new();
        let mut document = Document {
            events: Vec::new(),
            error: None,
            aliases: BTreeMap::new(),
        };

        loop {
            let (event, mark) = match parser.next() {
                Ok((event, mark)) => (event, mark),
                Err(err) => {
                    document.error = Some(Error::from(err).shared());
                    return Some(document);
                }
            };
            let event = match event {
                YamlEvent::StreamStart => continue,
                YamlEvent::StreamEnd => {
                    self.parser = None;
                    return if first {
                        if document.events.is_empty() {
                            document.events.push((Event::Void, mark));
                        }
                        Some(document)
                    } else {
                        None
                    };
                }
                YamlEvent::DocumentStart => continue,
                YamlEvent::DocumentEnd => {
                    document.events.push((Event::Void, mark));
                    return Some(document);
                }
                YamlEvent::Alias(alias) => match anchors.get(&alias) {
                    Some((id, _)) => Event::Alias(*id),
                    None => {
                        document.error =
                            Some(error::new(ErrorImpl::UnknownAnchor(mark.into())).shared());
                        return Some(document);
                    }
                },
                YamlEvent::Scalar(mut scalar) => {
                    if let Some(anchor) = scalar.anchor.take() {
                        if let Some((_, first_mark)) = anchors.get(&anchor) {
                            document.error = Some(
                                error::new(ErrorImpl::DuplicateAnchor {
                                    first: (*first_mark).into(),
                                    second: mark.into(),
                                })
                                .shared(),
                            );
                            return Some(document);
                        }
                        let id = anchors.len();
                        anchors.insert(anchor, (id, mark));
                        document.aliases.insert(id, document.events.len());
                    }
                    Event::Scalar(scalar)
                }
                YamlEvent::SequenceStart(mut sequence_start) => {
                    if let Some(anchor) = sequence_start.anchor.take() {
                        if let Some((_, first_mark)) = anchors.get(&anchor) {
                            document.error = Some(
                                error::new(ErrorImpl::DuplicateAnchor {
                                    first: (*first_mark).into(),
                                    second: mark.into(),
                                })
                                .shared(),
                            );
                            return Some(document);
                        }
                        let id = anchors.len();
                        anchors.insert(anchor, (id, mark));
                        document.aliases.insert(id, document.events.len());
                    }
                    Event::SequenceStart(sequence_start)
                }
                YamlEvent::SequenceEnd => Event::SequenceEnd,
                YamlEvent::MappingStart(mut mapping_start) => {
                    if let Some(anchor) = mapping_start.anchor.take() {
                        if let Some((_, first_mark)) = anchors.get(&anchor) {
                            document.error = Some(
                                error::new(ErrorImpl::DuplicateAnchor {
                                    first: (*first_mark).into(),
                                    second: mark.into(),
                                })
                                .shared(),
                            );
                            return Some(document);
                        }
                        let id = anchors.len();
                        anchors.insert(anchor, (id, mark));
                        document.aliases.insert(id, document.events.len());
                    }
                    Event::MappingStart(mapping_start)
                }
                YamlEvent::MappingEnd => Event::MappingEnd,
            };
            document.events.push((event, mark));
        }
    }
}

/// Breaks self-referential `<<` merges (e.g. `tables: &t` merged into one of
/// `t`'s own elements) before deserialization even starts, since
/// `apply_merge` only runs after the document is fully materialized and would
/// otherwise recurse forever. Mirrors PyYAML's `flatten_mapping`: drop any
/// `<<`-key alias whose target range contains the mapping doing the merge.
/// Non-merge alias cycles still error, since they have no finite
/// representation.
fn drop_self_referential_merge_aliases(document: &mut Document<'_>) {
    let n = document.events.len();
    if n == 0 {
        return;
    }

    // For every *Start event, the index of its matching *End event.
    let mut end_of = vec![usize::MAX; n];
    {
        let mut stack: Vec<usize> = Vec::new();
        for (i, (event, _)) in document.events.iter().enumerate() {
            match event {
                Event::SequenceStart(_) | Event::MappingStart(_) => stack.push(i),
                Event::SequenceEnd | Event::MappingEnd => {
                    if let Some(start) = stack.pop() {
                        end_of[start] = i;
                    }
                }
                _ => {}
            }
        }
    }

    let skip_node = |events: &[(Event<'_>, Mark)], i: usize| -> usize {
        match &events[i].0 {
            Event::SequenceStart(_) | Event::MappingStart(_) => end_of[i] + 1,
            _ => i + 1,
        }
    };
    let is_within = |target: usize, start: usize, end: usize| target >= start && target <= end;

    let mut to_delete: Vec<usize> = Vec::new();
    {
        let events = &document.events;
        for (m, (event, _)) in events.iter().enumerate() {
            if !matches!(event, Event::MappingStart(_)) {
                continue;
            }
            let end = end_of[m];
            if end == usize::MAX {
                continue;
            }
            let mut i = m + 1;
            while i < end {
                let key_is_merge =
                    matches!(&events[i].0, Event::Scalar(scalar) if scalar.value.as_ref() == b"<<");
                let value_idx = skip_node(events, i);
                if key_is_merge {
                    match &events[value_idx].0 {
                        Event::Alias(id) => {
                            let target = document.aliases.get(id).copied();
                            if target.is_some_and(|target| {
                                is_within(value_idx, target, end_of[target])
                            }) {
                                // The whole merge value is self-referential:
                                // drop both the `<<` key and its value, which
                                // is equivalent to the key never having been
                                // present.
                                to_delete.push(i);
                                to_delete.push(value_idx);
                            }
                        }
                        Event::SequenceStart(_) => {
                            // `<<: [*a, *b, ...]` -- drop only the
                            // self-referential elements, keep the rest of the
                            // merge list intact.
                            let seq_end = end_of[value_idx];
                            let mut j = value_idx + 1;
                            while j < seq_end {
                                if let Event::Alias(id) = &events[j].0 {
                                    let target = document.aliases.get(id).copied();
                                    if target.is_some_and(|target| {
                                        is_within(j, target, end_of[target])
                                    }) {
                                        to_delete.push(j);
                                    }
                                }
                                j = skip_node(events, j);
                            }
                        }
                        _ => {}
                    }
                }
                i = skip_node(events, value_idx);
            }
        }
    }

    if to_delete.is_empty() {
        return;
    }
    to_delete.sort_unstable();
    to_delete.dedup();
    let to_delete = to_delete;

    let mut remap = vec![0usize; n];
    let mut new_events = Vec::with_capacity(n - to_delete.len());
    let mut delete_iter = to_delete.iter().copied().peekable();
    for (i, entry) in document.events.drain(..).enumerate() {
        if delete_iter.peek() == Some(&i) {
            delete_iter.next();
            continue;
        }
        remap[i] = new_events.len();
        new_events.push(entry);
    }
    document.events = new_events;
    for target in document.aliases.values_mut() {
        *target = remap[*target];
    }
}
