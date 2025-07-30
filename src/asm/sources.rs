use std::{cmp::Ordering, iter::FusedIterator, ops::Range, path::Path, rc::Rc};

use compact_str::{CompactString, ToCompactString};

use crate::Identifier;

#[derive(Debug)]
pub struct Source {
    pub name: CompactString,
    // TODO: consider only computing the `Source` when printing diagnostics? Would also avoid the `'id: 'ret` issue
    pub contents: ariadne::Source<CompactString>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Span {
    CommandLine,
    Builtin,
    Normal(NormalSpan),
}

#[derive(Debug, Clone)]
pub struct NormalSpan {
    // TODO(perf): consider the `rclite` crate, since we don't need weak refs
    pub src: Rc<Source>,
    pub bytes: Range<usize>,
    pub kind: SpanKind,
    /// For example, for a [`File`][SpanKind::File], this is the span of its triggering `INCLUDE` directive.
    pub parent: Option<Rc<NormalSpan>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpanKind {
    File, // has the name of the underlying `src`.
    Macro(Identifier),
    Loop(u32),
    Expansion(Identifier),
    MacroArg(usize),
    CombinedMacroArgs,
    UniqueId,
    Invalid,
}

impl Source {
    pub fn load_file<P: AsRef<Path>>(path: P) -> std::io::Result<Rc<Self>> {
        let contents = std::fs::read_to_string(&path)?.to_compact_string().into();
        let name = path.as_ref().display().to_compact_string();
        Ok(Rc::new(Self { name, contents }))
    }
}

impl SpanKind {
    pub fn ends_implicitly(&self) -> bool {
        // Not using `matches` so that the match is guaranteed to be exhaustive.
        match self {
            Self::File | Self::Macro(..) | Self::Loop(..) => false,
            Self::Expansion(..)
            | Self::MacroArg(..)
            | Self::CombinedMacroArgs
            | Self::UniqueId
            | Self::Invalid => true,
        }
    }
}

impl NormalSpan {
    pub fn new(src: Rc<Source>, kind: SpanKind, parent: Option<Rc<NormalSpan>>) -> Self {
        let bytes = 0..src.contents.len();
        Self {
            src,
            bytes,
            kind,
            parent,
        }
    }

    pub fn sub_span(&self, sub_range: Range<usize>) -> Self {
        debug_assert!(self.is_offset_valid(sub_range.start));
        debug_assert!(self.is_offset_valid(sub_range.end));
        Self {
            src: Rc::clone(&self.src),
            bytes: sub_range,
            kind: self.kind,
            parent: self.parent.clone(),
        }
    }

    pub fn is_offset_valid(&self, byte_ofs: usize) -> bool {
        // Not using `.contains()` because we accept offsets matching the end.
        self.bytes.start <= byte_ofs && byte_ofs <= self.bytes.end
    }
}

// Required by `ariadne`.
// Instead of comparing sources by contents, we check if both spans point to the same source.
impl PartialEq for NormalSpan {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            src,
            bytes,
            kind,
            parent,
        } = self; // Using this to make sure not to forget any fields if they're added.
        Rc::ptr_eq(src, &other.src)
            && (bytes, kind, parent) == (&other.bytes, &other.kind, &other.parent)
    }
}

impl Span {
    pub fn merged_with(&self, other: &Self) -> Self {
        let (Self::Normal(lhs), Self::Normal(rhs)) = (self, &other) else {
            panic!("Only normal spans can be merged, not {self:?} and {other:?}");
        };

        fn parent_of(span: &NormalSpan) -> &NormalSpan {
            debug_assert!(
                span.kind.ends_implicitly(),
                "Spans can only be merged if they are inside of the same “strong” context"
            );
            span.parent.as_ref().unwrap()
        }
        struct ParentIter<'span>(Option<&'span NormalSpan>);
        impl<'span> Iterator for ParentIter<'span> {
            type Item = &'span NormalSpan;
            fn next(&mut self) -> Option<Self::Item> {
                let span = self.0?;
                self.0 = span.parent.as_deref();
                self.0
            }
        }
        impl FusedIterator for ParentIter<'_> {}

        // TODO(perf): store the depth directly in the span?
        let left_depth = ParentIter(Some(lhs)).count();
        let right_depth = ParentIter(Some(rhs)).count();
        // Start looking for the topmost common level.
        // In principle, the spans could be pointing at the same source but different locations within it;
        // in practice, callers should just not attempt to merge such tokens.
        //
        // This logic is meant for e.g. one token at top level, and the other inside of an expansion.
        let mut left = lhs;
        let mut right = rhs;
        match left_depth.cmp(&right_depth) {
            Ordering::Equal => {}
            Ordering::Less => {
                for _ in 0..right_depth - left_depth {
                    right = parent_of(right);
                }
            }
            Ordering::Greater => {
                for _ in 0..left_depth - right_depth {
                    left = parent_of(left);
                }
            }
        }
        while !Rc::ptr_eq(&left.src, &right.src) {
            left = parent_of(left);
            right = parent_of(right);
        }

        debug_assert_eq!(left.kind, right.kind);
        debug_assert!(left.bytes.end <= right.bytes.start);
        Self::Normal(NormalSpan {
            src: Rc::clone(&left.src),
            bytes: left.bytes.start..right.bytes.end,
            kind: left.kind,
            parent: left.parent.clone(),
        })
    }
}
