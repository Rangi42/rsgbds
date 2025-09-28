use std::{cmp::Ordering, fs::File, io::Read, iter::FusedIterator, ops::Range, rc::Rc};

use compact_str::{CompactString, ToCompactString};

use crate::Identifier;

pub struct Source {
    pub name: CompactString,
    // TODO: consider only computing the `Source` when printing diagnostics? Would also avoid the `'id: 'ret` issue
    pub contents: ariadne::Source<CompactString>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Span {
    CommandLine,
    Builtin,
    TopLevel,
    Normal(NormalSpan),
}

#[derive(Debug, Clone)]
pub struct NormalSpan {
    pub node: FileNode,
    pub bytes: Range<usize>,
}
/// A [`NormalSpan`], but without the byte range.
///
/// It is the information that gets serialised into the object file.
#[derive(Debug, Clone)]
pub struct FileNode {
    // TODO(perf): consider the `rclite` crate, since we don't need weak refs
    pub src: Rc<Source>,
    pub kind: SpanKind,
    /// For example, for a [`File`][SpanKind::File], this is the span of its triggering `INCLUDE` directive.
    pub parent: Option<Rc<NormalSpan>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
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
    pub fn load_file(
        mut file: File,
        name: CompactString,
    ) -> Result<Rc<Self>, (std::io::Error, CompactString)> {
        let mut string = String::new();
        if let Err(err) = file.read_to_string(&mut string) {
            return Err((err, name));
        }
        let contents = string.to_compact_string().into();
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
        let bytes = 0..src.contents.text().len();
        Self {
            node: FileNode { src, kind, parent },
            bytes,
        }
    }

    pub fn sub_span(&self, bytes: Range<usize>) -> Self {
        debug_assert!(self.is_offset_valid(bytes.start));
        debug_assert!(self.is_offset_valid(bytes.end));
        Self {
            node: self.node.clone(),
            bytes,
        }
    }

    pub fn is_offset_valid(&self, byte_ofs: usize) -> bool {
        // Not using `.contains()` because we accept offsets matching the end.
        self.bytes.start <= byte_ofs && byte_ofs <= self.bytes.end
    }

    /// Moves the span's beginning to its end, so that it is empty.
    /// In essence, this discards all characters scanned thus far.
    pub fn make_empty(&mut self) {
        self.bytes.start = self.bytes.end;
    }
}

// Required by `ariadne` to determine whether two spans point to the same file.
// Instead of comparing sources by contents, we check if both spans point to the same source.

impl PartialEq for NormalSpan {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.node.src, &other.node.src)
    }
}

// Required by the object file emitter.

impl PartialEq for FileNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.src, &other.src)
            && self.kind == other.kind
            && match (self.parent.as_ref(), other.parent.as_ref()) {
                (None, None) => true,
                (Some(this_parent), Some(other_parent)) => Rc::ptr_eq(this_parent, other_parent),
                _ => false,
            }
    }
}
impl Eq for FileNode {}
impl std::hash::Hash for FileNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(Rc::as_ptr(&self.src) as usize);
        self.kind.hash(state);
        if let Some(parent) = self.parent.as_ref() {
            state.write_usize(Rc::as_ptr(parent) as usize);
        }
    }
}

impl Span {
    pub fn extract_normal(self) -> NormalSpan {
        let Self::Normal(span) = self else {
            unreachable!("Expected a normal span, got {self:?}")
        };
        span
    }

    pub fn merged_with(&self, other: &Self) -> Self {
        let (Self::Normal(lhs), Self::Normal(rhs)) = (self, &other) else {
            panic!("Only normal spans can be merged, not {self:?} and {other:?}");
        };
        Self::Normal(lhs.merged_with(rhs))
    }
}
impl NormalSpan {
    pub fn merged_with(&self, other: &Self) -> Self {
        fn parent_of(span: &NormalSpan) -> &NormalSpan {
            debug_assert!(
                span.node.kind.ends_implicitly(),
                "Spans can only be merged if they are inside of the same “strong” context"
            );
            span.node.parent.as_ref().unwrap()
        }
        struct ParentIter<'span>(Option<&'span NormalSpan>);
        impl<'span> Iterator for ParentIter<'span> {
            type Item = &'span NormalSpan;
            fn next(&mut self) -> Option<Self::Item> {
                let span = self.0?;
                self.0 = span.node.parent.as_deref();
                self.0
            }
        }
        impl FusedIterator for ParentIter<'_> {}

        // TODO(perf): store the depth directly in the span?
        let left_depth = ParentIter(Some(self)).count();
        let right_depth = ParentIter(Some(other)).count();
        // Start looking for the topmost common level.
        // In principle, the spans could be pointing at the same source but different locations within it;
        // in practice, callers should just not attempt to merge such tokens.
        //
        // This logic is meant for e.g. one token at top level, and the other inside of an expansion.
        let mut left = self;
        let mut right = other;
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
        while !Rc::ptr_eq(&left.node.src, &right.node.src) {
            left = parent_of(left);
            right = parent_of(right);
        }

        debug_assert_eq!(left.node.kind, right.node.kind);
        debug_assert!(
            left.bytes.end <= right.bytes.start,
            "Attempting to merge {:?} and {:?} in {}",
            left.bytes,
            right.bytes,
            left.node.src.name,
        );
        NormalSpan {
            node: left.node.clone(),
            bytes: left.bytes.start..right.bytes.end,
        }
    }
}

/// [`ariadne::Source`] has huge `Debug` output that we don't care about, so reduce it to its text contents.
impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source")
            .field("name", &self.name)
            .field("contents", &self.contents.text())
            .finish()
    }
}
