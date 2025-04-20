use compact_str::CompactString;
use indexmap::{map::Entry, IndexMap};
use rustc_hash::FxBuildHasher;

use crate::{common::section::MemRegion, context_stack::Span, expr::Expr};

#[derive(Debug)]
pub struct Sections<'ctx_stack> {
    sections: IndexMap<CompactString, Section<'ctx_stack>, FxBuildHasher>,
    /// The first is the “data” section, the second is the “symbol” section.
    pub active_section: Option<(ActiveSection, ActiveSection)>,
    section_stack: Vec<Option<(ActiveSection, ActiveSection)>>,
}

#[derive(Debug)]
pub struct Section<'ctx_stack> {
    attrs: SectionAttrs,
    patches: Vec<Patch<'ctx_stack>>,
    // TODO: the rest of the fucking owl
}
#[derive(Debug)]
pub struct SectionAttrs {
    pub kind: SectionKind,
    pub mem_region: MemRegion,
    pub address: AddrConstraint,
    pub bank: Option<u32>,
}
#[derive(Debug)]
pub enum SectionKind {
    Normal,
    Union,
    Fragment,
}
#[derive(Debug, PartialEq, Eq)]
pub enum AddrConstraint {
    None,
    Align(u8, u16),
    Addr(u16),
}

#[derive(Debug, Clone)]
pub struct ActiveSection {
    id: usize,
    offset: usize,
}

#[derive(Debug)]
struct Patch<'ctx_stack> {
    span: Span<'ctx_stack>,
    expr: Expr<'ctx_stack>,
    kind: PatchKind,
}
#[derive(Debug)]
enum PatchKind {
    Byte,
    Word,
    Long,
}

impl<'ctx_stack> Sections<'ctx_stack> {
    pub fn new() -> Self {
        Self {
            sections: IndexMap::with_hasher(FxBuildHasher::default()),
            active_section: None,
            section_stack: vec![],
        }
    }

    pub fn create_if_not_exists(
        &mut self,
        name: CompactString,
        attrs: SectionAttrs,
    ) -> ActiveSection {
        match self.sections.entry(name) {
            Entry::Vacant(entry) => {
                let id = entry
                    .insert_entry(Section {
                        attrs,
                        patches: vec![],
                    })
                    .index();
                ActiveSection { id, offset: 0 }
            }
            Entry::Occupied(entry) => todo!(),
        }
    }

    pub fn push_active_section(&mut self) {
        self.section_stack.push(self.active_section.take());
    }
    pub fn pop_active_section(&mut self) -> Option<()> {
        self.active_section = self.section_stack.pop()?;
        Some(())
    }
}

impl AddrConstraint {
    pub fn merge(&mut self, other: Self, offset: usize) -> Result<(), MergeError> {
        match (self as &mut _, other) {
            // If the new constraint is nonexistent, then there is nothing to do.
            (_, AddrConstraint::None) => Ok(()),
            // In this case, the new constraint simply replaces the old one, with no chance of conflict either.
            (AddrConstraint::None, other) => {
                *self = other;
                Ok(())
            }
            (AddrConstraint::Align(_, _), AddrConstraint::Align(_, _)) => todo!(),
            (AddrConstraint::Align(_, _), AddrConstraint::Addr(_)) => todo!(),
            (AddrConstraint::Addr(_), AddrConstraint::Align(_, _)) => todo!(),
            (AddrConstraint::Addr(_), AddrConstraint::Addr(_)) => todo!(),
        }
    }
}
pub enum MergeError {}

impl ActiveSection {
    pub fn points_to_same_as(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
