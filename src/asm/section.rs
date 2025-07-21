use compact_str::CompactString;
use indexmap::{map::Entry, IndexMap};
use rustc_hash::FxBuildHasher;

use crate::{
    common::{section::MemRegion, S},
    expr::Expr,
    sources::Span,
};

#[derive(Debug)]
pub struct Sections {
    sections: IndexMap<CompactString, Section, FxBuildHasher>,
    /// The first is the “data” section, the second is the “symbol” section.
    pub active_section: Option<(ActiveSection, ActiveSection)>,
    section_stack: Vec<Option<(ActiveSection, ActiveSection)>>,
}

#[derive(Debug)]
pub struct Section {
    attrs: SectionAttrs,
    patches: Vec<Patch>,
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
struct Patch {
    span: Span,
    expr: Expr,
    kind: PatchKind,
}
#[derive(Debug)]
enum PatchKind {
    Byte,
    Word,
    Long,
}

impl Sections {
    pub fn new() -> Self {
        Self {
            sections: IndexMap::with_hasher(FxBuildHasher),
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
            (
                AddrConstraint::Align(align, align_ofs),
                AddrConstraint::Align(other_align, other_align_ofs),
            ) => {
                if other_align_ofs % (1 << *align) == *align_ofs % (1 << other_align) {
                    if *align < other_align {
                        *align = other_align;
                        *align_ofs = other_align_ofs;
                    }
                    Ok(())
                } else {
                    Err(MergeError::ConflictingAlign(
                        *align,
                        *align_ofs,
                        other_align,
                        other_align_ofs,
                    ))
                }
            }
            (AddrConstraint::Align(align, align_ofs), AddrConstraint::Addr(addr)) => {
                if addr % (1 << *align) == *align_ofs {
                    *self = AddrConstraint::Addr(addr);
                    Ok(())
                } else {
                    Err(MergeError::BadAlignFixed(*align, *align_ofs, addr))
                }
            }
            (AddrConstraint::Addr(addr), AddrConstraint::Align(align, align_ofs)) => {
                if *addr % (1 << align) == align_ofs {
                    Ok(())
                } else {
                    Err(MergeError::FixedBadAlign(*addr, align, align_ofs))
                }
            }
            (AddrConstraint::Addr(addr), AddrConstraint::Addr(other_addr)) => {
                if *addr == other_addr {
                    Ok(())
                } else {
                    Err(MergeError::AddrMismatch(*addr, other_addr))
                }
            }
        }
    }
}
#[derive(Debug)]
pub enum MergeError {
    AddrMismatch(u16, u16),
    FixedBadAlign(u16, u8, u16),
    BadAlignFixed(u8, u16, u16),
    ConflictingAlign(u8, u16, u8, u16),
}
impl MergeError {
    pub fn details(&self) -> (String, String) {
        match self {
            Self::AddrMismatch(addr, other_addr) => (
                format!("Cannot place the section at address ${other_addr:04x}"),
                format!("The section is already fixed at address ${addr:04x}"),
            ),
            Self::FixedBadAlign(addr, align, align_ofs) => (
                format!(
                    "Cannot align the section's lower {align} bit{} to {align_ofs}",
                    S::from(*align),
                ),
                format!(
                    "The fixed address would have the bit{} equal to {} at this point",
                    S::from(*align),
                    addr % (1 << align),
                ),
            ),
            Self::BadAlignFixed(align, align_ofs, addr) => (
                format!("Cannot place the section at address ${addr:04x}"),
                format!(
                    "The lower {align} bit{} of the address are equal to ${} here",
                    S::from(*align),
                    addr % (1 << align),
                ),
            ),
            Self::ConflictingAlign(align, align_ofs, other_align, other_align_ofs) => (
                format!(
                    "Cannot align the section's lower {align} bit{} to {align_ofs}",
                    S::from(*align),
                ),
                format!(
                    "The bits would be equal to {} at this point",
                    align_ofs % (1 << other_align),
                ),
            ),
        }
    }
}

impl ActiveSection {
    pub fn points_to_same_as(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
