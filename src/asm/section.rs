use std::cell::Cell;

use compact_str::CompactString;
use indexmap::{map::Entry, IndexMap};
use rustc_hash::FxBuildHasher;

use crate::{
    common::{section::MemRegion, S},
    diagnostics::{self, warning},
    expr::Expr,
    instructions::Instruction,
    sources::Span,
    symbols::Symbols,
    Identifier, Identifiers, Options,
};

#[derive(Debug)]
pub struct Sections {
    pub sections: IndexMap<CompactString, Section, FxBuildHasher>,
    /// The first is the “data” section, the second is the “symbol” section.
    pub active_section: Option<(ActiveSection, ActiveSection)>,
    section_stack: Vec<(Span, Option<(ActiveSection, ActiveSection)>)>,
}
type SectionId = usize; // Index into the `IndexMap`.

#[derive(Debug)]
pub struct Section {
    // Really, the span of the *first* definition.
    def_span: Span,
    attrs: SectionAttrs,
    patches: Vec<Patch>,
    bytes: Contents,
}
#[derive(Debug)]
enum Contents {
    Data(Vec<u8>),
    NoData(usize),
}
#[derive(Debug)]
pub struct SectionAttrs {
    pub kind: SectionKind,
    pub mem_region: MemRegion,
    pub address: AddrConstraint,
    pub bank: Option<u32>,
}
#[derive(Debug, displaydoc::Display)]
pub enum SectionKind {
    /// normal
    Normal,
    /// union
    Union,
    /// fragment
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
    pub id: SectionId,
    pub offset: usize,
}

#[derive(Debug)]
struct Patch {
    span: Span,
    expr: Expr,
    kind: PatchKind,
    offset: usize,
    pc: (SectionId, usize),
}
#[derive(Debug)]
pub enum PatchKind {
    Byte,
    Word,
    Long,
    Jr,
    // TODO: transfer the HRAM, `rst`, `bit`, `set`, and `res` RPN opcodes to patch kinds
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
        def_span: Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> ActiveSection {
        match self.sections.entry(name) {
            Entry::Vacant(entry) => {
                let bytes = if attrs.mem_region.has_data() {
                    Contents::Data(vec![])
                } else {
                    Contents::NoData(0)
                };
                let id = entry
                    .insert_entry(Section {
                        def_span,
                        attrs,
                        patches: vec![],
                        bytes,
                    })
                    .index();
                ActiveSection { id, offset: 0 }
            }

            Entry::Occupied(mut entry) => {
                let sect = entry.get();
                let offset = match (&attrs.kind, &sect.attrs.kind) {
                    (SectionKind::Normal, SectionKind::Normal) => {
                        diagnostics::error(
                            &def_span,
                            |error| {
                                error.set_message(format!(
                                    "normal-type section \"{}\" redefined",
                                    entry.key(),
                                ));
                                error.add_labels([
                                    diagnostics::error_label(&def_span)
                                        .with_message("redefined here..."),
                                    diagnostics::error_label(&sect.def_span)
                                        .with_message("...original definition here"),
                                ]);
                                error.set_help("declare the section as `union` to overlay the pieces, or as `fragment` to concatenate them");
                            },
                            nb_errors_left,
                            options,
                        );
                        sect.bytes.len()
                    }
                    (SectionKind::Union, SectionKind::Union) => 0, // Restart from the beginning.
                    (SectionKind::Fragment, SectionKind::Fragment) => sect.bytes.len(),

                    (new, old) => {
                        diagnostics::error(
                            &def_span,
                            |error| {
                                error.set_message(format!(
                                    "incorrect type used for section \"{}\"",
                                    entry.key(),
                                ));
                                error.add_labels([
                                    diagnostics::error_label(&def_span)
                                        .with_message(format!("defined again as {new} here...")),
                                    diagnostics::error_label(&sect.def_span).with_message(format!(
                                        "...but originally defined as {old} here"
                                    )),
                                ])
                            },
                            nb_errors_left,
                            options,
                        );
                        sect.bytes.len()
                    }
                };
                if let Err(err) = entry.get_mut().attrs.address.merge(attrs.address, offset) {
                    diagnostics::error(
                        &def_span,
                        |error| {
                            let (msg, details) = err.details();
                            error.set_message(msg);
                            error.add_label(
                                diagnostics::error_label(&def_span).with_message(details),
                            );
                        },
                        nb_errors_left,
                        options,
                    );
                }

                let id = entry.index();
                ActiveSection { id, offset }
            }
        }
    }

    pub fn find(&self, section_id: usize) -> &Section {
        &self.sections[section_id]
    }

    pub fn push_active_section(&mut self, pushs_span: Span) {
        self.section_stack
            .push((pushs_span, self.active_section.take()));
    }
    pub fn pop_active_section(&mut self) -> Option<()> {
        self.active_section = self.section_stack.pop()?.1;
        Some(())
    }

    pub fn warn_if_stack_not_empty(&self, nb_errors_left: &Cell<usize>, options: &Options) {
        if !self.section_stack.is_empty() {
            diagnostics::warn(
                warning!("unmatched-directive"),
                &Span::Builtin,
                |warning| {
                    warning.set_message("`pushs` without matching `pops`");
                    warning.add_labels(self.section_stack.iter().map(|(span, _entry)| {
                        diagnostics::warning_label(span).with_message("no `pops` matches this")
                    }));
                },
                nb_errors_left,
                options,
            )
        }
    }

    pub fn emit_instruction(
        &mut self,
        instruction: Instruction,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some((data_section, symbol_section)) = self.active_section.as_mut() else {
            diagnostics::error(
                &instruction.span,
                |error| {
                    error.set_message("instruction emitted outside of a section");
                    error.add_label(
                        diagnostics::error_label(&instruction.span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|(_span, entry)| entry.is_some())
                    {
                        error.set_help("consider popping a section with `pops` before this");
                    } else {
                        error.set_help("consider opening a section with `section` before this");
                    }
                },
                nb_errors_left,
                options,
            );
            return;
        };

        let data_sect = &mut self.sections[data_section.id];
        if let Contents::Data(data) = &mut data_sect.bytes {
            debug_assert_eq!(data.len(), data_section.offset);
            data.extend_from_slice(&instruction.bytes);

            if let Some(patch) = instruction.patch {
                data_sect.patches.push(Patch {
                    span: instruction.span,
                    expr: patch.expr,
                    kind: patch.kind,
                    offset: data_section.offset + usize::from(patch.offset),
                    pc: (symbol_section.id, symbol_section.offset),
                });
            }
        } else {
            diagnostics::error(
                &instruction.span,
                |error| {
                    error.set_message("instruction emitted outside of ROM");
                    error.add_label(diagnostics::error_label(&instruction.span).with_message(
                        format!(
                            "a {} section is active here",
                            data_sect.attrs.mem_region.name()
                        ),
                    ));
                    error.set_help("you can store the data in ROM while leaving labels pointing to RAM, using `load`");
                },
                nb_errors_left,
                options,
            );
        }

        data_section.offset += instruction.bytes.len();
        symbol_section.offset += instruction.bytes.len();
    }

    // The linker will perform a similar check, but this helps catching such errors early, and not generating huge object files.
    pub fn check_section_sizes(&self, nb_errors_left: &Cell<usize>, options: &Options) {
        for (name, section) in &self.sections {
            let size = match &section.bytes {
                Contents::Data(data) => data.len(),
                Contents::NoData(size) => *size,
            };
            let max_size = section.attrs.mem_region.max_size();

            if size > usize::from(max_size) {
                diagnostics::error(
                    &section.def_span,
                    |error| {
                        error.set_message(format!(
                            "section \"{name}\" is larger than maximum size",
                        ));
                        error.add_label(diagnostics::error_label(&section.def_span).with_message(format!("this section grew to {size} (${size:04x}) bytes, over the limit of {max_size}")));
                        error.set_help("consider splitting this section into multiple ones");
                    },
                    nb_errors_left,
                    options,
                );
            }

            // Also check some section assumptions while we're here.
            debug_assert_eq!(
                section.attrs.mem_region.has_data(),
                matches!(section.bytes, Contents::Data(_)),
                "{name}'s data is inconsistent with its mem region",
            );
            debug_assert!(
                section.attrs.mem_region.has_data() || section.patches.is_empty(),
                "Non-data section {name} has patches!?",
            );
        }
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
                format!("cannot place the section at address ${other_addr:04x}"),
                format!("the section is already fixed at address ${addr:04x}"),
            ),
            Self::FixedBadAlign(addr, align, align_ofs) => (
                format!(
                    "cannot align the section's lower {align} bit{} to {align_ofs}",
                    S::from(*align),
                ),
                format!(
                    "the fixed address would have the bit{} equal to {} at this point",
                    S::from(*align),
                    addr % (1 << align),
                ),
            ),
            Self::BadAlignFixed(align, align_ofs, addr) => (
                format!("cannot place the section at address ${addr:04x}"),
                format!(
                    "the lower {align} bit{} of the address are equal to ${} here",
                    S::from(*align),
                    addr % (1 << align),
                ),
            ),
            Self::ConflictingAlign(align, align_ofs, other_align, other_align_ofs) => (
                format!(
                    "cannot align the section's lower {align} bit{} to {align_ofs}",
                    S::from(*align),
                ),
                format!(
                    "the bits would be equal to {} at this point",
                    align_ofs % (1 << other_align),
                ),
            ),
        }
    }
}

impl ActiveSection {
    pub fn define_label(
        &self,
        name: Identifier,
        symbols: &mut Symbols,
        identifiers: &Identifiers,
        definition: Span,
        exported: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        symbols.define_label(
            name,
            identifiers,
            definition,
            (self.id, self.offset),
            exported,
            nb_errors_left,
            options,
        )
    }
    pub fn points_to_same_as(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Section {
    pub fn address(&self) -> Option<u16> {
        match self.attrs.address {
            AddrConstraint::None | AddrConstraint::Align(_, _) => None,
            AddrConstraint::Addr(addr) => Some(addr),
        }
    }
}

impl Contents {
    fn len(&self) -> usize {
        match self {
            Self::Data(data) => data.len(),
            Self::NoData(len) => *len,
        }
    }
}
