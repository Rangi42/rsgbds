use std::cell::Cell;

use compact_str::CompactString;
use indexmap::{map::Entry, IndexMap};
use rustc_hash::FxBuildHasher;

use crate::{
    common::{section::MemRegion, S},
    diagnostics::{self, warning},
    expr::Expr,
    macro_args::MacroArgs,
    sources::Span,
    symbols::Symbols,
    Identifier, Identifiers, Options,
};

/// Contains more `impl`s on [`Sections`], related to emitting data.
mod data;
mod union;

#[derive(Debug)]
pub struct Sections {
    pub sections: SectionMap,
    pub active_section: Option<ActiveSections>,
    section_stack: Vec<SectionStackEntry>,
    pub assertions: Vec<Assertion>,
}
pub type SectionMap = IndexMap<CompactString, Section, FxBuildHasher>;
pub type SectionId = usize; // Index into the `IndexMap`.
#[derive(Debug)]
pub struct ActiveSections {
    /// The ID of the section to which bytes are emitted.
    pub data_section: ActiveSection,
    /// The ID of the section in which PC is.
    pub sym_section: ActiveSection,
    // TODO(perf): consider using a `smallvec` instead, this is hardly ever more than 1
    unions: Vec<UnionEntry>,
}
#[derive(Debug)]
struct UnionEntry {
    offset_at_entry: usize,
    overall_size: usize,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActiveSection {
    pub id: SectionId,
    pub offset: usize,
    sym_scopes: Vec<Option<Identifier>>, // TODO(perf): consider a `smallvec` instead
}

#[derive(Debug)]
pub struct Section {
    // Really, the span of the *first* definition.
    pub def_span: Span,
    pub attrs: SectionAttrs,
    pub patches: Vec<Patch>,
    pub bytes: Contents,
}
#[derive(Debug)]
pub enum Contents {
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

#[derive(Debug)]
pub struct Patch {
    pub kind: PatchKind,
    pub rest: LinkTimeExpr,
}
#[derive(Debug)]
pub struct Assertion {
    pub level: AssertLevel,
    pub rest: LinkTimeExpr,
    pub message: CompactString,
}
#[derive(Debug)]
pub struct LinkTimeExpr {
    pub span: Span,
    pub expr: Expr,
    pub offset: usize,
    pub pc: Option<(SectionId, usize)>,
}
#[derive(Debug)]
pub enum PatchKind {
    Byte,
    Word,
    Long,
    Jr,
    // TODO: transfer the HRAM, `rst`, `bit`, `set`, and `res` RPN opcodes to patch kinds
}
#[derive(Debug, Clone, Copy)]
pub enum AssertLevel {
    Warn,
    Error,
    Fatal,
}

#[derive(Debug)]
pub struct SectionStackEntry {
    pushs_span: Span,
    active_section: Option<ActiveSections>,
}

impl Sections {
    pub fn new() -> Self {
        Self {
            sections: IndexMap::with_hasher(FxBuildHasher),
            active_section: None,
            section_stack: vec![],
            assertions: vec![],
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
                ActiveSection {
                    id,
                    offset: 0,
                    sym_scopes: vec![],
                }
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
                // Reset the symbol scope, even when entering a fragment of an existing section.
                ActiveSection {
                    id,
                    offset,
                    sym_scopes: vec![],
                }
            }
        }
    }

    pub fn open_load_block(
        &mut self,
        new_active_section: ActiveSection,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.reject_active_union(span, nb_errors_left, options);

        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("`load` used outside of a section");
                    error.add_label(
                        diagnostics::error_label(span).with_message("this directive is invalid"),
                    );
                },
                nb_errors_left,
                options,
            );
            return;
        };
        if active.is_load_block_active() {
            diagnostics::warn(
                warning!("unterminated-load"),
                span,
                |error| {
                    error.set_message("`load` block terminated by another `load`");
                    error.add_label(
                        diagnostics::warning_label(span)
                            .with_message("no `endl` before this point"),
                    );
                },
                nb_errors_left,
                options,
            );
        }

        let section = &self.sections[new_active_section.id];
        if section.attrs.mem_region.has_data() {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("`load` blocks cannot designate a data section");
                    error.add_label(diagnostics::error_label(span).with_message(format!(
                        "this section is in {}",
                        section.attrs.mem_region.name(),
                    )));
                },
                nb_errors_left,
                options,
            );
        }

        if active.is_section_active(new_active_section.id)
            || Self::is_in_stack(&self.section_stack, new_active_section.id)
        {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("`load` cannot designate an active section");
                    error.add_label(diagnostics::error_label(span).with_message(format!(
                        "section \"{}\" is {} at this point",
                        self.sections.keys()[new_active_section.id],
                        if active.is_section_active(new_active_section.id) {
                            "active"
                        } else {
                            "in the stack"
                        },
                    )));
                },
                nb_errors_left,
                options,
            );
        } else {
            let prev_sym_sect = std::mem::replace(&mut active.sym_section, new_active_section);
            active.data_section.sym_scopes = prev_sym_sect.sym_scopes;
        }
    }

    pub fn close_load_block(
        &mut self,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("`endl` used outside of a section");
                    error.add_label(
                        diagnostics::error_label(span).with_message("this directive is invalid"),
                    );
                },
                nb_errors_left,
                options,
            );
            return;
        };

        if active.is_load_block_active() {
            // End the `LOAD` block.
            active.sym_section = active.data_section.clone();
        } else {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("`endl` used outside of a `load` block");
                    error.add_label(
                        diagnostics::error_label(span).with_message("this directive is invalid"),
                    );
                },
                nb_errors_left,
                options,
            );
        }
    }

    pub fn find(&self, section_id: usize) -> &Section {
        &self.sections[section_id]
    }

    pub fn end_section(&mut self, span: &Span, nb_errors_left: &Cell<usize>, options: &Options) {
        match self.active_section.take() {
            None => diagnostics::error(
                span,
                |error| {
                    error.set_message("cannot end a lack of section");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("no section is active at this point"),
                    );
                },
                nb_errors_left,
                options,
            ),
            Some(active) => {
                if active.is_load_block_active() {
                    diagnostics::warn(
                        warning!("unterminated-load"),
                        span,
                        |error| {
                            error.set_message("`load` block terminated by `endsection`");
                            error.add_label(
                                diagnostics::warning_label(span)
                                    .with_message("no `endl` before this point"),
                            );
                        },
                        nb_errors_left,
                        options,
                    );
                }

                if let Some(union) = active.unions.last() {
                    diagnostics::error(
                        &union.span,
                        |error| {
                            error.set_message("unclosed `union`");
                            error.add_labels([
                                diagnostics::note_label(&union.span)
                                    .with_message("this `union` doesn't have a matching `endu`..."),
                                diagnostics::error_label(span).with_message("...before this"),
                            ]);
                        },
                        nb_errors_left,
                        options,
                    );
                }
            }
        }
    }

    pub fn reject_active_union(
        &self,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        if let Some(union) = self
            .active_section
            .as_ref()
            .and_then(|active| active.unions.last())
        {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("unclosed `union`");
                    error.add_labels([
                        diagnostics::note_label(&union.span)
                            .with_message("this `union doesn't have a matching `endu`..."),
                        diagnostics::error_label(span).with_message("...before this"),
                    ]);
                },
                nb_errors_left,
                options,
            )
        }
    }
    pub fn switch_to(
        &mut self,
        new_active: ActiveSection,
        keyword_span: &Span,
        keyword_name: &str,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.reject_active_union(keyword_span, nb_errors_left, options);

        if Self::is_in_stack(&self.section_stack, new_active.id) {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("cannot switch to a section currently in the stack");
                    error.add_label(diagnostics::error_label(keyword_span).with_message(format!(
                        "\"{}\" is in the section stack at this point",
                        self.sections.get_index(new_active.id).unwrap().0,
                    )));
                },
                nb_errors_left,
                options,
            );
        } else {
            if self
                .active_section
                .as_ref()
                .is_some_and(|active| active.is_load_block_active())
            {
                diagnostics::warn(
                    warning!("unterminated-load"),
                    keyword_span,
                    |error| {
                        error.set_message(format!("`load` block terminated by `{keyword_name}`"));
                        error.add_label(
                            diagnostics::warning_label(keyword_span)
                                .with_message("no `endl` before this point"),
                        );
                    },
                    nb_errors_left,
                    options,
                );
            }

            self.active_section = Some(ActiveSections::new(new_active));
        }
    }

    pub fn is_active_or_in_stack(&self, section_id: usize) -> bool {
        let refs_section = |opt: &Option<ActiveSections>| {
            opt.as_ref()
                .is_some_and(|active| active.is_section_active(section_id))
        };
        refs_section(&self.active_section) || Self::is_in_stack(&self.section_stack, section_id)
    }
    fn is_in_stack(section_stack: &[SectionStackEntry], section_id: usize) -> bool {
        let refs_section = |opt: &Option<ActiveSections>| {
            opt.as_ref()
                .is_some_and(|active| active.is_section_active(section_id))
        };
        section_stack
            .iter()
            .any(|entry| refs_section(&entry.active_section))
    }
    pub fn push_active_section(&mut self, pushs_span: Span) {
        let entry = SectionStackEntry {
            pushs_span,
            active_section: self.active_section.take(),
        };
        self.section_stack.push(entry);
    }
    pub fn pop_active_section(
        &mut self,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Option<()> {
        self.reject_active_union(span, nb_errors_left, options);

        if self
            .active_section
            .as_ref()
            .is_some_and(|active| active.is_load_block_active())
        {
            diagnostics::warn(
                warning!("unterminated-load"),
                span,
                |error| {
                    error.set_message("`load` block terminated by `pops`");
                    error.add_label(
                        diagnostics::warning_label(span)
                            .with_message("no `endl` before this point"),
                    );
                },
                nb_errors_left,
                options,
            );
        }

        let entry = self.section_stack.pop()?;
        self.active_section = entry.active_section;
        Some(())
    }

    pub fn warn_if_stack_not_empty(&self, nb_errors_left: &Cell<usize>, options: &Options) {
        if !self.section_stack.is_empty() {
            diagnostics::warn(
                warning!("unmatched-directive"),
                &Span::Builtin,
                |warning| {
                    warning.set_message("`pushs` without matching `pops`");
                    warning.add_labels(self.section_stack.iter().map(|entry| {
                        diagnostics::warning_label(&entry.pushs_span)
                            .with_message("no `pops` matches this")
                    }));
                },
                nb_errors_left,
                options,
            )
        }
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

    pub fn warn_if_unclosed_load_block(&self, nb_errors_left: &Cell<usize>, options: &Options) {
        match self.active_section.as_ref() {
            Some(active) if active.is_load_block_active() => {
                let span = &self.sections[active.sym_section.id].def_span;
                diagnostics::warn(
                    warning!("unterminated-load"),
                    span,
                    |warning| {
                        warning.set_message("unterminated `load` block");
                        warning.add_label(
                            diagnostics::warning_label(span)
                                .with_message("this section is terminated by the end of input"),
                        );
                    },
                    nb_errors_left,
                    options,
                );
            }
            _ => {}
        }
    }

    pub fn all_link_time_exprs(&self) -> impl Iterator<Item = &LinkTimeExpr> {
        self.sections
            .iter()
            .flat_map(|(_name, sect)| &sect.patches)
            .map(|patch| &patch.rest)
            .chain(self.assertions.iter().map(|assertion| &assertion.rest))
    }
}

impl From<(u8, u16)> for AddrConstraint {
    fn from((alignment, offset): (u8, u16)) -> Self {
        match alignment {
            0 => Self::None,
            1..=15 => Self::Align(alignment, offset),
            16.. => Self::Addr(offset),
        }
    }
}
impl AddrConstraint {
    pub fn align_mask(&self) -> u16 {
        match self {
            Self::None => 0,
            Self::Align(alignment, _) => (1 << *alignment) - 1,
            Self::Addr(_) => 0xFFFF,
        }
    }
    pub fn align_ofs(&self) -> u16 {
        match self {
            Self::None => 0,
            Self::Align(_, align_ofs) => *align_ofs,
            Self::Addr(addr) => *addr,
        }
    }

    pub fn merge(&mut self, other: Self, offset: usize) -> Result<(), MergeError> {
        match (self as &mut _, other) {
            // If the new constraint is nonexistent, then there is nothing to do.
            (_, Self::None) => Ok(()),
            // In these cases, the new constraint simply replaces the old one, with no chance of conflict either.
            (Self::None, Self::Align(align, align_ofs)) => {
                *self = Self::Align(align, align_ofs.wrapping_sub(offset as u16) % (1 << align));
                Ok(())
            }
            (Self::None, Self::Addr(addr)) => {
                *self = Self::Addr(addr.wrapping_sub(offset as u16));
                Ok(())
            }
            // Now we get to the interesting cases.
            (Self::Align(align, align_ofs), Self::Align(other_align, other_align_ofs)) => {
                let cur_align_offset = align_ofs.wrapping_add(offset as u16) % (1 << *align);
                if other_align_ofs % (1 << *align) == cur_align_offset % (1 << other_align) {
                    if *align < other_align {
                        *align = other_align;
                        *align_ofs =
                            other_align_ofs.wrapping_sub(offset as u16) % (1 << other_align);
                    }
                    Ok(())
                } else {
                    Err(MergeError::ConflictingAlign(
                        *align,
                        cur_align_offset,
                        other_align,
                        other_align_ofs,
                    ))
                }
            }
            (Self::Align(align, align_ofs), Self::Addr(addr)) => {
                let cur_align_offset = align_ofs
                    .wrapping_add(offset as u16)
                    .rem_euclid(1 << *align);
                if addr % (1 << *align) == cur_align_offset {
                    *self = Self::Addr(addr.wrapping_sub(offset as u16));
                    Ok(())
                } else {
                    Err(MergeError::BadAlignFixed(*align, cur_align_offset, addr))
                }
            }
            (Self::Addr(addr), Self::Align(align, align_ofs)) => {
                let cur_addr = addr.wrapping_add(offset as u16);
                if cur_addr % (1 << align) == align_ofs {
                    Ok(())
                } else {
                    Err(MergeError::FixedBadAlign(cur_addr, align, align_ofs))
                }
            }
            (Self::Addr(addr), Self::Addr(other_addr)) => {
                let cur_addr = addr.wrapping_add(offset as u16);
                if cur_addr == other_addr {
                    Ok(())
                } else {
                    Err(MergeError::AddrMismatch(cur_addr, other_addr))
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
                    "the fixed address would have the bit{} equal to ${:02x} at this point",
                    S::from(*align),
                    addr % (1 << align),
                ),
            ),
            Self::BadAlignFixed(align, align_ofs, addr) => (
                format!("cannot place the section at address ${addr:04x}"),
                format!(
                    "the lower {align} bit{} of the address are equal to ${align_ofs:02x} here",
                    S::from(*align),
                ),
            ),
            Self::ConflictingAlign(align, align_ofs, other_align, other_align_ofs) => (
                format!(
                    "cannot align the section's lower {other_align} bit{} to ${other_align_ofs:02x}",
                    S::from(*other_align),
                ),
                format!(
                    "the lower {align} bit{} would be equal to ${align_ofs:02x} at this point",
                    S::from(*align),
                ),
            ),
        }
    }
}

impl ActiveSections {
    pub fn new(section: ActiveSection) -> Self {
        Self {
            data_section: section.clone(),
            sym_section: section,
            unions: vec![],
        }
    }

    pub fn is_load_block_active(&self) -> bool {
        self.data_section.id != self.sym_section.id
    }

    fn is_section_active(&self, sect_id: usize) -> bool {
        self.data_section.id == sect_id || self.sym_section.id == sect_id
    }

    pub fn advance_by(&mut self, nb_bytes: usize) {
        self.data_section.offset += nb_bytes;
        self.sym_section.offset += nb_bytes;
    }

    pub fn sym_scope(&self, index: usize) -> Option<Identifier> {
        self.sym_section.sym_scopes.get(index).copied().flatten()
    }

    pub fn set_sym_scope(&mut self, name: Identifier, identifiers: &Identifiers) {
        let name_str = identifiers.resolve(name).unwrap();
        let depth = name_str
            .as_bytes()
            .iter()
            .filter(|&&byte| byte == b'.')
            .count();
        self.sym_section.sym_scopes.resize(depth + 1, None);
        self.sym_section.sym_scopes[depth] = Some(name);
    }

    pub fn define_label(
        &mut self,
        name: Identifier,
        symbols: &mut Symbols,
        identifiers: &Identifiers,
        definition: Span,
        exported: bool,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let sect_id = self.sym_section.id;
        let offset = self.sym_section.offset;
        symbols.define_label(
            name,
            identifiers,
            definition,
            (sect_id, offset),
            exported,
            self,
            macro_args,
            nb_errors_left,
            options,
        )
    }
}

impl Section {
    pub fn address(&self) -> Option<u16> {
        match self.attrs.address {
            AddrConstraint::None | AddrConstraint::Align(_, _) => None,
            AddrConstraint::Addr(addr) => Some(addr),
        }
    }

    pub fn bank(&self) -> Option<u32> {
        if self.attrs.mem_region.is_banked() {
            self.attrs.bank
        } else {
            Some(self.attrs.mem_region.min_bank())
        }
    }

    pub fn bytes_until_alignment(&self, alignment: u8, offset: u16, ofs_into_sect: usize) -> usize {
        let (cur_align, cur_offset) = match self.attrs.address {
            AddrConstraint::None => return 0, // The section is going to be newly constrained.
            AddrConstraint::Align(cur_alignment, pc_value) => {
                debug_assert!(cur_alignment < 16, "alignment {cur_alignment} >= 16!?");
                (cur_alignment, pc_value)
            }
            AddrConstraint::Addr(pc_value) => (16, pc_value),
        };

        // We need `(cur_offset + ofs_into_sect + return value) % (1 << alignment) == offset`
        usize::from(offset)
            .wrapping_sub(ofs_into_sect)
            .wrapping_sub(cur_offset.into())
            .rem_euclid(1 << std::cmp::min(alignment, cur_align))
    }
}

impl Contents {
    pub fn len(&self) -> usize {
        match self {
            Self::Data(data) => data.len(),
            Self::NoData(len) => *len,
        }
    }
}
