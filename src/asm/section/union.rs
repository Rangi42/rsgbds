use std::cell::Cell;

use crate::{diagnostics, section::SectionKind, sources::Span, Options};

use super::{ActiveSection, Contents, SectionMap, Sections, UnionEntry};

impl Sections {
    pub fn enter_union(
        &mut self,
        keyword_span: Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                &keyword_span,
                |error| {
                    error.set_message("`union` used outside of a section");
                    error.add_label(
                        diagnostics::error_label(&keyword_span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|entry| entry.active_section.is_some())
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

        let data_sect = &mut self.sections[active.data_section.id];
        match &mut data_sect.bytes {
            Contents::Data(_data) => {
                diagnostics::error(
                    &keyword_span,
                    |error| {
                        error.set_message("`union` used in ROM");
                        error.add_label(diagnostics::error_label(&keyword_span).with_message(
                            format!(
                                "a {} section is active here",
                                data_sect.attrs.mem_region.name(),
                            ),
                        ));
                        error.set_help("`union` can only be used in RAM sections");
                    },
                    nb_errors_left,
                    options,
                );
            }

            Contents::NoData(len) => {
                // Currently, unions are restricted to non-code sections, which prevents them interacting with `load` blocks.
                // Lift this restriction at your own peril!
                debug_assert!(!active.is_load_block_active());
                debug_assert_eq!(active.data_section.offset, active.sym_section.offset);

                if !matches!(data_sect.attrs.kind, SectionKind::Union) && active.unions.is_empty() {
                    debug_assert_eq!(*len, active.data_section.offset);
                }
                active.unions.push(UnionEntry {
                    offset_at_entry: active.data_section.offset,
                    span: keyword_span,
                    overall_size: 0,
                });
            }
        }
    }

    pub fn next_union_block(
        &mut self,
        keyword_span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("`nextu` used outside of a section");
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|entry| entry.active_section.is_some())
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
        let Some(union) = active.unions.last_mut() else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("`nextu` used outside of a union");
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no `union` matches this"),
                    );
                },
                nb_errors_left,
                options,
            );
            return;
        };

        union.record_size_of_block(&active.data_section, &active.sym_section, &self.sections);
        active.data_section.offset = union.offset_at_entry;
        active.sym_section.offset = union.offset_at_entry;
    }

    pub fn end_union_block(
        &mut self,
        keyword_span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("`endu` used outside of a section");
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|entry| entry.active_section.is_some())
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
        if let Some(mut union) = active.unions.pop() {
            union.record_size_of_block(&active.data_section, &active.sym_section, &self.sections);
            active.data_section.offset = union.offset_at_entry + union.overall_size;
            active.sym_section.offset = union.offset_at_entry + union.overall_size;
            let data_sect = &self.sections[active.data_section.id];
            if !matches!(data_sect.attrs.kind, SectionKind::Union) && active.unions.is_empty() {
                debug_assert_eq!(data_sect.bytes.len(), active.data_section.offset);
            }
        } else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("`endu` used outside of a union");
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no `union` matches this"),
                    );
                },
                nb_errors_left,
                options,
            );
        }
    }
}

impl UnionEntry {
    fn record_size_of_block(
        &mut self,
        data_section: &ActiveSection,
        sym_section: &ActiveSection,
        sections: &SectionMap,
    ) {
        debug_assert_eq!(
            data_section.id, sym_section.id,
            "`union` used in `load` block!!"
        );
        debug_assert_eq!(data_section.offset, sym_section.offset);
        debug_assert!(data_section.offset <= sections[data_section.id].bytes.len()); // The length should have been kept track of.
        let this_block_size = data_section.offset - self.offset_at_entry;
        self.overall_size = std::cmp::max(self.overall_size, this_block_size);
    }
}
