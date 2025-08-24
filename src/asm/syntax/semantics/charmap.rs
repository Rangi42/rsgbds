use compact_str::CompactString;

use crate::{diagnostics, sources::Span, Identifier, Identifiers};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn charmap_entry(&mut self, (string, span): (CompactString, Span), values: Vec<i32>) {
        if self
            .charmaps
            .active_charmap_mut()
            .add_mapping(&string, values)
            .is_none()
        {
            self.error(&span, |error| {
                error.set_message("cannot charmap an empty string");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this string shouldn't be empty"),
                );
            });
        }
    }

    pub fn create_charmap(
        &mut self,
        name: Identifier,
        source: Option<Identifier>,
        span_idx: usize,
    ) {
        let def_span = self.nth_span(span_idx);
        let res = match source {
            Some(ident) => self.charmaps.make_copy(name, def_span, ident),
            None => self.charmaps.make_new(name, def_span),
        };

        if let Err(err) = res {
            self.error(err.diag_span(), |error| {
                err.make_diag(error, self.identifiers);
            });
        }
    }

    pub fn switch_to_charmap(&mut self, name: Identifier, span_idx: usize) {
        let span = &self.line_spans[span_idx];
        if self.charmaps.switch_to(name).is_none() {
            self.error(span, |error| {
                error.set_message(format!(
                    "no charmap named \"{}\" exists",
                    self.identifiers.resolve(name).unwrap(),
                ));
                error.add_label(
                    diagnostics::error_label(span).with_message("cannot switch to this charmap"),
                );
            });
        }
    }

    pub fn push_charmap(&mut self) {
        self.charmaps.push_active_charmap();
    }

    pub fn pop_charmap(&mut self, span_idx: usize) {
        if self.charmaps.pop_active_charmap().is_none() {
            let span = &self.line_spans[span_idx];
            self.error(span, |error| {
                error.set_message("no entries in the charmap stack");
                error.add_label(diagnostics::error_label(span).with_message("cannot pop"));
            })
        }
    }
}
