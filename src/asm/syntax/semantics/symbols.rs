use compact_str::CompactString;
use either::Either;

use crate::{diagnostics, expr::Expr, Identifier};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn define_label(&mut self, name: Identifier, def_span_id: usize, exported: bool) {
        let def_span = self.nth_span(def_span_id);

        if let Some((_data_section, sym_section)) = &self.sections.active_section {
            sym_section.define_label(
                name,
                self.symbols,
                self.identifiers,
                def_span,
                exported,
                self.macro_args.last(),
                self.nb_errors_left,
                self.options,
            );
        } else {
            self.error(&def_span, |error| {
                error.set_message("label defined outside of a section");
                error.add_label(
                    diagnostics::error_label(&def_span)
                        .with_message("no section is active at this point"),
                );
            });
        }
    }

    pub fn string_or_numeric_sym(
        &mut self,
        name: Identifier,
        span_idx: usize,
    ) -> Either<Expr, CompactString> {
        let span = self.nth_span(span_idx);

        let Some(sym) = self.symbols.find(&name) else {
            // Any undefined symbol, assume is a label (reference).
            return Either::Left(Expr::symbol(name, span));
        };

        if let Some(res) = sym.get_string(
            self.symbols.global_scope,
            self.symbols.local_scope,
            self.identifiers,
        ) {
            let string = res.unwrap_or_else(|err| {
                self.error(&span, |error| {
                    error.set_message(&err);
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("error evaluating this symbol"),
                    );
                });
                Default::default()
            });
            Either::Right(string)
        } else {
            // Likewise, assume treat anything else as a symbol expression; non-numeric symbols will get filtered out at evaluation time.
            Either::Left(Expr::symbol(name, span))
        }
    }
}
