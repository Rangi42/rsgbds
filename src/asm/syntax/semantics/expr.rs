use crate::{diagnostics, expr::Expr, Identifier};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn is_expr_constant(&self, expr: Expr, l_span_idx: usize, r_span_idx: usize) -> Expr {
        let value = match self.try_const_eval(&expr) {
            Ok((_value, _span)) => 1,
            Err(err) => {
                if err.can_be_deferred_to_linker() {
                    0
                } else {
                    1 // Report the value as constant, but don't report the error itself.
                }
            }
        };

        Expr::number(value, self.span_from_to(l_span_idx, r_span_idx))
    }

    pub fn expr_def_of_symbol(
        &self,
        ident: Identifier,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = match self.symbols.find(&ident) {
            Some(sym)
                if sym.exists(
                    self.symbols.global_scope.as_ref(),
                    self.sections
                        .active_section
                        .as_ref()
                        .map(|active| &active.sym_section),
                    self.macro_args.last(),
                ) =>
            {
                1
            }
            _ => 0,
        };

        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn bank_of_ident(
        &self,
        (ident_span_idx, ident): (usize, Identifier),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        if let Some(res) = self.symbols.find(&ident).and_then(|sym| {
            sym.get_string(
                self.symbols.global_scope,
                self.symbols.local_scope,
                self.identifiers,
            )
        }) {
            match res {
                Ok(name) => Expr::bank_of_section(name, span),
                Err(err) => {
                    let ident_span = &self.line_spans[ident_span_idx];
                    self.error(ident_span, |error| {
                        error.set_message(&err);
                        error.add_label(
                            diagnostics::error_label(ident_span)
                                .with_message("error evaluating this symbol"),
                        );
                    });
                    Expr::nothing(span)
                }
            }
        } else {
            Expr::bank_of_symbol(ident, span)
        }
    }
}
