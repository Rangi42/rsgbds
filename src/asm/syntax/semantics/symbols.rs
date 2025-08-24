use compact_str::CompactString;
use either::Either;

use crate::{
    diagnostics,
    expr::{BinOp, Expr},
    sources::Span,
    Identifier,
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn define_label(&mut self, name: Identifier, def_span_id: usize, exported: bool) {
        let def_span = self.nth_span(def_span_id);

        if let Some(active) = self.sections.active_section.as_ref() {
            active.sym_section.define_label(
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
    ) -> Either<Expr, (CompactString, Span)> {
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
            Either::Right((string, span))
        } else {
            // Likewise, assume treat anything else as a symbol expression; non-numeric symbols will get filtered out at evaluation time.
            Either::Left(Expr::symbol(name, span))
        }
    }

    pub fn define_string(
        &mut self,
        export: Option<usize>,
        redef: bool,
        name: Identifier,
        (string, _span): (CompactString, Span),
        span_idx: usize,
    ) {
        if let Some(span_idx) = export {
            let span = &self.line_spans[span_idx];
            self.error(span, |error| {
                error.set_message("cannot export string constants");
                error.add_label(
                    diagnostics::error_label(span).with_message("consider removing this"),
                );
            });
        }

        let span = self.nth_span(span_idx);

        self.symbols.define_string(
            name,
            self.identifiers,
            span,
            string,
            redef,
            self.sections
                .active_section
                .as_ref()
                .map(|active| &active.sym_section),
            self.macro_args.last(),
            self.nb_errors_left,
            self.options,
        );
    }

    pub fn define_numeric(
        &mut self,
        export: Option<usize>,
        redef: bool,
        name: Identifier,
        mutable: bool,
        expr: Expr,
        span_idx: usize,
    ) {
        match self.try_const_eval(&expr) {
            Ok((value, _span)) => {
                let span = self.nth_span(span_idx);

                self.symbols.define_constant(
                    name,
                    self.identifiers,
                    span,
                    value,
                    mutable,
                    export.is_some(),
                    redef,
                    self.sections
                        .active_section
                        .as_ref()
                        .map(|active| &active.sym_section),
                    self.macro_args.last(),
                    self.nb_errors_left,
                    self.options,
                );
            }
            Err(error) => self.report_expr_error(error),
        }
    }

    pub fn compound_assignment(
        &mut self,
        export: Option<usize>,
        redef: bool,
        name: Identifier,
        operator: BinOp,
        expr: Expr,
        op_span_idx: usize,
        name_span_idx: usize,
    ) {
        let name_span = self.nth_span(name_span_idx);
        let synthetic_expr = Expr::symbol(name, name_span).binary_op(operator, expr, Span::Builtin); // This span is not used.

        match self.try_const_eval(&synthetic_expr) {
            Ok((value, _span)) => {
                let span = self.nth_span(op_span_idx);
                self.symbols.define_constant(
                    name,
                    self.identifiers,
                    span,
                    value,
                    true, // Mutable.
                    export.is_some(),
                    redef,
                    self.sections
                        .active_section
                        .as_ref()
                        .map(|active| &active.sym_section),
                    self.macro_args.last(),
                    self.nb_errors_left,
                    self.options,
                );
            }
            Err(error) => self.report_expr_error(error),
        }
    }

    pub fn define_rs(
        &mut self,
        export: Option<usize>,
        redef: bool,
        name: Identifier,
        stride: i32,
        maybe_expr: Option<Expr>,
        span_idx: usize,
    ) {
        let offset = match maybe_expr {
            Some(expr) => self.try_const_eval(&expr).map(|(value, _span)| value),
            None => Ok(1),
        };
        match offset {
            Ok(value) => {
                let rs = self.symbols.rs();
                let constant_value = std::mem::replace(rs, rs.wrapping_add(value * stride));

                let span = self.nth_span(span_idx);

                self.symbols.define_constant(
                    name,
                    self.identifiers,
                    span,
                    constant_value,
                    false,
                    export.is_some(),
                    redef,
                    self.sections
                        .active_section
                        .as_ref()
                        .map(|active| &active.sym_section),
                    self.macro_args.last(),
                    self.nb_errors_left,
                    self.options,
                );
            }
            Err(error) => self.report_expr_error(error),
        }
    }

    pub fn export_symbol(&mut self, name: Identifier, span_idx: usize) {
        let span = self.nth_span(span_idx);
        self.symbols.export(
            name,
            span,
            self.identifiers,
            self.nb_errors_left,
            self.options,
        );
    }

    pub fn delete_symbols(&mut self, names: Vec<(usize, Identifier)>) {
        for (span_idx, name) in names {
            let span = self.nth_span(span_idx);
            self.symbols.delete(
                name,
                span,
                self.identifiers,
                self.nb_errors_left,
                self.options,
            );
        }
    }

    pub fn reset_rs(&mut self) {
        *self.symbols.rs() = 0;
    }

    pub fn set_rs(&mut self, expr: Expr) {
        match self.try_const_eval(&expr) {
            Ok((value, _span)) => *self.symbols.rs() = value,
            Err(error) => self.report_expr_error(error),
        }
    }
}
