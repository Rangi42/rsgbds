use std::fmt::Display;

use compact_str::CompactString;
use either::Either;

use crate::{
    diagnostics::{self, warning},
    expr::{BinOp, Expr, OpKind},
    sources::Span,
    symbols::{SymbolData, SymbolKind},
    Identifier, Identifiers,
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn define_label(&mut self, name: Option<Identifier>, def_span_id: usize, exported: bool) {
        let def_span = self.nth_span(def_span_id);

        if let Some(active) = self.sections.active_section.as_mut() {
            if let Some(ident) = name {
                active.define_label(
                    ident,
                    self.symbols,
                    self.identifiers,
                    def_span,
                    exported,
                    self.macro_args.last(),
                    self.nb_errors_left,
                    self.options,
                );
            }
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

    pub fn define_anon_label(&mut self, def_span_id: usize) {
        let def_span = self.nth_span(def_span_id);

        if let Some(active) = self.sections.active_section.as_mut() {
            active.define_label(
                self.symbols.cur_anon_label(self.identifiers),
                self.symbols,
                self.identifiers,
                def_span,
                false, // Anonymous labels are never exported.
                self.macro_args.last(),
                self.nb_errors_left,
                self.options,
            );
            self.symbols.defined_anon_label();
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

    pub fn resolve_local_ident(
        &mut self,
        mut name: CompactString,
        span_idx: usize,
    ) -> Option<Identifier> {
        if let Some(scope) = self
            .sections
            .active_section
            .as_ref()
            .and_then(|active| active.sym_scope(0))
        {
            let scope_name = self.identifiers.resolve(scope).unwrap();
            debug_assert!(!scope_name.contains('.'), "scope = {scope_name:?}");
            name.insert_str(0, scope_name);
            Some(self.identifiers.get_or_intern(&name))
        } else {
            let span = &self.line_spans[span_idx];
            self.error(span, |error| {
                error.set_message("local symbol in main scope");
                error.add_label(
                    diagnostics::error_label(span).with_message("no global label before this"),
                );
            });
            None
        }
    }

    pub fn anon_label_name(&mut self, offset: i32, span_idx: usize) -> Option<Identifier> {
        let span = &self.line_spans[span_idx];
        self.symbols.anon_label_name(
            offset,
            self.identifiers,
            span,
            self.nb_errors_left,
            self.options,
        )
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

        if let Some(res) = sym.get_string(self.sections.active_section.as_ref(), self.identifiers) {
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
            self.sections.active_section.as_ref(),
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
                    self.sections.active_section.as_ref(),
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
                    self.sections.active_section.as_ref(),
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
                    false, // Not mutable.
                    export.is_some(),
                    redef,
                    self.sections.active_section.as_ref(),
                    self.macro_args.last(),
                    self.nb_errors_left,
                    self.options,
                );
            }
            Err(error) => self.report_expr_error(error),
        }
    }

    pub fn define_function(
        &mut self,
        redef: bool,
        name: Identifier,
        param_names: Vec<Identifier>,
        expr: Expr,
        span_idx: usize,
    ) {
        let span = &self.line_spans[span_idx];

        // Check for shadowing.
        // TODO

        // Check for by-reference captures, and tag the referenced symbols to warn on deletion/mutation.
        // TODO

        // Check for unused arguments.
        let mut unused: Vec<_> = param_names
            .iter()
            .map(|ident| !self.identifiers.resolve(*ident).unwrap().starts_with('_')) // A leading `_` suppresses the warning.
            .collect();
        for op in expr.ops() {
            if let OpKind::Symbol(ident) = &op.kind {
                if let Some(idx) = param_names.iter().position(|param| *param == *ident) {
                    unused[idx] = false;
                }
            }
        }
        let mut unused_iter = unused.iter();
        if let Some(last_unused) = unused_iter.rposition(|is_unused| *is_unused) {
            let rest = unused_iter.as_slice();
            if unused_iter.any(|is_unused| *is_unused) {
                self.warn(warning!("unused-func-param"), span, |warning| {
                    warning.set_message("unused function parameters");
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "{}and {} are not used within the function's body",
                        Params(rest, &param_names, self.identifiers),
                        self.identifiers.resolve(param_names[last_unused]).unwrap(),
                    )));
                    struct Params<'idents, 'names, 'unused>(
                        &'unused [bool],
                        &'names [Identifier],
                        &'idents Identifiers,
                    );
                    impl Display for Params<'_, '_, '_> {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            let mut iter = self.0.iter();
                            while let Some(idx) = iter.position(|is_unused| *is_unused) {
                                write!(f, "{}, ", self.2.resolve(self.1[idx]).unwrap())?;
                            }
                            Ok(())
                        }
                    }
                });
            } else {
                self.warn(warning!("unused-func-param"), span, |warning| {
                    warning.set_message("unused function parameter");
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "{} is not used within the function's body",
                        self.identifiers.resolve(param_names[last_unused]).unwrap(),
                    )));
                });
            }
        }

        let span = self.nth_span(span_idx);
        self.symbols.define_function(
            name,
            self.identifiers,
            span,
            param_names,
            expr,
            redef,
            self.sections.active_section.as_ref(),
            self.macro_args.last(),
            self.nb_errors_left,
            self.options,
        )
    }

    pub fn export_symbol(&mut self, name: Option<Identifier>, span_idx: usize) {
        if let Some(ident) = name {
            let span = self.nth_span(span_idx);
            self.symbols.export(
                ident,
                span,
                self.identifiers,
                self.nb_errors_left,
                self.options,
            );
        }
    }

    pub fn delete_symbol(&mut self, name: Option<Identifier>, span_idx: usize) {
        if let Some(ident) = name {
            let span = self.nth_span(span_idx);
            self.symbols.delete(
                ident,
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

    pub fn section_of(
        &self,
        (name, span): (Option<Identifier>, &Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let opt = if let Some(ident) = name {
            match self.symbols.find_not_placeholder(&ident) {
                None => {
                    self.error(span, |error| {
                        error.set_message(format!(
                            "no symbol called `{}`",
                            self.identifiers.resolve(ident).unwrap(),
                        ));
                        error.add_label(
                            diagnostics::error_label(span)
                                .with_message("cannot get the section of that symbol"),
                        );
                    });
                    None
                }
                Some(SymbolData::User {
                    kind: SymbolKind::Label { section_id, .. },
                    ..
                }) => Some(*section_id),
                Some(SymbolData::Pc) => match self.sections.active_section.as_ref() {
                    None => {
                        self.error(span, |error| {
                            error.set_message("PC does not belong to any section here");
                            error.add_label(
                                diagnostics::error_label(span)
                                    .with_message("no section is active at this point"),
                            );
                        });
                        None
                    }
                    Some(active) => Some(active.sym_section.id),
                },
                Some(kind) => {
                    self.error(span, |error| {
                        error.set_message(format!(
                            "`{}` does not belong to a section",
                            self.identifiers.resolve(ident).unwrap(),
                        ));
                        error.add_label(diagnostics::error_label(span).with_message(format!(
                            "{} symbols do not belong to any section",
                            kind.kind_name(),
                        )));
                    });
                    None
                }
            }
        } else {
            None
        };
        let string = opt.map_or_else(Default::default, |section_id| {
            self.sections
                .sections
                .get_index(section_id)
                .unwrap()
                .0
                .clone()
        });

        let span = self.span_from_to(l_span_idx, r_span_idx);
        (string, span)
    }
}
