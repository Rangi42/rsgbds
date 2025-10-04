use crate::{
    diagnostics,
    expr::{Expr, OpKind},
    symbols::{SymbolData, SymbolKind},
    Identifier,
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn function_call(
        &mut self,
        ident: Identifier,
        lazy_eval: Option<usize>,
        args: Vec<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let Some(symbol) = self.symbols.find(&ident) else {
            self.error(&span, |error| {
                error.set_message(format!(
                    "no function called `{}`",
                    self.identifiers.resolve(ident).unwrap(),
                ));
                error.add_label(diagnostics::error_label(&span).with_message("no such function"));
            });
            return Expr::nothing(span);
        };
        let SymbolData::User {
            kind: SymbolKind::Function { param_names, expr },
            ..
        } = symbol
        else {
            self.error(&span, |error| {
                error.set_message(format!(
                    "`{}` is not a function",
                    self.identifiers.resolve(ident).unwrap(),
                ));
                error.add_labels([
                    diagnostics::error_label(&span).with_message("invalid function call"),
                    diagnostics::note_label(symbol.def_span())
                        .with_message(format!("defined as {} here", symbol.kind_name())),
                ]);
            });
            return Expr::nothing(span);
        };

        if args.len() != param_names.len() {
            self.error(&span, |error| {
                error.set_message("wrong number of arguments given to function call");
                error.add_labels([
                    diagnostics::note_label(symbol.def_span()).with_message(format!(
                        "defined as taking {} arguments here...",
                        param_names.len(),
                    )),
                    diagnostics::error_label(&span)
                        .with_message(format!("...but given {} arguments here", args.len())),
                ]);
            });
            return Expr::nothing(span);
        }

        if lazy_eval.is_some() {
            // Lazy evaluation.

            // Assume that each argument is expanded once; for each arg, subtract 1 since it'll be replacing one op.
            let mut ops = Vec::with_capacity(
                expr.ops().len() + args.iter().fold(0, |sum, arg| arg.ops().len() - 1 + sum),
            );
            for op in expr.ops() {
                if let OpKind::Symbol(ident) = op.kind {
                    if let Some(arg_idx) = param_names.iter().position(|name| *name == ident) {
                        ops.extend(args[arg_idx].ops().cloned());
                        continue;
                    }
                }
                ops.push(op.clone());
            }
            ops.into_iter().collect()
        } else {
            // Eager evaluation.

            let mut arg_results = Vec::with_capacity(args.len());
            for arg in &args {
                match self.try_const_eval(arg) {
                    Ok((value, _span)) => arg_results.push(value),
                    Err(err) => self.report_expr_error(err),
                }
            }

            if arg_results.len() != args.len() {
                // At least one error has been produced, bail out.
                return Expr::nothing(span);
            }
            expr.ops()
                .map(|op| {
                    let mut new_op = op.clone();
                    if let OpKind::Symbol(ident) = op.kind {
                        if let Some(arg_idx) = param_names.iter().position(|name| *name == ident) {
                            new_op.kind = OpKind::Number(arg_results[arg_idx]);
                        }
                    }
                    new_op
                })
                .collect()
        }
    }

    pub fn is_expr_constant(&self, expr: Expr, l_span_idx: usize, r_span_idx: usize) -> Expr {
        let value = match self.try_const_eval(&expr) {
            Ok((_value, _span)) => 1,
            Err(err) => {
                if err.can_be_deferred_to_linker() {
                    0
                } else {
                    // Report the value as constant, but don't report the error itself:
                    // generally, if the function returns 1 (true), then its argument gets used again;
                    // reporting the error here would cause it to be reported twice.
                    // Returning 0 could cause the error to be deferred to the linker, which is not great UX.
                    1
                }
            }
        };

        Expr::number(value, self.span_from_to(l_span_idx, r_span_idx))
    }

    pub fn ident(&self, ident: Identifier, span_idx: usize) -> Expr {
        let span = self.line_spans[span_idx].clone();
        if let Some(res) = self
            .symbols
            .find(&ident)
            .and_then(|sym| sym.get_string(self.sections.active_section.as_ref(), self.identifiers))
        {
            match res {
                Ok(string) => self.str_to_num((string, span)),
                Err(err) => {
                    self.error(&span, |error| {
                        error.set_message(&err);
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message("referenced in a numeric expression here"),
                        )
                    });
                    Expr::nothing(span)
                }
            }
        } else {
            Expr::symbol(ident, span)
        }
    }

    pub fn local_ident(&self, ident: Option<Identifier>, span_idx: usize) -> Expr {
        let span = self.line_spans[span_idx].clone();
        if let Some(name) = ident {
            Expr::symbol(name, span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn anon_label_ref(&self, ident: Option<Identifier>, span_idx: usize) -> Expr {
        let span = self.line_spans[span_idx].clone();
        if let Some(name) = ident {
            Expr::symbol(name, span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn expr_def_of_symbol(
        &self,
        ident: Option<Identifier>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some(name) = ident {
            let value = match self.symbols.find(&name) {
                Some(sym)
                    if sym.exists(
                        self.sections.active_section.as_ref(),
                        self.macro_args.last(),
                    ) =>
                {
                    1
                }
                _ => 0,
            };

            Expr::number(value, span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn bank_of_ident(
        &self,
        (ident_span_idx, ident): (usize, Option<Identifier>),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some(name) = ident {
            if let Some(res) = self.symbols.find(&name).and_then(|sym| {
                sym.get_string(self.sections.active_section.as_ref(), self.identifiers)
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
                Expr::bank_of_symbol(name, span)
            }
        } else {
            Expr::nothing(span)
        }
    }
}
