use compact_str::CompactString;
use either::Either;

use crate::{
    diagnostics::{self, warning},
    expr::Expr,
    section::{AssertLevel, Assertion, LinkTimeExpr},
    sources::Span,
};

use super::{parse_ctx, ParseError};

impl parse_ctx!() {
    pub fn print(&self, value: Either<Expr, (CompactString, Span)>) {
        match value {
            Either::Left(expr) => match self.try_const_eval(&expr) {
                Ok((value, _span)) => print!("${value:02X}"),
                Err(err) => self.report_expr_error(err),
            },
            Either::Right((string, _span)) => print!("{string}"),
        }
    }

    pub fn do_fail(
        &mut self,
        (msg, _span): (CompactString, Span),
        span_idx: usize,
    ) -> Result<(), ParseError> {
        let span = &self.line_spans[span_idx];
        self.error(span, |error| {
            error.set_message(msg);
            error.add_label(
                diagnostics::error_label(span).with_message("assembly aborted due to this"),
            );
        });
        Err(ParseError::User { error: () })
    }

    pub fn do_warn(&mut self, (msg, _span): (CompactString, Span), span_idx: usize) {
        let span = &self.line_spans[span_idx];
        self.warn(warning!("user"), span, |warning| {
            warning.set_message(msg);
            warning.add_label(diagnostics::warning_label(span).with_message("triggered here"));
        });
    }

    pub fn assert(
        &mut self,
        (level, expr, msg): (AssertLevel, Expr, Option<(CompactString, Span)>),
        span_idx: usize,
    ) -> Result<(), ParseError> {
        match expr.prep_for_patch(self.symbols, self.macro_args.last(), self.sections) {
            Ok(Either::Left((0, span))) => assert_failure(level, msg.as_ref(), &span, self),
            Ok(Either::Left(_)) => Ok(()), // OK
            Err(error) => {
                self.report_expr_error(error);
                Ok(())
            }
            Ok(Either::Right(expr)) => {
                let span = self.nth_span(span_idx);
                self.sections.assertions.push(Assertion {
                    level,
                    rest: LinkTimeExpr {
                        span,
                        expr,
                        offset: 0, // TODO: unused, move it to the `Patch` instead
                        pc: self
                            .sections
                            .active_section
                            .as_ref()
                            .map(|active| (active.sym_section.id, active.sym_section.offset)),
                    },
                    message: match msg {
                        Some((string, _span)) => string,
                        None => "assertion failed".into(),
                    },
                });
                Ok(())
            }
        }
    }

    pub fn static_assert(
        &mut self,
        (level, expr, msg): (AssertLevel, Expr, Option<(CompactString, Span)>),
    ) -> Result<(), ParseError> {
        match self.try_const_eval(&expr) {
            Ok((0, span)) => assert_failure(level, msg.as_ref(), &span, self),
            Ok(_) => Ok(()), // OK
            Err(error) => {
                self.report_expr_error(error);
                Ok(())
            }
        }
    }
}

fn assert_failure(
    level: AssertLevel,
    msg: Option<&(CompactString, Span)>,
    span: &Span,
    parse_ctx: &mut parse_ctx!(),
) -> Result<(), ParseError> {
    match level {
        AssertLevel::Warn => {
            parse_ctx.error(span, |error| {
                error.set_message("assertion failed");
                error.add_label(diagnostics::error_label(span).with_message(match msg {
                    Some((text, _span)) => text,
                    None => "this expression evaluates to 0",
                }));
            });
            Ok(())
        }
        AssertLevel::Error | AssertLevel::Fatal => {
            parse_ctx.error(span, |error| {
                error.set_message("assertion failed");
                error.add_label(diagnostics::error_label(span).with_message(match msg {
                    Some((text, _span)) => text,
                    None => "this expression evaluates to 0",
                }));
            });
            matches!(level, AssertLevel::Error)
                .then_some(())
                .ok_or(ParseError::User { error: () })
        }
    }
}
