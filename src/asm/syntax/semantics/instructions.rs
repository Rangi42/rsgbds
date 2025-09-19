use crate::{diagnostics, expr::Expr, instructions::Instruction};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn emit_instruction(&mut self, maybe_instr: Option<Instruction>) {
        if let Some(instr) = maybe_instr {
            self.sections.emit_instruction(
                instr,
                self.identifiers,
                self.symbols,
                self.macro_args.last(),
                self.nb_errors_left,
                self.options,
            );
        } else {
            // An error should have been reported by the parsing function, so don't report a new one.
            debug_assert_ne!(
                self.nb_errors_left.get(),
                self.options.max_errors,
                "Failed to parse instruction but did not generate an error!?",
            );
        }
    }

    pub fn check_expr_is_ff00(&mut self, expr: Expr) {
        match self.try_const_eval(&expr) {
            Ok((0xff00, _span)) => {} // Aight.
            Ok((value, span)) => self.error(&span, |error| {
                error.set_message("base address is not equal to $ff00");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message(format!("this evaluates to ${value:04x}")),
                );
            }),
            Err(err) => self.report_expr_error(err),
        }
    }
}
