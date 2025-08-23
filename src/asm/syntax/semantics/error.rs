use crate::diagnostics;

use super::parse_ctx;

impl parse_ctx!() {
    pub fn report_diff_marker(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];
        self.error(span, |error| {
            error.set_message("syntax error: leftover diff marker");
            error.add_label(diagnostics::error_label(span).with_message("this is invalid"));
            error.set_help("consider applying patches using the `patch` command");
        });
    }

    pub fn report_stray(&mut self, span_idx: usize, msg: &str) {
        let span = &self.line_spans[span_idx];
        self.error(span, |error| {
            error.set_message(msg);
            error.add_label(
                diagnostics::error_label(span).with_message("no opening directive matches this"),
            );
        });
    }
}
