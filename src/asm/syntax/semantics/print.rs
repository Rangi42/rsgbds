use compact_str::CompactString;
use either::Either;

use crate::{expr::Expr, sources::Span};

use super::parse_ctx;

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
}
