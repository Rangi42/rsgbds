use std::cell::Cell;

use compact_str::CompactString;

use crate::{
    diagnostics,
    macro_args::MacroArgs,
    section::Sections,
    sources::Span,
    symbols::{SymbolData, Symbols},
    syntax::tokens::{tok, TokenPayload},
    Identifier, Identifiers, Options,
};

#[derive(Debug, Clone)]
pub struct Expr {
    /// This is never empty for a valid expression; however, syntax errors yield bogus [`Expr`]s
    /// that have this list be empty.
    // TODO: use `smallvec` or something, it will most often contain few elements
    payload: Vec<Op>,
}

#[derive(Debug, Clone)]
pub struct Op {
    span: Span,
    pub kind: OpKind,
}

#[derive(Debug, Clone)]
pub enum OpKind {
    Number(i32),
    Symbol(Identifier),
    BankOfSym(Identifier),
    BankOfSect(CompactString),
    StartOfSect(CompactString),
    Binary(BinOp),
    Unary(UnOp),
    Low,
    High,
    Rst,
    Ldh,
    BitCheck(u8),
    /// Placeholder of sorts.
    Nothing,
}

#[derive(Debug)]
pub struct Error {
    span: Span,
    kind: ErrKind,
}

#[derive(Debug)]
enum ErrKind {
    SymNotFound(Identifier),
    SymDeleted(Identifier, Span),
    // The boolean indicates whether the expression contains just the symbol,
    //   or if the error has bubbled up at least once.
    // This is used to resolve non-constant symbols in some constant ways,
    //   such as subtraction of labels, or ANDing of aligned labels.
    SymNotConst {
        name: Identifier,
        just_the_sym: bool,
    },
    SectAddrNotConst {
        name: CompactString,
        just_the_sect: bool,
    },
    SymBankNotConst(Identifier),
    BankOfNonLabel(Identifier, &'static str),
    SectBankNotConst(CompactString),
    DivBy0,
    RstRange(i32),
    LdhRange(i32),
    BitRange(i32),
    // This error kind is special, in that it's never reported.
    // It's required so that expressions with bad syntax can be "evaluated" gracefully,
    //   but since bad syntax already causes a syntax error to be reported, emitting an
    //   error for this would just create duplicates.
    NoExpr,
}

impl Expr {
    fn from_terminal(op: Op) -> Self {
        Self { payload: vec![op] }
    }

    pub fn nothing(span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Nothing,
        })
    }

    pub fn number(number: i32, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Number(number),
        })
    }

    pub fn symbol(name: Identifier, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Symbol(name),
        })
    }

    pub fn bank_of_symbol(name: Identifier, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::BankOfSym(name),
        })
    }

    pub fn bank_of_section(name: CompactString, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::BankOfSect(name),
        })
    }

    pub fn start_of_section(name: CompactString, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::StartOfSect(name),
        })
    }
}

impl Expr {
    pub fn binary_op(mut self, operator: BinOp, mut other: Self, op_span: Span) -> Self {
        if self.payload.is_empty() || other.payload.is_empty() {
            self.payload.clear();
        } else {
            let new_op = Op {
                kind: OpKind::Binary(operator),
                span: op_span,
            };
            self.payload.reserve(other.payload.len() + 1);
            self.payload.append(&mut other.payload);
            self.payload.push(new_op);
        }
        self
    }

    pub fn unary_op(mut self, operator: UnOp, op_span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                kind: OpKind::Unary(operator),
                span: op_span,
            });
        }
        self
    }

    pub fn ldh(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::Ldh,
            });
        }
        self
    }
    pub fn rst(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::Rst,
            });
        }
        self
    }

    pub fn bit_check(mut self, or_mask: u8, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::BitCheck(or_mask),
            });
        }
        self
    }
}

impl Expr {
    // TODO: there may be more than one reason!
    pub fn try_const_eval(
        &self,
        symbols: &Symbols,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Result<(i32, Span), Error> {
        debug_assert_ne!(self.payload.len(), 0);

        let mut eval_stack = vec![];
        for op in &self.payload {
            let res = match &op.kind {
                &OpKind::Number(value) => Ok((value, op.span.clone())),
                &OpKind::Symbol(name) => match symbols.find(&name) {
                    None => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymNotFound(name),
                    }),
                    Some(SymbolData::Deleted(span)) => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymDeleted(name, span.clone()),
                    }),
                    Some(sym) => match sym.get_number(macro_args, sections) {
                        Some(value) => Ok((value, op.span.clone())),
                        None => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::SymNotConst {
                                name,
                                just_the_sym: true,
                            },
                        }),
                    },
                },
                &OpKind::BankOfSym(name) => match symbols.find(&name) {
                    None => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymNotFound(name),
                    }),
                    Some(SymbolData::Deleted(span)) => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymDeleted(name, span.clone()),
                    }),
                    Some(sym) => match sym.get_section_and_offset(sections) {
                        Some((sect_id, _ofs)) => match sections.sections[sect_id].bank() {
                            Some(value) => Ok((value as i32, op.span.clone())),
                            None => Err(Error {
                                span: op.span.clone(),
                                kind: ErrKind::SymBankNotConst(name),
                            }),
                        },
                        None => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::BankOfNonLabel(name, sym.kind_name()),
                        }),
                    },
                },
                OpKind::BankOfSect(name) => match sections
                    .sections
                    .get(name)
                    .and_then(|section| section.bank())
                {
                    Some(value) => Ok((value as i32, op.span.clone())),
                    None => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SectBankNotConst(name.clone()),
                    }),
                },
                OpKind::StartOfSect(name) => match sections
                    .sections
                    .get(name)
                    .and_then(|section| section.address())
                {
                    Some(value) => Ok((value as i32, op.span.clone())),
                    None => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SectAddrNotConst {
                            name: name.clone(),
                            just_the_sect: true,
                        },
                    }),
                },
                OpKind::Binary(operator) => {
                    let rhs = eval_stack.pop().unwrap();
                    let lhs = eval_stack.pop().unwrap();
                    operator.const_eval(lhs, rhs, symbols, sections)
                }
                OpKind::Unary(operator) => {
                    let value = eval_stack.pop().unwrap();
                    operator.const_eval(value, &op.span)
                }
                OpKind::Low => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number, _span)) => Ok((number & 0xFF, op.span.clone())),
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                OpKind::High => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number, _span)) => Ok(((number >> 8) & 0xFF, op.span.clone())),
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                OpKind::Rst => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number, span)) => {
                            if number & !0x38 == 0 {
                                Ok((number | 0xC7, span))
                            } else {
                                Err(Error {
                                    span,
                                    kind: ErrKind::RstRange(number),
                                })
                            }
                        }
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                OpKind::Ldh => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number @ 0xFF00..=0xFFFF, span)) => Ok((number & 0xFF, span)),
                        Ok((number, span)) => Err(Error {
                            span,
                            kind: ErrKind::LdhRange(number),
                        }),
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                &OpKind::BitCheck(or_mask) => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number @ 0..=7, span)) => Ok((number | i32::from(or_mask), span)),
                        Ok((number, span)) => Err(Error {
                            span,
                            kind: ErrKind::BitRange(number),
                        }),
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                OpKind::Nothing => Err(Error {
                    span: op.span.clone(),
                    kind: ErrKind::NoExpr,
                }),
            };
            eval_stack.push(res);
        }

        debug_assert_eq!(eval_stack.len(), 1);
        eval_stack.pop().unwrap()
    }

    /// This function takes an expression, and reduces it to its non-constant components, so that it's ready to be serialised.
    /// This step is important, because it evaluates symbols that are constant at this point;
    /// if the expression was evaluated during object file generation, the following code would misbehave:
    ///
    /// ```rgbasm
    /// FOR I, 2
    ///     dw Label + I
    /// ENDR
    /// ```
    ///
    /// ...because this would emit the `Label + I` expression identically twice,
    /// yet the intent is that each capture the same value for `Label`, but different values for `I`.
    pub fn prep_for_patch(
        &self,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Result<(i32, Span), Result<Self, Error>> {
        debug_assert_ne!(self.payload.len(), 0);

        match self.try_const_eval(symbols, macro_args, sections) {
            Ok((value, span)) => Ok((value, span)),
            Err(Error {
                kind:
                    ErrKind::SymNotFound(..) | ErrKind::SymDeleted(..) | ErrKind::SymNotConst { .. },
                ..
            }) => {
                Err(Ok(Self {
                    payload: self
                        .payload
                        .iter()
                        .map(|op| Op {
                            span: op.span.clone(),
                            kind: match &op.kind {
                                // Eagerly evaluate symbols that can be.
                                OpKind::Symbol(name) => match symbols
                                    .find(name)
                                    .and_then(|sym| sym.get_number(macro_args, sections))
                                {
                                    Some(value) => OpKind::Number(value),
                                    // TODO: we might want to create a `Ref` here;
                                    // but we'd need to also ensure it doesn't get deleted, even after it gets replaced...
                                    None => OpKind::Symbol(*name),
                                },
                                kind => kind.clone(),
                            },
                        })
                        .collect(),
                }))
            }
            Err(err) => Err(Err(err)),
        }
    }

    pub fn first_span(&self) -> &Span {
        &self.payload[0].span
    }
    pub fn last_span(&self) -> &Span {
        &self.payload.last().unwrap().span
    }
    pub fn overall_span(&self) -> Span {
        match self.payload.as_slice() {
            // Attempting to merge a span with itself would fail.
            [op] => op.span.clone(),
            ops => ops[0].span.merged_with(&ops.last().unwrap().span),
        }
    }

    pub fn ops(&self) -> impl Iterator<Item = &Op> {
        self.payload.iter()
    }
}

impl Op {
    /// Returns whether the operation references a symbol; used for the object file emission.
    pub fn get_symbol(&self) -> Option<Identifier> {
        match self.kind {
            OpKind::Symbol(ident) | OpKind::BankOfSym(ident) => Some(ident),
            OpKind::Number(..)
            | OpKind::BankOfSect(..)
            | OpKind::StartOfSect(..)
            | OpKind::Binary(..)
            | OpKind::Unary(..)
            | OpKind::Low
            | OpKind::High
            | OpKind::Rst
            | OpKind::Ldh
            | OpKind::BitCheck(..)
            | OpKind::Nothing => None,
        }
    }
}

operators! { // Sorted from lowest binding power (precedence), to highest.
    BinOp[left_assoc] LogicalOr("||");
    BinOp[left_assoc] LogicalAnd("&&");
    BinOp[left_assoc] NotEqual("!="), Equal("=="), LessEq("<="), Less("<"), GreaterEq(">="), Greater(">");
    BinOp[left_assoc] Add("+"), Subtract("-");
    BinOp[left_assoc] And("&"), Or("|"), Xor("^");
    BinOp[left_assoc] LeftShift("<<"), RightShift(">>"), UnsignedRightShift(">>>");
    BinOp[left_assoc] Multiply("*"), Divide("/"), Modulo("%");
    UnOp Complement("~"), Identity("+"), Negation("-"), Not("!");
    BinOp[right_assoc] Exponent("**");
}

/// Convenience function to turn a Rust boolean to rgbasm's equivalent.
fn from_bool(b: bool) -> i32 {
    if b {
        1
    } else {
        0
    }
}

impl BinOp {
    fn const_eval(
        &self,
        lhs: Result<(i32, Span), Error>,
        rhs: Result<(i32, Span), Error>,
        symbols: &Symbols,
        sections: &Sections,
    ) -> Result<(i32, Span), Error> {
        // Most operators are "greedy", and require both operands to be known in order to be
        // const-evaluable themselves.
        macro_rules! greedy {
            (|$left:ident, $right:ident| $res:expr) => {
                match (lhs, rhs) {
                    (Ok(($left, left_span)), Ok(($right, right_span))) => {
                        $res.map(|value| (value, left_span.merged_with(&right_span)))
                    }
                    (Ok(_), Err(err)) | (Err(err), _) => Err(err.bubble_up()),
                }
            };
            (|($left:ident, $left_span:ident), ($right:ident, $right_span:ident)| $res:expr) => {
                match (lhs, rhs) {
                    (Ok(($left, $left_span)), Ok(($right, $right_span))) => $res,
                    (Ok(_), Err(err)) | (Err(err), _) => Err(err.bubble_up()),
                }
            };
        }

        match self {
            // These two operators are "lazy", so errors on the right-hand side are ignored if possible.
            BinOp::LogicalOr => match (lhs, rhs) {
                (Err(err), _) | (Ok((0, _)), Err(err)) => Err(err),
                (Ok((0, left_span)), Ok((0, right_span))) => {
                    Ok((0, left_span.merged_with(&right_span)))
                }
                // Either operand must be Ok(non_zero).
                (
                    Ok((_, left_span)),
                    Ok((_, right_span))
                    | Err(Error {
                        span: right_span, ..
                    }),
                ) => Ok((1, left_span.merged_with(&right_span))),
            },
            BinOp::LogicalAnd => match (lhs, rhs) {
                (
                    Ok((0, left_span)),
                    Ok((_, right_span))
                    | Err(Error {
                        span: right_span, ..
                    }),
                )
                | (Ok((_, left_span)), Ok((0, right_span))) => {
                    Ok((0, left_span.merged_with(&right_span)))
                }
                (Ok((_, left_span)), Ok((_, right_span))) => {
                    Ok((1, left_span.merged_with(&right_span)))
                }
                (Ok(_), Err(err)) | (Err(err), _) => Err(err),
            },
            BinOp::NotEqual => greedy!(|lhs, rhs| Ok(from_bool(lhs != rhs))),
            BinOp::Equal => greedy!(|lhs, rhs| Ok(from_bool(lhs == rhs))),
            BinOp::LessEq => greedy!(|lhs, rhs| Ok(from_bool(lhs <= rhs))),
            BinOp::Less => greedy!(|lhs, rhs| Ok(from_bool(lhs < rhs))),
            BinOp::GreaterEq => greedy!(|lhs, rhs| Ok(from_bool(lhs >= rhs))),
            BinOp::Greater => greedy!(|lhs, rhs| Ok(from_bool(lhs > rhs))),
            BinOp::Add => greedy!(|lhs, rhs| Ok(lhs.wrapping_add(rhs))),
            BinOp::Subtract => match (lhs, rhs) {
                // Subtracting two labels that belong to the same section is constant.
                (
                    Err(Error {
                        span: left_span,
                        kind: left_kind,
                    }),
                    Err(Error {
                        span: right_span,
                        kind: right_kind,
                    }),
                ) => {
                    match (
                        left_kind.get_section_and_offset(symbols, sections),
                        right_kind.get_section_and_offset(symbols, sections),
                    ) {
                        (Some((left_sect, left_ofs)), Some((right_sect, right_ofs)))
                            if left_sect == right_sect =>
                        {
                            Ok((
                                left_ofs.wrapping_sub(right_ofs) as i32,
                                left_span.merged_with(&right_span),
                            ))
                        }
                        // The symbols aren't two labels belonging to the same section, bubble up the (left-hand) error.
                        (_, _) => Err(Error {
                            span: left_span,
                            kind: left_kind,
                        }
                        .bubble_up()),
                    }
                }
                (Ok((lhs, left_span)), Ok((rhs, right_span))) => (Ok(lhs.wrapping_sub(rhs)))
                    .map(|value| (value, left_span.merged_with(&right_span))),
                (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err.bubble_up()),
            },
            // TODO: symbol alignment can be constant
            BinOp::And => greedy!(|lhs, rhs| Ok(lhs & rhs)),
            BinOp::Or => greedy!(|lhs, rhs| Ok(lhs | rhs)),
            BinOp::Xor => greedy!(|lhs, rhs| Ok(lhs ^ rhs)),
            // TODO: `-Wshift-amount`
            BinOp::LeftShift => greedy!(|lhs, rhs| Ok(shift_left(lhs, rhs))),
            BinOp::RightShift => greedy!(|lhs, rhs| Ok(shift_right(lhs, rhs))),
            BinOp::UnsignedRightShift => greedy!(|lhs, rhs| Ok(shift_right_unsigned(lhs, rhs))),
            BinOp::Multiply => greedy!(|lhs, rhs| Ok((lhs as u32).wrapping_mul(rhs as u32) as i32)),
            BinOp::Divide => greedy!(|(lhs, left_span), (rhs, right_span)| if rhs == 0 {
                Err(Error {
                    span: right_span,
                    kind: ErrKind::DivBy0,
                })
            } else {
                Ok((div_floor(lhs, rhs), left_span.merged_with(&right_span)))
            }),
            BinOp::Modulo => greedy!(|(lhs, left_span), (rhs, right_span)| if rhs == 0 {
                Err(Error {
                    span: right_span,
                    kind: ErrKind::DivBy0,
                })
            } else {
                Ok((modulo(lhs, rhs), left_span.merged_with(&right_span)))
            }),
            BinOp::Exponent => greedy!(|lhs, rhs| Ok(lhs.wrapping_pow(rhs as u32))),
        }
    }
}

// From the Rust standard library, currently unstable.
fn div_floor(lhs: i32, rhs: i32) -> i32 {
    let d = lhs / rhs;
    let r = lhs % rhs;

    // If the remainder is non-zero, we need to subtract one if the
    // signs of lhs and rhs differ, as this means we rounded upwards
    // instead of downwards. We do this branchlessly by creating a mask
    // which is all-ones iff the signs differ, and 0 otherwise. Then by
    // adding this mask (which corresponds to the signed value -1), we
    // get our correction.
    let correction = (lhs ^ rhs) >> (i32::BITS - 1);
    if r != 0 {
        d + correction
    } else {
        d
    }
}
fn modulo(lhs: i32, rhs: i32) -> i32 {
    let remainder = lhs / rhs;
    // Adjust module to have the sign of the divisor, not the sign of the dividend.
    remainder + rhs * from_bool((remainder < 0) != (rhs < 0))
}
fn shift_left(lhs: i32, rhs: i32) -> i32 {
    match rhs {
        0..=31 => lhs << rhs,
        32.. => 0,
        rhs => shift_right(lhs, -rhs),
    }
}
fn shift_right(lhs: i32, rhs: i32) -> i32 {
    match rhs {
        0..=31 => lhs >> rhs,
        32.. => {
            if lhs < 0 {
                -1
            } else {
                0
            }
        }
        rhs => shift_left(lhs, -rhs),
    }
}
fn shift_right_unsigned(lhs: i32, rhs: i32) -> i32 {
    match rhs {
        0..=31 => ((lhs as u32) >> rhs) as i32,
        32.. => 0,
        rhs => shift_left(lhs, -rhs),
    }
}

impl UnOp {
    fn const_eval(
        &self,
        value: Result<(i32, Span), Error>,
        operator_span: &Span,
    ) -> Result<(i32, Span), Error> {
        let map = |f: fn(i32) -> i32| match value {
            Ok((num, span)) => Ok((f(num), operator_span.merged_with(&span))),
            Err(err) => Err(err.bubble_up()),
        };
        match self {
            UnOp::Complement => map(|n| !n),
            UnOp::Identity => map(|n| n),
            UnOp::Negation => map(|n| -n),
            UnOp::Not => map(|n| if n == 0 { 1 } else { 0 }),
        }
    }
}

impl Error {
    pub fn report(
        &self,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        // Do not report these, as they stem from syntax errors, and thus are duplicates.
        if self.is_nothing() {
            return;
        }

        diagnostics::error(
            &self.span,
            |error| {
                let (message, label_message) = self.kind.messages(identifiers);
                error.set_message(message);
                error.add_label(diagnostics::error_label(&self.span).with_message(label_message))
            },
            nb_errors_left,
            options,
        )
    }

    pub fn is_nothing(&self) -> bool {
        matches!(self.kind, ErrKind::NoExpr)
    }

    fn bubble_up(self) -> Self {
        let kind = match self.kind {
            ErrKind::SymNotConst { name, .. } => ErrKind::SymNotConst {
                name,
                just_the_sym: false,
            },
            kind => kind,
        };
        Self {
            span: self.span,
            kind,
        }
    }
}
impl ErrKind {
    fn messages(&self, identifiers: &Identifiers) -> (String, String) {
        match self {
            Self::SymNotFound(ident) => (
                format!(
                    "no symbol called `{}`",
                    identifiers.resolve(*ident).unwrap(),
                ),
                "no symbol by this name exists at this point".into(),
            ),
            // TODO: add a label that highlights `span`
            Self::SymDeleted(ident, span) => (
                format!(
                    "the symbol `{}` was deleted",
                    identifiers.resolve(*ident).unwrap(),
                ),
                "no symbol by this name exists at this point".into(),
            ),
            // TODO: suggest that difference between symbols and/or alignment can be constant?
            Self::SymNotConst { name, .. } => (
                format!("`{}` is not constant", identifiers.resolve(*name).unwrap()),
                "this symbol's value is not known at this point".into(),
            ),
            // TODO: likewise
            Self::SectAddrNotConst { name, .. } => (
                format!("\"{name}\"'s address is not constant"),
                "this section's address is not known at this point".into(),
            ),
            Self::SymBankNotConst(name) => (
                format!(
                    "the bank of `{}` is not constant",
                    identifiers.resolve(*name).unwrap(),
                ),
                "this symbol's bank is not known at this point".into(),
            ),
            ErrKind::BankOfNonLabel(name, kind_name) => (
                format!(
                    "requested the bank of `{}`, which is not a label",
                    identifiers.resolve(*name).unwrap(),
                ),
                format!("this symbol is a {kind_name}"),
            ),
            Self::SectBankNotConst(name) => (
                format!("the bank of \"{name}\" is not constant",),
                "this section's bank is not known at this point".into(),
            ),
            Self::DivBy0 => ("division by zero".into(), "this is equal to zero".into()),
            Self::RstRange(value) => (
                format!("destination address ${value:04X} is not valid for `rst`"),
                "this must be one of $00, $08, $10, $18, $20, $28, $30, or $38".into(),
            ),
            ErrKind::LdhRange(value) => (
                format!("${value:04X} is not a valid address for `ldh`"),
                "this must be between $FF00 and $FFFF inclusive".into(),
            ),
            ErrKind::BitRange(value) => (
                format!("{value} is not a valid bit number"),
                "this must be between 0 and 7 inclusive".into(),
            ),
            Self::NoExpr => unreachable!(),
        }
    }

    fn get_section_and_offset(
        &self,
        symbols: &Symbols,
        sections: &Sections,
    ) -> Option<(usize, usize)> {
        match self {
            Self::SymNotConst {
                name,
                just_the_sym: true,
            } => symbols
                .symbols
                .get(name)
                .and_then(|sym| sym.get_section_and_offset(sections)),
            Self::SectAddrNotConst {
                name,
                just_the_sect: true,
            } => sections.sections.get_index_of(name).map(|idx| (idx, 0)),
            _ => None,
        }
    }
}

macro_rules! operators {
    ($(
        $kind:ident $([$assoc:ident])? $first_name:ident $first_tok:tt $(, $name:ident $tok:tt)*
    );* $(;)?) => {
        enum Precedence {$( $first_name, )*} // Only used to auto-gen incrementing values.
        binary_operators! {$( $kind $([$assoc])? $first_name $first_tok $(, $name $tok)* );*}
        unary_operators! {$( $kind $([$assoc])? $first_name $first_tok $(, $name $tok)* );*}
    };
}
macro_rules! binary_operators {
    ($(
        $(BinOp[$assoc:ident] $first_name:ident($first_token:tt) $(, $name:ident($token:tt))*)?
        $(UnOp $($un_name:ident $un_tok:tt),+)?
    );*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum BinOp {
            $($( $first_name, $( $name, )*)?)*
        }
        impl BinOp {
            pub fn from_token(token: &TokenPayload) -> Option<Self> {
                match token {
                    $($(
                        tok!($first_token) => Some(Self::$first_name),
                        $( tok!($token) => Some(Self::$name), )*
                    )?)*
                    _ => None,
                }
            }

            pub fn binding_power(&self) -> (u8,u8) {
                match self {$($(
                    Self::$first_name $( | Self::$name )* => $assoc!((Precedence::$first_name as u8) * 2 + 1),
                )?)*}
            }
        }
    };
}
macro_rules! unary_operators {
    ($(
        $(BinOp[$assoc:ident] $($bin_name:ident $bin_tok:tt),+)?
        $(UnOp $first_name:ident($first_token:tt) $(, $name:ident($token:tt))*)?
    );*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum UnOp {
            $($( $first_name, $( $name, )*)?)*
        }
        impl UnOp {
            pub fn from_token(token: &TokenPayload) -> Option<Self> {
                match token {
                    $($(
                        tok!($first_token) => Some(Self::$first_name),
                        $( tok!($token) => Some(Self::$name), )*
                    )?)*
                    _ => None,
                }
            }

            pub fn binding_power(&self) -> ((),u8) {
                match self {$($(
                    Self::$first_name $( | Self::$name )* => ((), (Precedence::$first_name as u8) * 2 + 1),
                )?)*}
            }
        }
    };
}

// A left-associative operator binds the operand to its right tighter than the operand to its left.
// This might seem backwards, but from the point of view of the operand, it means that the operand
// to its *left* binds tighter than the one to its *right*; so the one to the left has priority.
macro_rules! left_assoc {
    ($base:expr) => {
        (($base, $base + 1))
    };
}
macro_rules! right_assoc {
    ($base:expr) => {
        (($base + 1, $base))
    };
}

// This `use` allows using the macros before they are defined within the same file.
// (`macro_rules!` are a little janky at times.)
use {binary_operators, left_assoc, operators, right_assoc, unary_operators};
