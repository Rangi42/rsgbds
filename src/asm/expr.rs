use std::cell::Cell;

use compact_str::CompactString;
use either::Either;

use crate::{
    common::section::MemRegion,
    diagnostics::{self, warning, ReportBuilder},
    macro_args::MacroArgs,
    section::{SectionKind, Sections},
    sources::Span,
    symbols::{SymbolData, SymbolError, Symbols},
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
    SizeOfSect(CompactString),
    StartOfSect(CompactString),
    SizeOfRegion(MemRegion),
    StartOfRegion(MemRegion),
    Binary(BinOp),
    Unary(UnOp),
    Low,
    High,
    Bitwidth,
    Tzcount,
    Rst,
    Ldh,
    BitCheck(u8),
    /// Placeholder of sorts.
    Nothing,
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub kind: ErrKind,
}

#[derive(Debug)]
pub enum ExprWarning {
    ShiftByNeg(bool, i32),
    ShiftByTooMuch(bool, i32),
    LeftShiftNeg(i32),
    MinDivM1,
}

#[derive(Debug)]
pub enum ErrKind {
    SymNotFound(Identifier),
    SymDeleted(Identifier, Span),
    NonNumericSym(Identifier, &'static str, Span),
    SymError(SymbolError<'static, 'static>),
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
    SizeOfSectNotConst(CompactString),
    SymBankNotConst(Identifier),
    BankOfNonLabel(Identifier, &'static str),
    SectBankNotConst(CompactString),
    SizeOfRegion,
    StartOfRegion,
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

    pub fn size_of_section(name: CompactString, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::SizeOfSect(name),
        })
    }
    pub fn size_of_region(kind: MemRegion, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::SizeOfRegion(kind),
        })
    }

    pub fn start_of_section(name: CompactString, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::StartOfSect(name),
        })
    }
    pub fn start_of_region(kind: MemRegion, span: Span) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::StartOfRegion(kind),
        })
    }
}
impl FromIterator<Op> for Expr {
    fn from_iter<T: IntoIterator<Item = Op>>(iter: T) -> Self {
        Self {
            payload: iter.into_iter().collect(),
        }
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

    pub fn high(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::High,
            });
        }
        self
    }
    pub fn low(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::Low,
            });
        }
        self
    }
    pub fn bitwidth(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::Bitwidth,
            });
        }
        self
    }
    pub fn tzcount(mut self, span: Span) -> Self {
        if !self.payload.is_empty() {
            self.payload.push(Op {
                span,
                kind: OpKind::Tzcount,
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
    pub fn try_const_eval<F: FnMut(ExprWarning, &Span)>(
        &self,
        symbols: &Symbols,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
        mut warn: F,
    ) -> Result<(i32, Span), Error> {
        debug_assert_ne!(self.payload.len(), 0);

        let mut eval_stack = vec![];
        for op in &self.payload {
            let res = match &op.kind {
                &OpKind::Number(value) => Ok((value, op.span.clone())),
                &OpKind::Symbol(name) => match symbols.find(&name) {
                    None | Some(SymbolData::ExportPlaceholder(..)) => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymNotFound(name),
                    }),
                    Some(SymbolData::Deleted(span)) => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymDeleted(name, span.clone()),
                    }),
                    Some(sym) => match sym.get_number(macro_args, sections) {
                        Some(Ok(Some(value))) => Ok((value, op.span.clone())),
                        Some(Ok(None)) => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::SymNotConst {
                                name,
                                just_the_sym: true,
                            },
                        }),
                        Some(Err(err)) => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::SymError(err),
                        }),
                        None => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::NonNumericSym(
                                name,
                                sym.kind_name(),
                                sym.def_span().clone(),
                            ),
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
                        Some(Ok((sect_id, _ofs))) => match sections.sections[sect_id].bank() {
                            Some(value) => Ok((value as i32, op.span.clone())),
                            None => Err(Error {
                                span: op.span.clone(),
                                kind: ErrKind::SymBankNotConst(name),
                            }),
                        },
                        Some(Err(err)) => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::SymError(err),
                        }),
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
                // Sections can grow if they are still active (including in the section stack),
                // or if they are `union`/`fragment` typed (because the linker will merge pieces).
                OpKind::SizeOfSect(name) => {
                    match sections
                        .sections
                        .get_full(name)
                        .and_then(|(idx, _name, section)| {
                            (!sections.is_active_or_in_stack(idx)).then_some(section)
                        }) {
                        Some(section) if matches!(section.attrs.kind, SectionKind::Normal) => {
                            Ok((section.bytes.len() as i32, op.span.clone()))
                        }
                        _ => Err(Error {
                            span: op.span.clone(),
                            kind: ErrKind::SizeOfSectNotConst(name.clone()),
                        }),
                    }
                }
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
                // These two can vary depending on linker settings.
                OpKind::SizeOfRegion(..) => Err(Error {
                    span: op.span.clone(),
                    kind: ErrKind::SizeOfRegion,
                }),
                OpKind::StartOfRegion(..) => Err(Error {
                    span: op.span.clone(),
                    kind: ErrKind::StartOfRegion,
                }),
                OpKind::Binary(operator) => {
                    let rhs = eval_stack.pop().unwrap();
                    let lhs = eval_stack.pop().unwrap();
                    operator.const_eval(lhs, rhs, symbols, sections, &mut warn)
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
                OpKind::Bitwidth => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number, _span)) => {
                            Ok((32 - number.leading_zeros() as i32, op.span.clone()))
                        }
                        Err(err) => Err(err.bubble_up()),
                    }
                }
                OpKind::Tzcount => {
                    let value = eval_stack.pop().unwrap();
                    match value {
                        Ok((number, _span)) => {
                            Ok((number.trailing_zeros() as i32, op.span.clone()))
                        }
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
                        Ok((number @ 0..=7, span)) => Ok((number << 3 | i32::from(or_mask), span)),
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
    pub fn prep_for_patch<F: FnMut(ExprWarning, &Span)>(
        &self,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
        warn: F,
    ) -> Result<Either<(i32, Span), Self>, Error> {
        debug_assert_ne!(self.payload.len(), 0);

        match self.try_const_eval(symbols, macro_args, sections, warn) {
            Ok((value, span)) => Ok(Either::Left((value, span))),
            Err(Error { kind, .. }) if kind.can_be_deferred_to_linker() => {
                Ok(Either::Right(Self {
                    payload: self
                        .payload
                        .iter()
                        .map(|op| {
                            Ok(Op {
                                span: op.span.clone(),
                                kind: match &op.kind {
                                    // Eagerly evaluate symbols that can be.
                                    OpKind::Symbol(name) => match symbols.find(name) {
                                        None
                                        | Some(
                                            SymbolData::Deleted(..)
                                            | SymbolData::ExportPlaceholder(..),
                                        ) => OpKind::Symbol(*name),
                                        Some(sym) => match sym.get_number(macro_args, sections) {
                                            None => {
                                                return Err(Error {
                                                    span: op.span.clone(),
                                                    kind: ErrKind::NonNumericSym(
                                                        *name,
                                                        sym.kind_name(),
                                                        sym.def_span().clone(),
                                                    ),
                                                })
                                            }
                                            Some(Err(err)) => {
                                                return Err(Error {
                                                    span: op.span.clone(),
                                                    kind: ErrKind::SymError(err),
                                                })
                                            }
                                            Some(Ok(Some(value))) => OpKind::Number(value),
                                            Some(Ok(None)) => OpKind::Symbol(*name),
                                        },
                                    },
                                    kind => kind.clone(),
                                },
                            })
                        })
                        .collect::<Result<_, _>>()?,
                }))
            }
            Err(err) => Err(err),
        }
    }

    pub fn first_span(&self) -> Span {
        self.payload[0].span.clone()
    }

    pub fn ops(&self) -> impl ExactSizeIterator<Item = &Op> {
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
            | OpKind::SizeOfSect(..)
            | OpKind::StartOfRegion(..)
            | OpKind::SizeOfRegion(..)
            | OpKind::Binary(..)
            | OpKind::Unary(..)
            | OpKind::Low
            | OpKind::High
            | OpKind::Bitwidth
            | OpKind::Tzcount
            | OpKind::Rst
            | OpKind::Ldh
            | OpKind::BitCheck(..)
            | OpKind::Nothing => None,
        }
    }
}

// Sorted from lowest binding power (precedence), to highest.
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    LogicalOr,

    LogicalAnd,

    NotEqual,

    Equal,
    LessEq,
    Less,
    GreaterEq,
    Greater,

    Add,
    Subtract,

    And,
    Or,
    Xor,

    LeftShift,
    RightShift,
    UnsignedRightShift,

    Multiply,
    Divide,
    Modulo,

    Exponent,
}
// All unary operators are situated between `Modulo` and `Exponent`.
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Complement,
    Identity,
    Negation,
    Not,
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
    fn const_eval<F: FnMut(ExprWarning, &Span)>(
        &self,
        lhs: Result<(i32, Span), Error>,
        rhs: Result<(i32, Span), Error>,
        symbols: &Symbols,
        sections: &Sections,
        mut warn: F,
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
                (Ok((lhs, left_span)), Ok((rhs, right_span))) => {
                    Ok((lhs.wrapping_sub(rhs), left_span.merged_with(&right_span)))
                }
                (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err.bubble_up()),
            },
            BinOp::And => {
                fn try_compute_align_mask(
                    mask: i32,
                    err_kind: &ErrKind,
                    sections: &Sections,
                    symbols: &Symbols,
                ) -> Option<i32> {
                    err_kind
                        .get_section_and_offset(symbols, sections)
                        .and_then(|(sect_id, ofs)| {
                            let section = &sections.sections[sect_id];
                            let unknown_bits = !section.attrs.address.align_mask();
                            if mask & i32::from(unknown_bits) == 0 {
                                // Only known bits are kept: the result is constant!
                                Some(
                                    (ofs + usize::from(section.attrs.address.align_ofs())) as i32
                                        & mask,
                                )
                            } else {
                                // Not all bits are known to the assembler.
                                None
                            }
                        })
                }
                match (lhs, rhs) {
                    // ANDing with an aligned label can be constant.
                    (
                        Ok((mask, left_span)),
                        Err(Error {
                            span: right_span,
                            kind,
                        }),
                    ) => match try_compute_align_mask(mask, &kind, sections, symbols) {
                        Some(value) => Ok((value, left_span.merged_with(&right_span))),
                        None => Err(Error {
                            span: right_span,
                            kind,
                        }
                        .bubble_up()),
                    },
                    (
                        Err(Error {
                            span: left_span,
                            kind,
                        }),
                        Ok((mask, right_span)),
                    ) => match try_compute_align_mask(mask, &kind, sections, symbols) {
                        Some(value) => Ok((value, left_span.merged_with(&right_span))),
                        None => Err(Error {
                            span: left_span,
                            kind,
                        }
                        .bubble_up()),
                    },
                    (Ok((lhs, left_span)), Ok((rhs, right_span))) => {
                        Ok((lhs & rhs, left_span.merged_with(&right_span)))
                    }
                    (Err(err), Err(_)) => Err(err.bubble_up()),
                }
            }
            BinOp::Or => greedy!(|lhs, rhs| Ok(lhs | rhs)),
            BinOp::Xor => greedy!(|lhs, rhs| Ok(lhs ^ rhs)),
            BinOp::LeftShift => greedy!(|(lhs, left_span), (rhs, right_span)| {
                let span = left_span.merged_with(&right_span);
                if rhs < 0 {
                    warn(ExprWarning::ShiftByNeg(false, rhs), &span);
                }
                if rhs >= 32 {
                    warn(ExprWarning::ShiftByTooMuch(false, rhs), &span);
                }
                Ok((shift_left(lhs, rhs), span))
            }),
            BinOp::RightShift => greedy!(|(lhs, left_span), (rhs, right_span)| {
                let span = left_span.merged_with(&right_span);
                if rhs < 0 {
                    warn(ExprWarning::ShiftByNeg(true, rhs), &span);
                }
                if rhs >= 32 {
                    warn(ExprWarning::ShiftByTooMuch(true, rhs), &span);
                }
                if lhs < 0 {
                    warn(ExprWarning::LeftShiftNeg(lhs), &span);
                }
                Ok((shift_right(lhs, rhs), span))
            }),
            BinOp::UnsignedRightShift => greedy!(|(lhs, left_span), (rhs, right_span)| {
                let span = left_span.merged_with(&right_span);
                if rhs < 0 {
                    warn(ExprWarning::ShiftByNeg(true, rhs), &span);
                }
                if rhs >= 32 {
                    warn(ExprWarning::ShiftByTooMuch(true, rhs), &span);
                }
                Ok((shift_right_unsigned(lhs, rhs), span))
            }),
            BinOp::Multiply => greedy!(|lhs, rhs| Ok((lhs as u32).wrapping_mul(rhs as u32) as i32)),
            BinOp::Divide => greedy!(|(lhs, left_span), (rhs, right_span)| if rhs == 0 {
                Err(Error {
                    span: right_span,
                    kind: ErrKind::DivBy0,
                })
            } else {
                let span = left_span.merged_with(&right_span);
                if lhs == i32::MIN && rhs == -1 {
                    warn(ExprWarning::MinDivM1, &span);
                }
                Ok((div_floor(lhs, rhs), span))
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

// Adapted from the Rust standard library.
fn div_floor(lhs: i32, rhs: i32) -> i32 {
    let d = lhs.wrapping_div(rhs);
    let r = lhs.wrapping_rem(rhs);

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
    let remainder = lhs.wrapping_rem(rhs);
    // Adjust modulo to have the sign of the divisor, not the sign of the dividend.
    remainder + rhs * from_bool(remainder != 0 && (lhs < 0) != (rhs < 0))
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
            UnOp::Negation => map(|n| n.wrapping_neg()),
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
                self.kind.report(identifiers, error, &self.span);
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

    pub fn can_be_deferred_to_linker(&self) -> bool {
        self.kind.can_be_deferred_to_linker()
    }
}
impl ErrKind {
    fn can_be_deferred_to_linker(&self) -> bool {
        match self {
            Self::SymNotFound(..)
            | Self::SymDeleted(..)
            | Self::SymNotConst { .. }
            | Self::SectAddrNotConst { .. }
            | Self::SizeOfSectNotConst(..)
            | Self::SymBankNotConst(..)
            | Self::SectBankNotConst(..)
            | Self::SizeOfRegion
            | Self::StartOfRegion => true,
            Self::NonNumericSym(..)
            | Self::SymError(..)
            | Self::BankOfNonLabel(..)
            | Self::DivBy0
            | Self::RstRange(..)
            | Self::LdhRange(..)
            | Self::BitRange(..)
            | Self::NoExpr => false,
        }
    }
    fn report<'span>(
        &'span self,
        identifiers: &Identifiers,
        error: &mut ReportBuilder<'span>,
        err_span: &'span Span,
    ) {
        match self {
            Self::SymNotFound(ident) => {
                error.set_message(format!(
                    "no symbol called `{}`",
                    identifiers.resolve(*ident).unwrap(),
                ));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("no symbol by this name exists at this point"),
                );
            }
            Self::SymDeleted(ident, span) => {
                error.set_message(format!(
                    "the symbol `{}` was deleted",
                    identifiers.resolve(*ident).unwrap(),
                ));
                error.add_labels([
                    diagnostics::error_label(err_span)
                        .with_message("no symbol by this name exists at this point"),
                    diagnostics::note_label(span).with_message("it has been deleted here"),
                ]);
            }
            Self::NonNumericSym(ident, kind_name, span) => {
                error.set_message(format!(
                    "the symbol `{}` isn't numeric",
                    identifiers.resolve(*ident).unwrap(),
                ));
                error.add_labels([
                    diagnostics::error_label(err_span)
                        .with_message("referenced in a numeric expression here"),
                    diagnostics::note_label(span)
                        .with_message(format!("defined as {kind_name} here")),
                ]);
            }
            Self::SymError(err) => {
                error.set_message(format!("{err}"));
                error.add_label(
                    diagnostics::error_label(err_span).with_message("this symbol is invalid"),
                );
            }
            // TODO: suggest that difference between symbols and/or alignment can be constant?
            Self::SymNotConst { name, .. } => {
                error.set_message(format!(
                    "`{}` is not constant",
                    identifiers.resolve(*name).unwrap()
                ));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this symbol's value is not known at this point"),
                );
            }
            // TODO: likewise
            Self::SectAddrNotConst { name, .. } => {
                error.set_message(format!("\"{name}\"'s address is not constant"));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this section's address is not known at this point"),
                );
            }
            Self::SymBankNotConst(name) => {
                error.set_message(format!(
                    "the bank of `{}` is not constant",
                    identifiers.resolve(*name).unwrap(),
                ));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this symbol's bank is not known at this point"),
                );
            }
            ErrKind::BankOfNonLabel(name, kind_name) => {
                error.set_message(format!(
                    "requested the bank of `{}`, which is not a label",
                    identifiers.resolve(*name).unwrap(),
                ));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message(format!("this symbol is a {kind_name}")),
                );
            }
            Self::SectBankNotConst(name) => {
                error.set_message(format!("the bank of \"{name}\" is not constant"));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this section's bank is not known at this point"),
                );
            }
            // TODO: if the section is active, suggest `endsection`
            Self::SizeOfSectNotConst(name) => {
                error.set_message(format!("\"{name}\"'s size is not constant"));
                error.add_label(
                    diagnostics::error_label(err_span).with_message(
                        "rgbasm can only know the size of non-active normal sections",
                    ),
                );
            }
            Self::SizeOfRegion => {
                error.set_message("the size of memory regions are not constant");
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this can depend on linker settings"),
                );
            }
            Self::StartOfRegion => {
                error.set_message("the start of memory regions are not constant");
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this can depend on linker settings"),
                );
            }
            Self::DivBy0 => {
                error.set_message("division by zero");
                error.add_label(
                    diagnostics::error_label(err_span).with_message("this is equal to zero"),
                );
            }
            Self::RstRange(value) => {
                error.set_message(format!(
                    "destination address ${value:04X} is not valid for `rst`"
                ));
                error.add_label(
                    diagnostics::error_label(err_span).with_message(
                        "this must be one of $00, $08, $10, $18, $20, $28, $30, or $38",
                    ),
                );
            }
            ErrKind::LdhRange(value) => {
                error.set_message(format!("${value:04X} is not a valid address for `ldh`"));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this must be between $FF00 and $FFFF inclusive"),
                );
            }
            ErrKind::BitRange(value) => {
                error.set_message(format!("{value} is not a valid bit number"));
                error.add_label(
                    diagnostics::error_label(err_span)
                        .with_message("this must be between 0 and 7 inclusive"),
                );
            }
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
                .and_then(|sym| sym.get_section_and_offset(sections).transpose().unwrap()),
            Self::SectAddrNotConst {
                name,
                just_the_sect: true,
            } => sections.sections.get_index_of(name).map(|idx| (idx, 0)),
            _ => None,
        }
    }
}
impl ExprWarning {
    pub fn report(&self, span: &Span, nb_errors_left: &Cell<usize>, options: &Options) {
        match self {
            Self::ShiftByNeg(dir_right, shift_amount) => diagnostics::warn(
                warning!("shift-amount"),
                span,
                |warning| {
                    warning.set_message(if *dir_right {
                        "shifting right by negative amount"
                    } else {
                        "shifting left by negative amount"
                    });
                    warning.add_label(
                        diagnostics::warning_label(span)
                            .with_message(format!("shifting by {shift_amount}")),
                    );
                    warning.set_help(if *dir_right {
                        "consider using `<<` instead"
                    } else {
                        "consider using `>>` or `>>>` instead"
                    });
                },
                nb_errors_left,
                options,
            ),
            Self::ShiftByTooMuch(dir_right, shift_amount) => diagnostics::warn(
                warning!("shift-amount"),
                span,
                |warning| {
                    warning.set_message(if *dir_right {
                        "shifting right by more than 31"
                    } else {
                        "shifting left by more than 31"
                    });
                    warning.add_label(
                        diagnostics::warning_label(span)
                            .with_message(format!("shifting by {shift_amount}")),
                    );
                },
                nb_errors_left,
                options,
            ),
            Self::LeftShiftNeg(shiftee) => diagnostics::warn(
                warning!("shift"),
                span,
                |warning| {
                    warning.set_message("shifting right a negative number");
                    warning.add_label(
                        diagnostics::warning_label(span)
                            .with_message(format!("the left-hand side evaluates to {shiftee}")),
                    );
                    warning.set_help("to divide rounding towards 0, use `/`; to perform a logical shift, use `>>>`");
                },
                nb_errors_left,
                options,
            ),
            Self::MinDivM1 => diagnostics::warn(
                warning!("div"),
                span,
                |warning| {
                    warning.set_message("division of -2_147_483_648 by -1 yields itself");
                    warning.add_label(
                        diagnostics::warning_label(span)
                            .with_message("the sign of this quotient may be surprising"),
                    );
                },
                nb_errors_left,
                options,
            ),
        }
    }
}
