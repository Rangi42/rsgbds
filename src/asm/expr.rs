use std::cell::Cell;

use crate::{
    diagnostics,
    macro_args::MacroArgs,
    section::Sections,
    sources::Span,
    symbols::{SymbolData, SymbolKind, Symbols},
    syntax::tokens::{tok, TokenPayload},
    Identifier, Identifiers, Options,
};

#[derive(Debug)]
pub struct Expr {
    /// This is never empty for a valid expression; however, syntax errors yield bogus [`Expr`]s
    /// that have this list be empty.
    // TODO: use `smallvec` or something, it will most often contain few elements
    payload: Vec<Op>,
}

#[derive(Debug)]
struct Op {
    span: Span,
    kind: OpKind,
}

#[derive(Debug, Clone, Copy)]
enum OpKind {
    Number(i32),
    Symbol(Identifier),
    Binary(BinOp),
    Unary(UnOp),
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
    SymDeleted(Span),
    // The boolean indicates whether the expression contains just the symbol,
    //   or if the error has bubbled up some.
    // This is used to resolve non-constant symbols in some constant ways,
    //   such as subtraction of labels, or ANDing of aligned labels.
    SymNotConst {
        name: Identifier,
        just_the_sym: bool,
    },
    DivBy0,
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
}

impl Expr {
    pub fn binary_op(mut self, operator: BinOp, mut other: Self, op_span: Span) -> Self {
        let (Some(lhs), Some(rhs)) = (self.payload.last(), other.payload.last()) else {
            self.payload.clear();
            return self;
        };

        let new_op = Op {
            kind: OpKind::Binary(operator),
            span: op_span,
        };
        self.payload.reserve(other.payload.len() + 1);
        self.payload.append(&mut other.payload);
        self.payload.push(new_op);

        self
    }

    pub fn unary_op(mut self, operator: UnOp, op_span: Span) -> Self {
        if let Some(last) = self.payload.last() {
            self.payload.push(Op {
                kind: OpKind::Unary(operator),
                span: op_span,
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
        debug_assert!(!self.payload.is_empty());
        let mut eval_stack = vec![];
        for op in &self.payload {
            match op.kind {
                OpKind::Number(value) => eval_stack.push(Ok((value, op.span.clone()))),
                OpKind::Symbol(name) => eval_stack.push(match symbols.find(&name) {
                    None => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymNotFound(name),
                    }),
                    Some(SymbolData::Deleted(span)) => Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::SymDeleted(span.clone()),
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
                }),
                OpKind::Binary(operator) => {
                    let rhs = eval_stack.pop().unwrap();
                    let lhs = eval_stack.pop().unwrap();
                    eval_stack.push(operator.const_eval(lhs, rhs, symbols, sections));
                }
                OpKind::Unary(operator) => {
                    let value = eval_stack.pop().unwrap();
                    eval_stack.push(operator.const_eval(value, &op.span));
                }
                OpKind::Nothing => {
                    eval_stack.push(Err(Error {
                        span: op.span.clone(),
                        kind: ErrKind::NoExpr,
                    }));
                }
            }
        }
        debug_assert_eq!(eval_stack.len(), 1);
        eval_stack.pop().unwrap()
    }

    pub fn first_span(&self) -> &Span {
        &self.payload[0].span
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
                        kind:
                            ErrKind::SymNotConst {
                                name: left_name,
                                just_the_sym: true,
                            },
                    }),
                    Err(Error {
                        span: right_span,
                        kind:
                            ErrKind::SymNotConst {
                                name: right_name,
                                just_the_sym: true,
                            },
                    }),
                ) => {
                    let left_sym = symbols.find(&left_name).unwrap();
                    let right_sym = symbols.find(&right_name).unwrap();
                    match (
                        left_sym.get_section_and_offset(sections),
                        right_sym.get_section_and_offset(sections),
                    ) {
                        (Some((left_sect, left_ofs)), Some((right_sect, right_ofs)))
                            if left_sect == right_sect =>
                        {
                            Ok((
                                right_ofs.wrapping_sub(left_ofs) as i32,
                                left_span.merged_with(&right_span),
                            ))
                        }
                        // The symbols aren't two labels belonging to the same section, bubble up the (left-hand) error.
                        (_, _) => Err(Error {
                            span: left_span,
                            kind: ErrKind::SymNotConst {
                                name: left_name,
                                just_the_sym: false,
                            },
                        }),
                    }
                }
                (Ok((lhs, left_span)), Ok((rhs, right_span))) => (Ok(lhs.wrapping_sub(rhs)))
                    .map(|value| (value, left_span.merged_with(&right_span))),
                (Ok(_), Err(err)) | (Err(err), _) => Err(err),
            },
            BinOp::And => greedy!(|lhs, rhs| Ok(lhs & rhs)),
            BinOp::Or => greedy!(|lhs, rhs| Ok(lhs | rhs)),
            BinOp::Xor => greedy!(|lhs, rhs| Ok(lhs ^ rhs)),
            BinOp::LeftShift => todo!(),
            BinOp::RightShift => todo!(),
            BinOp::UnsignedRightShift => todo!(),
            BinOp::Multiply => greedy!(|lhs, rhs| Ok((lhs as u32).wrapping_mul(rhs as u32) as i32)),
            BinOp::Divide => greedy!(|(lhs, left_span), (rhs, right_span)| if rhs == 0 {
                Err(Error {
                    span: right_span,
                    kind: ErrKind::DivBy0,
                })
            } else {
                Ok((lhs.wrapping_div(rhs), left_span.merged_with(&right_span)))
            }),
            BinOp::Modulo => todo!(),
            BinOp::Exponent => greedy!(|lhs, rhs| Ok(lhs.wrapping_pow(rhs as u32))),
        }
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
            Self::SymDeleted(span) => todo!(),
            // TODO: suggest that difference between symbols and/or alignment can be constant?
            Self::SymNotConst { name, .. } => (
                format!("`{}` is not constant", identifiers.resolve(*name).unwrap()),
                "this symbol's value is not known at this point".into(),
            ),
            Self::DivBy0 => ("division by zero".into(), "this is equal to zero".into()),
            Self::NoExpr => unreachable!(),
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
