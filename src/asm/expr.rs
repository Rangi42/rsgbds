use std::cell::Cell;

use crate::{
    context_stack::Span,
    diagnostics,
    source_store::SourceStore,
    symbols::SymName,
    syntax::tokens::{tok, TokenPayload},
    Options,
};

#[derive(Debug)]
pub struct Expr<'ctx_stack> {
    /// This is never empty for a valid expression; however, syntax errors yield bogus [`Expr`]s
    /// that have this list be empty.
    // TODO: use `smallvec` or something, it will most often contain few elements
    payload: Vec<Op<'ctx_stack>>,
}

#[derive(Debug)]
struct Op<'ctx_stack> {
    span: Span<'ctx_stack>,
    kind: OpKind,
}

#[derive(Debug)]
enum OpKind {
    Number(i32),
    Symbol(SymName),
    Binary(BinOp),
    Unary(UnOp),
    /// Placeholder of sorts.
    Nothing,
}

#[derive(Debug)]
pub struct Error<'ctx_stack> {
    span: Span<'ctx_stack>,
    kind: ErrKind,
}

#[derive(Debug)]
enum ErrKind {
    SymNotFound,
    DivBy0,
    // TODO: not sure this makes sense
    NoExpr,
}

impl<'ctx_stack> Expr<'ctx_stack> {
    fn from_terminal(op: Op<'ctx_stack>) -> Self {
        Self { payload: vec![op] }
    }

    pub fn nothing(span: Span<'ctx_stack>) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Nothing,
        })
    }

    pub fn number(number: i32, span: Span<'ctx_stack>) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Number(number),
        })
    }

    pub fn symbol(name: SymName, span: Span<'ctx_stack>) -> Self {
        Self::from_terminal(Op {
            span,
            kind: OpKind::Symbol(name),
        })
    }
}

impl<'ctx_stack> Expr<'ctx_stack> {
    pub fn binary_op(mut self, operator: BinOp, mut other: Self) -> Self {
        let (Some(lhs), Some(rhs)) = (self.payload.last(), other.payload.last()) else {
            self.payload.clear();
            return self;
        };

        let new_op = Op {
            kind: OpKind::Binary(operator),
            span: lhs.span.merged_with(&rhs.span),
        };
        self.payload.reserve(other.payload.len() + 1);
        self.payload.append(&mut other.payload);
        self.payload.push(new_op);

        self
    }

    pub fn unary_op(mut self, operator: UnOp, op_span: Span<'ctx_stack>) -> Self {
        if let Some(last) = self.payload.last() {
            self.payload.push(Op {
                kind: OpKind::Unary(operator),
                span: op_span.merged_with(&last.span),
            });
        }
        self
    }
}

impl<'ctx_stack> Expr<'ctx_stack> {
    // TODO: there may be more than one reason!
    pub fn try_const_eval(&self) -> Result<i32, Error<'ctx_stack>> {
        debug_assert!(!self.payload.is_empty());
        let mut eval_stack = vec![];
        for op in &self.payload {
            match &op.kind {
                OpKind::Number(value) => eval_stack.push(Ok(*value)),
                OpKind::Symbol(name) => todo!(),
                OpKind::Binary(operator) => {
                    let rhs = eval_stack.pop().unwrap();
                    let lhs = eval_stack.pop().unwrap();
                    eval_stack.push(operator.const_eval(lhs, rhs));
                }
                OpKind::Unary(operator) => {
                    let value = eval_stack.pop().unwrap();
                    eval_stack.push(operator.const_eval(value));
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

    pub fn first_span(&self) -> &Span<'ctx_stack> {
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
    fn const_eval<'ctx_stack>(
        &self,
        lhs: Result<i32, Error<'ctx_stack>>,
        rhs: Result<i32, Error<'ctx_stack>>,
    ) -> Result<i32, Error<'ctx_stack>> {
        // Most operators are "greedy", and require both operands to be known in order to be
        // const-evaluable themselves.
        macro_rules! greedy {
            (|$left:ident, $right:ident| $res:expr) => {
                match (lhs, rhs) {
                    (Ok($left), Ok($right)) => $res,
                    (Ok(_), Err(err)) | (Err(err), _) => Err(err),
                }
            };
        };

        match self {
            // These two operators are "lazy", so errors on the right-hand side are ignored if possible.
            BinOp::LogicalOr => match (lhs, rhs) {
                (Err(err), _) | (Ok(0), Err(err)) => Err(err),
                (Ok(0), Ok(0)) => Ok(0),
                (Ok(_), _) => Ok(1), // Either operand must be Ok(non_zero).
            },
            BinOp::LogicalAnd => match (lhs, rhs) {
                (Ok(0), _) | (Ok(_), Ok(0)) => Ok(0),
                (Ok(_), Ok(_)) => Ok(1),
                (Ok(_), Err(err)) | (Err(err), _) => Err(err),
            },
            BinOp::NotEqual => greedy!(|lhs, rhs| Ok(from_bool(lhs != rhs))),
            BinOp::Equal => greedy!(|lhs, rhs| Ok(from_bool(lhs == rhs))),
            BinOp::LessEq => greedy!(|lhs, rhs| Ok(from_bool(lhs <= rhs))),
            BinOp::Less => greedy!(|lhs, rhs| Ok(from_bool(lhs < rhs))),
            BinOp::GreaterEq => greedy!(|lhs, rhs| Ok(from_bool(lhs >= rhs))),
            BinOp::Greater => greedy!(|lhs, rhs| Ok(from_bool(lhs > rhs))),
            BinOp::Add => greedy!(|lhs, rhs| Ok(lhs.wrapping_add(rhs))),
            BinOp::Subtract => greedy!(|lhs, rhs| Ok(lhs.wrapping_sub(rhs))),
            BinOp::And => greedy!(|lhs, rhs| Ok(lhs & rhs)),
            BinOp::Or => greedy!(|lhs, rhs| Ok(lhs | rhs)),
            BinOp::Xor => greedy!(|lhs, rhs| Ok(lhs ^ rhs)),
            BinOp::LeftShift => todo!(),
            BinOp::RightShift => todo!(),
            BinOp::UnsignedRightShift => todo!(),
            BinOp::Multiply => greedy!(|lhs, rhs| Ok((lhs as u32).wrapping_mul(rhs as u32) as i32)),
            BinOp::Divide => greedy!(|lhs, rhs| lhs.checked_div(rhs).ok_or_else(|| Error {
                span: todo!(),
                kind: ErrKind::DivBy0
            })),
            BinOp::Modulo => todo!(),
            BinOp::Exponent => greedy!(|lhs, rhs| Ok(lhs.wrapping_pow(rhs as u32))),
        }
    }
}

impl UnOp {
    fn const_eval<'ctx_stack>(
        &self,
        value: Result<i32, Error<'ctx_stack>>,
    ) -> Result<i32, Error<'ctx_stack>> {
        let map = |f: fn(i32) -> i32| value.map(f);
        match self {
            UnOp::Complement => map(|n| !n),
            UnOp::Identity => map(|n| n),
            UnOp::Negation => map(|n| -n),
            UnOp::Not => map(|n| if n == 0 { 1 } else { 0 }),
        }
    }
}

impl Error<'_> {
    pub fn report(&self, sources: &SourceStore, nb_errors_left: &Cell<usize>, options: &Options) {
        diagnostics::error(
            &self.span,
            |error| {
                error.set_message("TODO");
                error.add_label(diagnostics::error_label(self.span.resolve()).with_message("TODO"))
            },
            sources,
            nb_errors_left,
            options,
        )
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
        #[derive(Debug)]
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
        #[derive(Debug)]
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
