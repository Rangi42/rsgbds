use crate::{
    context_stack::Span,
    symbols::SymName,
    syntax::tokens::{tok, TokenPayload},
};

pub struct Expr<'ctx_stack> {
    /// Either a tree of operations, or an error that has occurred while evaluating the expression.
    /// Note that expressions are evaluated greedily when the result is known, and that operations
    /// performed on an "error value" yield that same error (like a floating-point NaN), so that
    /// the innermost error(s) only is (are) reported.
    // TODO: use `smallvec` or something, these will most often contain few elements
    payload: Result<Vec<Op<'ctx_stack>>, Vec<Error<'ctx_stack>>>,
}

struct Op<'ctx_stack> {
    span: Span<'ctx_stack>,
    kind: OpKind,
}

enum OpKind {
    Number(i32),
    Symbol(SymName),
}

struct Error<'ctx_stack> {
    span: Span<'ctx_stack>,
    kind: ErrKind,
}

enum ErrKind {
    DivBy0,
}

impl<'ctx_stack> Expr<'ctx_stack> {
    pub fn nothing() -> Self {
        Self {
            payload: Err(vec![]),
        }
    }

    fn from_terminal(op: Op<'ctx_stack>) -> Self {
        Self {
            payload: Ok(vec![op]),
        }
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

impl Expr<'_> {
    pub fn binary_op(self, operator: BinOp, rhs: Self) -> Self {
        match (self.payload, rhs.payload) {
            (Err(mut errors), Err(mut more_errors)) => {
                errors.append(&mut more_errors);
                Self {
                    payload: Err(errors),
                }
            }
            (Err(errors), Ok(_)) | (Ok(_), Err(errors)) => Self {
                payload: Err(errors),
            },
            (Ok(ops), Ok(more_ops)) => match (&ops[..], &more_ops[..]) {
                (
                    [Op {
                        kind: OpKind::Number(lhs),
                        span: left_span,
                    }],
                    [Op {
                        kind: OpKind::Number(rhs),
                        span: right_span,
                    }],
                ) => Self {
                    payload: Ok(vec![Op {
                        span: left_span.merged_with(right_span),
                        kind: OpKind::Number(operator.const_eval(*lhs, *rhs)),
                    }]),
                },
                _ => todo!(),
            },
        }
    }

    pub fn unary_op(mut self, operator: UnOp) -> Self {
        if let Ok(ops) = &mut self.payload {
            todo!();
        }
        self
    }
}

impl Expr<'_> {
    // TODO: maybe return a reason why instead?
    pub fn try_const_eval(&self) -> Option<i32> {
        match &self.payload {
            Ok(ops) => match ops[..] {
                [Op {
                    kind: OpKind::Number(num),
                    ..
                }] => Some(num),
                _ => None,
            },
            _ => None,
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

impl BinOp {
    fn const_eval(&self, lhs: i32, rhs: i32) -> i32 {
        match self {
            BinOp::LogicalOr => todo!(),
            BinOp::LogicalAnd => todo!(),
            BinOp::NotEqual => todo!(),
            BinOp::Equal => todo!(),
            BinOp::LessEq => todo!(),
            BinOp::Less => todo!(),
            BinOp::GreaterEq => todo!(),
            BinOp::Greater => todo!(),
            BinOp::Add => lhs.wrapping_add(rhs),
            BinOp::Subtract => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::Xor => todo!(),
            BinOp::LeftShift => todo!(),
            BinOp::RightShift => todo!(),
            BinOp::UnsignedRightShift => todo!(),
            BinOp::Multiply => (lhs as u32).wrapping_mul(rhs as u32) as i32,
            BinOp::Divide => todo!(),
            BinOp::Modulo => todo!(),
            BinOp::Exponent => todo!(),
        }
    }
}

macro_rules! operators {
    ($(
        $kind:ident $([$assoc:ident])? $first_name:ident $first_tok:tt $(, $name:ident $tok:tt)*
    );* $(;)?) => {
        enum Precedence {$( $first_name, )*}
        binary_operators! {$( $kind $([$assoc])? $first_name $first_tok $(, $name $tok)* );*}
        unary_operators! {$( $kind $([$assoc])? $first_name $first_tok $(, $name $tok)* );*}
    };
}
macro_rules! binary_operators {
    ($(
        $(BinOp[$assoc:ident] $first_name:ident($first_token:tt) $(, $name:ident($token:tt))*)?
        $(UnOp $($un_name:ident $un_tok:tt),+)?
    );*) => {
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
