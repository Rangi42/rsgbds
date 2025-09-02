use std::cell::Cell;

use arrayvec::ArrayVec;
use either::Either;

use crate::{diagnostics, expr::Expr, section::PatchKind, sources::Span, Options};

#[derive(Debug)]
pub struct Instruction {
    pub span: Span,
    pub bytes: ArrayVec<u8, 3>,
    pub patch: Option<Patch>,
}

#[derive(Debug)]
pub struct Patch {
    pub kind: PatchKind,
    pub offset: u8,
    pub expr: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg8 {
    B,
    C,
    D,
    E,
    H,
    L,
    HlInd,
    A,
}

// Note that order matters here: these enums get converted to their `u8` repr for encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg16Arith {
    Bc,
    De,
    Hl,
    Sp,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg16Stack {
    Bc,
    De,
    Hl,
    Af,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg16Mem {
    Bc,
    De,
    Hli,
    Hld,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Condition {
    Nz,
    Z,
    Nc,
    C,
}

impl Reg8 {
    fn id(&self) -> u8 {
        *self as u8
    }
}
impl Condition {
    fn id(&self) -> u8 {
        *self as u8
    }
    pub fn negate(self) -> Self {
        match self {
            Self::Nz => Self::Z,
            Self::Z => Self::Nz,
            Self::Nc => Self::C,
            Self::C => Self::Nc,
        }
    }
}

impl Instruction {
    pub fn ld_r8_r8(
        dest: Reg8,
        src: Reg8,
        span: Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Option<Self> {
        if src == Reg8::HlInd && dest == Reg8::HlInd {
            diagnostics::error(
                &span,
                |error| {
                    error.set_message("`ld [hl], [hl]` doesn't exist");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("this instruction is invalid"),
                    );
                    error.set_note("trivia: its would-be encoding is occupied by `halt` instead");
                },
                nb_errors_left,
                options,
            );
            None
        } else {
            Some(Self {
                span,
                bytes: [0x40 | dest.id() << 3 | src.id()].into_iter().collect(),
                patch: None,
            })
        }
    }
    pub fn ld_r8_imm(dest: Reg8, src: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0x08 | dest.id() << 3, Default::default()]
                .into_iter()
                .collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: src,
            }),
        }
    }
    pub fn ld_r16_imm(reg: Reg16Arith, value: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [
                0x01 | (reg as u8) << 4,
                Default::default(),
                Default::default(),
            ]
            .into_iter()
            .collect(),
            patch: Some(Patch {
                kind: PatchKind::Word,
                offset: 1,
                expr: value,
            }),
        }
    }
    pub fn ld_a_r16_ind(is_write: bool, reg: Reg16Mem, span: Span) -> Self {
        Self {
            span,
            bytes: [if is_write { 0x02 } else { 0x0A } | (reg as u8) << 4]
                .into_iter()
                .collect(),
            patch: None,
        }
    }
    pub fn ld_a_addr(is_write: bool, address: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [
                if is_write { 0xEA } else { 0xFA },
                Default::default(),
                Default::default(),
            ]
            .into_iter()
            .collect(),
            patch: Some(Patch {
                kind: PatchKind::Word,
                offset: 1,
                expr: address,
            }),
        }
    }
    pub fn ld_sp_hl(span: Span) -> Self {
        Self {
            span,
            bytes: [0xF9].into_iter().collect(),
            patch: None,
        }
    }
    pub fn ld_hl_sp_ofs(offset: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xF8, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: offset,
            }),
        }
    }
    pub fn ld_addr_sp(address: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0x08, Default::default(), Default::default()]
                .into_iter()
                .collect(),
            patch: Some(Patch {
                kind: PatchKind::Word,
                offset: 1,
                expr: address,
            }),
        }
    }

    pub fn ldh_c(is_write: bool, span: Span) -> Self {
        Self {
            span,
            bytes: [if is_write { 0xE2 } else { 0xF2 }].into_iter().collect(),
            patch: None,
        }
    }
    pub fn ldh(is_write: bool, address: Expr, instr_span: Span, span: Span) -> Self {
        Self {
            span,
            bytes: [if is_write { 0xE0 } else { 0xF0 }, Default::default()]
                .into_iter()
                .collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: address.ldh(instr_span),
            }),
        }
    }

    pub fn add_hl_r16(rhs: Reg16Arith, span: Span) -> Self {
        Self {
            span,
            bytes: [(rhs as u8) << 4 | 0x09].into_iter().collect(),
            patch: None,
        }
    }
    pub fn add_sp_imm(ofs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xE8, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: ofs,
            }),
        }
    }
    pub fn add(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0x80 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xC6, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn adc(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0x88 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xCE, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn sub(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0x90 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xD6, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn sbc(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0x98 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xDE, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn and(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0xA0 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xE6, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn xor(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0xA8 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xEE, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn or(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0xB0 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xF6, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn cp(rhs: Either<Reg8, Expr>, span: Span) -> Self {
        match rhs {
            Either::Left(reg) => Self {
                span,
                bytes: [0xB8 | reg.id()].into_iter().collect(),
                patch: None,
            },
            Either::Right(expr) => Self {
                span,
                bytes: [0xFE, Default::default()].into_iter().collect(),
                patch: Some(Patch {
                    kind: PatchKind::Byte,
                    offset: 1,
                    expr,
                }),
            },
        }
    }

    pub fn dec_r8(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x05 | rhs.id() << 3].into_iter().collect(),
            patch: None,
        }
    }
    pub fn inc_r8(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x04 | rhs.id() << 3].into_iter().collect(),
            patch: None,
        }
    }
    pub fn dec_r16(rhs: Reg16Arith, span: Span) -> Self {
        Self {
            span,
            bytes: [(rhs as u8) << 4 | 0x03].into_iter().collect(),
            patch: None,
        }
    }
    pub fn inc_r16(rhs: Reg16Arith, span: Span) -> Self {
        Self {
            span,
            bytes: [(rhs as u8) << 4 | 0x0B].into_iter().collect(),
            patch: None,
        }
    }

    pub fn bit(bit: Expr, reg: Reg8, span: Span, instr_span: Span) -> Self {
        let second_byte = bit.bit_check(0x40 | reg.id(), instr_span);
        Self {
            span,
            bytes: [0xCB, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: second_byte,
            }),
        }
    }
    pub fn res(bit: Expr, reg: Reg8, span: Span, instr_span: Span) -> Self {
        let second_byte = bit.bit_check(0x80 | reg.id(), instr_span);
        Self {
            span,
            bytes: [0xCB, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: second_byte,
            }),
        }
    }
    pub fn set(bit: Expr, reg: Reg8, span: Span, instr_span: Span) -> Self {
        let second_byte = bit.bit_check(0xC0 | reg.id(), instr_span);
        Self {
            span,
            bytes: [0xCB, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr: second_byte,
            }),
        }
    }

    pub fn rlc(reg: Reg8, span: Span) -> Self {
        #[allow(clippy::identity_op)] // For symmetry with the other cases.
        Self {
            span,
            bytes: [0xCB, 0x00 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rl(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x10 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rrc(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x08 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rr(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x18 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn sla(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x20 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn sra(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x28 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn srl(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x38 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn swap(reg: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCB, 0x30 | reg.id()].into_iter().collect(),
            patch: None,
        }
    }

    pub fn pop(reg: Reg16Stack, span: Span) -> Self {
        Self {
            span,
            bytes: [0xC1 | (reg as u8) << 4].into_iter().collect(),
            patch: None,
        }
    }
    pub fn push(reg: Reg16Stack, span: Span) -> Self {
        Self {
            span,
            bytes: [0xC5 | (reg as u8) << 4].into_iter().collect(),
            patch: None,
        }
    }

    pub fn call(condition: Option<Condition>, target: Expr, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0xC4 | cond.id() << 3,
            None => 0xCD,
        };
        Self {
            span,
            bytes: [byte, Default::default(), Default::default()]
                .into_iter()
                .collect(),
            patch: Some(Patch {
                kind: PatchKind::Word,
                offset: 1,
                expr: target,
            }),
        }
    }
    pub fn jp(condition: Option<Condition>, target: Expr, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0xC2 | cond.id() << 3,
            None => 0xC3,
        };
        Self {
            span,
            bytes: [byte, Default::default(), Default::default()]
                .into_iter()
                .collect(),
            patch: Some(Patch {
                kind: PatchKind::Word,
                offset: 1,
                expr: target,
            }),
        }
    }
    pub fn jr(condition: Option<Condition>, target: Expr, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0x20 | cond.id() << 3,
            None => 0x18,
        };
        Self {
            span,
            bytes: [byte, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Jr,
                offset: 1,
                expr: target,
            }),
        }
    }
    pub fn ret(condition: Option<Condition>, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0xC0 | cond.id() << 3,
            None => 0xC9,
        };
        Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rst(target: Expr, instr_span: Span, span: Span) -> Self {
        Self {
            span,
            bytes: [Default::default()].into_iter().collect(),
            patch: Some(Patch {
                kind: PatchKind::Byte,
                offset: 0,
                expr: target.rst(instr_span),
            }),
        }
    }
    pub fn jp_hl(span: Span) -> Self {
        Self {
            span,
            bytes: [0xE9].into_iter().collect(),
            patch: None,
        }
    }

    pub fn stop(byte: Option<Expr>, span: Span) -> Self {
        Self {
            span,
            bytes: [0x10, Default::default()].into_iter().collect(),
            patch: byte.map(|expr| Patch {
                kind: PatchKind::Byte,
                offset: 1,
                expr,
            }),
        }
    }

    pub fn cpl(span: Span) -> Self {
        Self {
            span,
            bytes: [0x2F].into_iter().collect(),
            patch: None,
        }
    }

    pub fn ccf(span: Span) -> Self {
        Self {
            span,
            bytes: [0x3F].into_iter().collect(),
            patch: None,
        }
    }
    pub fn daa(span: Span) -> Self {
        Self {
            span,
            bytes: [0x27].into_iter().collect(),
            patch: None,
        }
    }
    pub fn di(span: Span) -> Self {
        Self {
            span,
            bytes: [0xF3].into_iter().collect(),
            patch: None,
        }
    }
    pub fn ei(span: Span) -> Self {
        Self {
            span,
            bytes: [0xFB].into_iter().collect(),
            patch: None,
        }
    }
    pub fn halt(span: Span) -> Self {
        Self {
            span,
            bytes: [0x76].into_iter().collect(),
            patch: None,
        }
    }
    pub fn nop(span: Span) -> Self {
        Self {
            span,
            bytes: [0x00].into_iter().collect(),
            patch: None,
        }
    }
    pub fn reti(span: Span) -> Self {
        Self {
            span,
            bytes: [0xD9].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rla(span: Span) -> Self {
        Self {
            span,
            bytes: [0x17].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rlca(span: Span) -> Self {
        Self {
            span,
            bytes: [0x07].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rra(span: Span) -> Self {
        Self {
            span,
            bytes: [0x1F].into_iter().collect(),
            patch: None,
        }
    }
    pub fn rrca(span: Span) -> Self {
        Self {
            span,
            bytes: [0x0F].into_iter().collect(),
            patch: None,
        }
    }
    pub fn scf(span: Span) -> Self {
        Self {
            span,
            bytes: [0x37].into_iter().collect(),
            patch: None,
        }
    }
}
