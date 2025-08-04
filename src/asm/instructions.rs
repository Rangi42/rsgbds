use arrayvec::ArrayVec;

use crate::{expr::Expr, sources::Span};

#[derive(Debug)]
pub struct Instruction {
    span: Span,
    bytes: ArrayVec<u8, 3>,
    patch: Option<Patch>,
}

#[derive(Debug)]
struct Patch {
    size: u8,
    offset: u8,
    expr: Expr,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
    Bc,
    De,
    Hl,
    Af,
    Sp,
    Hli,
    Hld,
}

#[derive(Debug, Clone, Copy)]
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
impl Reg16 {
    fn id(&self) -> u8 {
        match self {
            Reg16::Bc => 0,
            Reg16::De => 1,
            Reg16::Hl => 2,
            Reg16::Af => 3, // For `push` / `pop`.
            Reg16::Sp => 3, // For `inc`, `dec`, and `add`.
            // For `ld a, []`.
            Reg16::Hli => 2,
            Reg16::Hld => 3,
        }
    }
}
impl Condition {
    fn id(&self) -> u8 {
        *self as u8
    }
}

impl Instruction {
    pub fn add_r16(rhs: Reg16, span: Span) -> Option<Self> {
        match rhs {
            Reg16::Bc => Some(0x09),
            Reg16::De => Some(0x19),
            Reg16::Hl => Some(0x29),
            Reg16::Sp => Some(0x39),
            Reg16::Af | Reg16::Hli | Reg16::Hld => None,
        }
        .map(|byte| Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        })
    }
    pub fn add(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x80 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn add_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xC6, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn adc(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x88 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn adc_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xCE, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn sub(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x90 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn sub_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xD6, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn sbc(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0x98 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn sbc_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xDE, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn and(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xA0 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn and_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xE6, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn xor(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xA8 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn xor_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xEE, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn or(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xB0 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn or_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xF6, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
        }
    }

    pub fn cp(rhs: Reg8, span: Span) -> Self {
        Self {
            span,
            bytes: [0xB8 | rhs.id()].into_iter().collect(),
            patch: None,
        }
    }
    pub fn cp_imm(rhs: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0xFE, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: rhs,
            }),
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
    pub fn dec_r16(rhs: Reg16, span: Span) -> Option<Self> {
        match rhs {
            Reg16::Bc => Some(0x0B),
            Reg16::De => Some(0x1B),
            Reg16::Hl => Some(0x2B),
            Reg16::Sp => Some(0x3B),
            _ => None,
        }
        .map(|byte| Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        })
    }
    pub fn inc_r16(rhs: Reg16, span: Span) -> Option<Self> {
        match rhs {
            Reg16::Bc => Some(0x0B),
            Reg16::De => Some(0x1B),
            Reg16::Hl => Some(0x2B),
            Reg16::Sp => Some(0x3B),
            _ => None,
        }
        .map(|byte| Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        })
    }

    pub fn call(condition: Option<Condition>, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0xC4 | cond.id() << 3,
            None => 0xCD,
        };
        Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        }
    }
    pub fn jp(condition: Option<Condition>, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0xC2 | cond.id() << 3,
            None => 0xC3,
        };
        Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
        }
    }
    pub fn jr(condition: Option<Condition>, span: Span) -> Self {
        let byte = match condition {
            Some(cond) => 0x20 | cond.id() << 3,
            None => 0x18,
        };
        Self {
            span,
            bytes: [byte].into_iter().collect(),
            patch: None,
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
    pub fn rst(target: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 0,
                expr: target,
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

    pub fn stop(byte: Expr, span: Span) -> Self {
        Self {
            span,
            bytes: [0x10, Default::default()].into_iter().collect(),
            patch: Some(Patch {
                size: 1,
                offset: 1,
                expr: byte,
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
