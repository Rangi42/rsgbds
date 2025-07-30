use std::{fmt::Display, iter::Peekable, str::Chars};

use compact_str::CompactString;

#[derive(Debug)]
pub struct FormatSpec {
    force_sign: Option<char>,
    be_exact: bool,
    align_left: bool,
    pad_with_zeros: bool,
    width: usize,
    frac: Option<usize>,
    precision: usize,
    pub kind: FormatKind,
}
#[derive(Debug, Clone, Copy, displaydoc::Display)]
pub enum FormatKind {
    /// default
    Default,
    /// signed number
    Signed,
    /// unsigned number
    Unsigned,
    /// lower-hex number
    LowerHex,
    /// upper-hex number
    UpperHex,
    /// binary number
    Binary,
    /// octal number
    Octal,
    /// fixed-point number
    FixedPoint,
    /// string
    String,
}
impl Default for FormatSpec {
    fn default() -> Self {
        Self {
            force_sign: None,
            be_exact: true,
            align_left: false,
            pad_with_zeros: false,
            width: 0,
            frac: None,
            precision: 0,
            kind: FormatKind::Default,
        }
    }
}

#[derive(Debug, displaydoc::Display)]
pub enum FormatError {
    /// Unexpected character '{unexpected}' {for_what}
    UnexpectedChar {
        unexpected: char,
        for_what: &'static str,
    },
    /// Missing character {for_what}
    MissingChar { for_what: &'static str },
    /// a {sym_kind} symbol cannot be formatted as {fmt_kind}
    BadKind {
        sym_kind: &'static str,
        fmt_kind: FormatKind,
    },
}

impl FormatSpec {
    pub fn parse(src: &str, default_precision: usize) -> Result<Self, FormatError> {
        let mut chars = src.chars().peekable();

        let force_sign = chars.next_if(|ch| matches!(ch, '+' | ' '));
        let be_exact = chars.next_if_eq(&'#').is_some();
        let align_left = chars.next_if_eq(&'-').is_some();
        let pad_with_zeros = chars.next_if_eq(&'0').is_some();
        let width = chars
            .peek()
            .and_then(|ch| ch.to_digit(10))
            .map_or(0, |first_digit| {
                parse_decimal(&mut chars, first_digit as usize)
            });
        let frac = chars.next_if_eq(&'.').map(|_| todo!());
        let precision = chars
            .next_if_eq(&'q')
            .map(|_| todo!())
            .unwrap_or(default_precision);
        let kind = match chars.next() {
            Some('d') => Ok(FormatKind::Signed),
            Some('u') => Ok(FormatKind::Unsigned),
            Some('x') => Ok(FormatKind::LowerHex),
            Some('X') => Ok(FormatKind::UpperHex),
            Some('b') => Ok(FormatKind::Binary),
            Some('o') => Ok(FormatKind::Octal),
            Some('f') => Ok(FormatKind::FixedPoint),
            Some('s') => Ok(FormatKind::String),

            Some(unexpected) => Err(FormatError::UnexpectedChar {
                unexpected,
                for_what: "as a print type",
            }),
            None => Err(FormatError::MissingChar {
                for_what: "print type",
            }),
        }?;
        if let Some(unexpected) = chars.next() {
            return Err(FormatError::UnexpectedChar {
                unexpected,
                for_what: "after the print type",
            });
        }

        fn parse_decimal(chars: &mut Peekable<Chars>, first_digit: usize) -> usize {
            let mut width = first_digit;
            loop {
                chars.next();
                let Some(digit) = chars.next().and_then(|ch| ch.to_digit(10)) else {
                    break width;
                };
                width = width * 10 + digit as usize;
            }
        }

        Ok(Self {
            force_sign,
            be_exact,
            align_left,
            pad_with_zeros,
            width,
            frac,
            precision,
            kind,
        })
    }
}

impl FormatSpec {
    pub fn write_number(
        &self,
        number: u32,
        buf: &mut CompactString,
        sym_kind: &'static str,
    ) -> Result<(), FormatError> {
        use std::fmt::Write;

        // TODO: format flags
        match self.kind {
            FormatKind::Signed => Ok(write!(buf, "{}", number as i32).unwrap()),
            FormatKind::Unsigned => Ok(write!(buf, "{number}").unwrap()),
            FormatKind::LowerHex => Ok(write!(buf, "{number:x}").unwrap()),
            FormatKind::UpperHex | FormatKind::Default => Ok(write!(buf, "{number:X}").unwrap()),
            FormatKind::Binary => Ok(write!(buf, "{number:b}").unwrap()),
            FormatKind::Octal => Ok(write!(buf, "{}", todo!()).unwrap()),
            FormatKind::FixedPoint => Ok(write!(buf, "{}", todo!()).unwrap()),
            fmt_kind => Err(FormatError::BadKind { sym_kind, fmt_kind }),
        }
    }
}
struct NumberFormatter {}

impl FormatSpec {
    pub fn write_str(
        &self,
        string: &str,
        buf: &mut CompactString,
        sym_kind: &'static str,
    ) -> Result<(), FormatError> {
        use std::fmt::Write;

        match self.kind {
            FormatKind::String | FormatKind::Default => {
                let width = self.width;
                write!(
                    buf,
                    "{:width$}",
                    StringFormatter {
                        string,
                        escape: self.be_exact
                    }
                )
                .unwrap();
                Ok(())
            }
            fmt_kind => Err(FormatError::BadKind { sym_kind, fmt_kind }),
        }
    }
}
struct StringFormatter<'string> {
    string: &'string str,
    escape: bool,
}
impl Display for StringFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.escape {
            write!(f, "{}", self.string.escape_default())
        } else {
            write!(f, "{}", self.string)
        }
    }
}
