use std::{fmt::Display, iter::Peekable, str::Chars};

use compact_str::CompactString;

#[derive(Debug)]
pub struct FormatSpec {
    force_sign: Option<&'static str>,
    exact_prefix: Option<&'static str>,
    align_left: bool,
    pad_with_zeros: bool,
    width: usize,
    frac: Option<usize>,
    precision: u8,
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
            exact_prefix: None, // By default, print the '$' prefix. (This is guaranteed to be `Some`.)
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
    /// unexpected character '{unexpected}' {for_what}
    UnexpectedChar {
        unexpected: char,
        for_what: &'static str,
    },
    /// missing {for_what}
    MissingChar { for_what: &'static str },
    /// missing number after '{0}'
    MissingNumber(char),
    /// a {sym_kind} cannot be formatted as {fmt_kind}
    BadKind {
        sym_kind: &'static str,
        fmt_kind: FormatKind,
    },
    /// a {flag_name} is incompatible with {sym_kind} formatting
    IncompatibleFlag {
        flag_name: &'static str,
        sym_kind: &'static str,
    },
    /// a sign is only applicable to signed or fixed-point formatting
    SignOnUnsigned,
    /// a {flag_name} can only be used with fixed-point formatting
    FractionalFlag { flag_name: &'static str },
    /// fixed-point width cannot be more than 255
    FracWidthOver255,
    /// fixed-point precision cannot be 0
    FixPointZero,
    /// fixed-point precision cannot be more than 31
    FixPointPrecOver31,
}

impl FormatSpec {
    pub fn parse(src: &str, default_precision: u8) -> Result<(Self, &str), FormatError> {
        let mut fmt_chars = src.chars();
        let mut chars = fmt_chars.by_ref().peekable();

        let force_sign = match chars.peek() {
            Some('+') => {
                chars.next();
                Some("+")
            }
            Some(' ') => {
                chars.next();
                Some(" ")
            }
            _ => None,
        };
        let be_exact = chars.next_if_eq(&'#').is_some();
        let align_left = chars.next_if_eq(&'-').is_some();
        let pad_with_zeros = chars.next_if_eq(&'0').is_some();
        let width = chars
            .peek()
            .and_then(|ch| ch.to_digit(10))
            .map_or(0, |first_digit| {
                parse_decimal(&mut chars, first_digit as usize)
            });
        let frac =
            chars
                .next_if_eq(&'.')
                .map(|_ch| match chars.peek().and_then(|ch| ch.to_digit(10)) {
                    Some(first_digit) => parse_decimal(&mut chars, first_digit as usize),
                    None => 0,
                });
        let precision = chars
            .next_if_eq(&'q')
            .map(|_ch| expect_decimal(&mut chars, 'q'))
            .transpose()?;
        let kind = match chars.next() {
            Some('d') => Ok(FormatKind::Signed),
            Some('u') => Ok(FormatKind::Unsigned),
            Some('x') => Ok(FormatKind::LowerHex),
            Some('X') => Ok(FormatKind::UpperHex),
            Some('b') => Ok(FormatKind::Binary),
            Some('o') => Ok(FormatKind::Octal),
            Some('f') => Ok(FormatKind::FixedPoint),

            Some('s') => {
                if pad_with_zeros {
                    Err(FormatError::IncompatibleFlag {
                        flag_name: "zero-padding flag",
                        sym_kind: "string symbol",
                    })
                } else if frac.is_some() {
                    Err(FormatError::IncompatibleFlag {
                        flag_name: "fixed-point width",
                        sym_kind: "string symbol",
                    })
                } else if precision.is_some() {
                    Err(FormatError::IncompatibleFlag {
                        flag_name: "precision",
                        sym_kind: "string symbol",
                    })
                } else {
                    Ok(FormatKind::String)
                }
            }

            Some(unexpected) => Err(FormatError::UnexpectedChar {
                unexpected,
                for_what: "as a print type",
            }),
            None => Err(FormatError::MissingChar {
                for_what: "print type",
            }),
        }?;

        fn expect_decimal(
            chars: &mut Peekable<&mut Chars>,
            trigger_char: char,
        ) -> Result<usize, FormatError> {
            match chars.peek().and_then(|ch| ch.to_digit(10)) {
                None => Err(FormatError::MissingNumber(trigger_char)),
                Some(first_digit) => Ok(parse_decimal(chars, first_digit as usize)),
            }
        }
        fn parse_decimal(chars: &mut Peekable<&mut Chars>, first_digit: usize) -> usize {
            let mut width = first_digit;
            loop {
                chars.next();
                let Some(digit) = chars.peek().and_then(|ch| ch.to_digit(10)) else {
                    break width;
                };
                width = width * 10 + digit as usize;
            }
        }

        if !matches!(kind, FormatKind::FixedPoint) {
            if !matches!(kind, FormatKind::Signed) && force_sign.is_some() {
                return Err(FormatError::SignOnUnsigned);
            }
            if frac.is_some() {
                return Err(FormatError::FractionalFlag {
                    flag_name: "fixed-point width",
                });
            }
            if precision.is_some() {
                return Err(FormatError::FractionalFlag {
                    flag_name: "fixed-point precision",
                });
            }
        }

        match precision {
            Some(32..) => return Err(FormatError::FixPointPrecOver31),
            Some(0) => return Err(FormatError::FixPointZero),
            _ => {} // OK
        }
        let precision = precision.map(|prec| prec as u8);
        debug_assert!(
            matches!(precision, None | Some(1..=31)),
            "bad precision {precision:?} (default = {default_precision})",
        );
        if let Some(256..) = frac {
            return Err(FormatError::FracWidthOver255);
        }
        if pad_with_zeros && align_left {
            return Err(FormatError::IncompatibleFlag {
                flag_name: "zero-padded number",
                sym_kind: "left-aligned",
            });
        }
        let exact_prefix = be_exact
            .then(|| {
                kind.exact_prefix().ok_or(FormatError::IncompatibleFlag {
                    flag_name: "exact prefix",
                    sym_kind: "this kind of",
                })
            })
            .transpose()?;

        Ok((
            Self {
                force_sign,
                exact_prefix,
                align_left,
                pad_with_zeros,
                width,
                frac,
                precision: precision.unwrap_or(default_precision),
                kind,
            },
            fmt_chars.as_str(),
        ))
    }
    pub fn require_full_parse((this, rest): (Self, &str)) -> Result<Self, FormatError> {
        if let Some(unexpected) = rest.chars().next() {
            return Err(FormatError::UnexpectedChar {
                unexpected,
                for_what: "after the print type",
            });
        }
        Ok(this)
    }
}

impl FormatKind {
    fn exact_prefix(&self) -> Option<&'static str> {
        match self {
            FormatKind::Default => unreachable!(),
            FormatKind::Signed => None,
            FormatKind::Unsigned => None,
            FormatKind::LowerHex => Some("$"),
            FormatKind::UpperHex => Some("$"),
            FormatKind::Binary => Some("%"),
            FormatKind::Octal => Some("&"),
            // Those won't be printed, but must be `Some` to indicate that the flag is accepted.
            FormatKind::FixedPoint => Some("\0"),
            FormatKind::String => Some("\0"),
        }
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

        match self.kind {
            FormatKind::Signed
            | FormatKind::Unsigned
            | FormatKind::LowerHex
            | FormatKind::UpperHex
            | FormatKind::Default
            | FormatKind::Binary
            | FormatKind::Octal
            | FormatKind::FixedPoint => {
                let fmt = NumberFormatter {
                    number,
                    force_sign: self.force_sign,
                    exact_prefix: if matches!(self.kind, FormatKind::Default) {
                        Some("$")
                    } else {
                        self.exact_prefix
                    },
                    precision: self.precision,
                    frac: self.frac.unwrap_or(5), // 5 digits is enough for the default Q16.16
                    kind: self.kind,
                };
                if self.pad_with_zeros {
                    write!(buf, "{fmt:#0width$}", width = self.width)
                } else if self.align_left {
                    write!(buf, "{fmt:<#width$}", width = self.width)
                } else {
                    write!(buf, "{fmt:>#width$}", width = self.width)
                }
                .unwrap();
                Ok(())
            }
            fmt_kind => Err(FormatError::BadKind { sym_kind, fmt_kind }),
        }
    }
}
struct NumberFormatter {
    number: u32,
    force_sign: Option<&'static str>,
    exact_prefix: Option<&'static str>,
    precision: u8,
    frac: usize,
    kind: FormatKind,
}
impl Display for NumberFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        static LOWERCASE_DIGITS: &[u8] = &[
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd',
            b'e', b'f',
        ];
        static UPPERCASE_DIGITS: &[u8] = &[
            b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D',
            b'E', b'F',
        ];
        let digits = match self.kind {
            FormatKind::String => unreachable!(),
            FormatKind::FixedPoint => {
                write!(
                    f,
                    "{:.*}",
                    self.frac,
                    self.number as i32 as f64 / (1u32 << self.precision) as f64,
                )?;
                if self.exact_prefix.is_some() {
                    write!(f, "q{}", self.precision)?;
                }
                return Ok(());
            }
            FormatKind::Signed | FormatKind::Unsigned => &LOWERCASE_DIGITS[..10],
            FormatKind::Binary => &LOWERCASE_DIGITS[..2],
            FormatKind::Octal => &LOWERCASE_DIGITS[..8],
            FormatKind::LowerHex => LOWERCASE_DIGITS,
            FormatKind::Default | FormatKind::UpperHex => UPPERCASE_DIGITS,
        };
        let (is_negative, prefix, mut number) = if let Some(prefix) = self.exact_prefix {
            debug_assert_eq!(self.force_sign, None);
            debug_assert_ne!(prefix, "\0");
            (false, prefix, self.number)
        } else if matches!(self.kind, FormatKind::Signed) {
            if (self.number as i32) < 0 {
                (true, "", (self.number as i32).wrapping_neg() as u32)
            } else if let Some(prefix) = self.force_sign {
                (false, prefix, self.number)
            } else {
                (false, "", self.number)
            }
        } else {
            (false, "", self.number)
        };

        let mut buf = [0; 32]; // 32 bits.
        let mut idx = buf.len();
        let base = digits.len() as u32;
        loop {
            // This must run at least once, so that 0 prints one digit.
            idx -= 1;
            buf[idx] = digits[number.rem_euclid(base) as usize];
            number /= base;
            if number == 0 {
                break;
            }
        }

        f.pad_integral(
            !is_negative,
            prefix,
            std::str::from_utf8(&buf[idx..]).unwrap(),
        )
    }
}

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
                write!(
                    buf,
                    "{}",
                    Padding {
                        width: self.width,
                        align_left: self.align_left,
                        inner: StringFormatter {
                            string,
                            escape: self.exact_prefix.is_some()
                        }
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
            let string = self.string.escape_default().to_string();
            f.pad(&string)
        } else {
            f.pad(self.string)
        }
    }
}

struct Padding<D: Display> {
    width: usize,
    align_left: bool,
    inner: D,
}
impl<D: Display> Display for Padding<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = self.width;
        if self.align_left {
            write!(f, "{:<width$}", &self.inner)
        } else {
            write!(f, "{:>width$}", &self.inner)
        }
    }
}
