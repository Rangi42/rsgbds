/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

//! Convenience functionality shared between the RGBDS executables.

use std::fmt::Display;

pub mod argfile;
shadow_rs::shadow!(build);
pub mod cli;
pub mod dash_stdio;
pub mod diagnostics;
pub mod section;

// https://doc.rust-lang.org/nightly/edition-guide/rust-2024/rpit-lifetime-capture.html#migrating-away-from-the-captures-trick
#[doc(hidden)]
pub trait Captures<T: ?Sized> {}
impl<T: ?Sized, U: ?Sized> Captures<T> for U {}

#[derive(Debug, Clone, Copy)]
pub enum S {
    One,
    Other,
}
macro_rules! impl_from {
    ($t:ty) => {
        impl From<$t> for S {
            fn from(value: $t) -> Self {
                match value {
                    1 => Self::One,
                    _ => Self::Other,
                }
            }
        }
    };
}
impl_from!(u8);
impl_from!(u16);
impl_from!(u32);
impl_from!(u64);
impl_from!(u128);
impl_from!(usize);
impl Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::Other = self {
            f.write_str("s")?;
        }
        Ok(())
    }
}
