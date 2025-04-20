/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

//! Convenience functionality shared between the RGBDS executables.

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
