use std::{cell::Cell, cmp::Ordering, rc::Rc};

use compact_str::{format_compact, CompactString};

use crate::{
    common::S,
    diagnostics::{self, warning},
    sources::{Source, Span},
    Options,
};

/// This should be created using the [`FromIterator`] trait.
#[derive(Debug)]
pub struct MacroArgs {
    args: Vec<Rc<Source>>,
    shift: usize,
    combined_args: Option<Rc<Source>>,
}

#[derive(Debug)]
pub struct UniqueId {
    /// If the `Vec` is empty, no unique ID is available.
    /// Otherwise, it contains `Some` for contexts that have requested a unique ID,
    /// and `None` for contexts that haven't yet.
    // TODO(perf): consider using a `smallvec` or similar
    cur_id: Vec<Option<Rc<Source>>>,
    next_id: u32,
}

impl MacroArgs {
    pub fn new() -> Self {
        Self {
            args: vec![],
            shift: 0,
            combined_args: None,
        }
    }

    pub fn push_arg(mut self, arg: CompactString) -> Self {
        self.args.push(Rc::new(Source {
            name: format_compact!("<macro argument #{}>", self.args.len() + 1),
            contents: arg.into(),
        }));
        self
    }
}

impl MacroArgs {
    pub fn arg(&self, idx: usize) -> Option<&Rc<Source>> {
        self.args.get(idx - 1 + self.shift)
    }

    pub fn max_valid(&self) -> usize {
        // Not `- 1`, because the arguments are 1-indexed!
        self.args.len().saturating_sub(self.shift)
    }

    pub fn shift_by(
        &mut self,
        offset: isize,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        match offset.cmp(&0) {
            Ordering::Equal => return, // Nothing will happen, we don't even need to invalidate the cache.

            Ordering::Greater if offset as usize > self.max_valid() => {
                diagnostics::warn(
                    warning!("macro-shift"),
                    span,
                    |warning| {
                        warning.set_message(format!(
                            "attempting to shift by more than the {} available macro argument{}",
                            self.max_valid(),
                            S::from(self.max_valid()),
                        ));
                        warning.add_label(
                            diagnostics::warning_label(span)
                                .with_message(format!("attempting to shift by {offset} here")),
                        );
                    },
                    nb_errors_left,
                    options,
                );

                self.shift = self.args.len();
            }

            Ordering::Less if -offset as usize > self.shift => {
                diagnostics::warn(
                    warning!("macro-shift"),
                    span,
                    |warning| {
                        warning.set_message(format!(
                            "attempting to unshift by more than the {} shifted argument{}",
                            self.shift,
                            S::from(self.shift),
                        ));
                        warning.add_label(
                            diagnostics::warning_label(span)
                                .with_message(format!("attempting to shift by {offset} here")),
                        );
                    },
                    nb_errors_left,
                    options,
                );

                self.shift = 0;
            }

            _ => self.shift = self.shift.wrapping_add_signed(offset),
        }

        self.combined_args = None; // Invalidate the cache.
    }

    pub fn combined_args(&mut self) -> &Rc<Source> {
        self.combined_args.get_or_insert_with(|| {
            let args = &self.args[self.shift..];
            let mut contents = CompactString::with_capacity(
                args.iter()
                    .fold(0, |acc, arg| acc + arg.contents.text().len() + 1),
            );
            for (i, arg) in args.iter().enumerate() {
                contents.push_str(arg.contents.text());
                // Commas go between arguments, and after an empty argument.
                if i + self.shift + 1 < self.args.len() || arg.contents.is_empty() {
                    contents.push(',');
                }
            }
            Rc::new(Source {
                name: "<combined macro args>".into(),
                contents: contents.into(),
            })
        })
    }
}

impl UniqueId {
    pub fn new() -> Self {
        Self {
            cur_id: Vec::with_capacity(4),
            next_id: 0,
        }
    }

    pub fn unique_id(&mut self) -> Option<&Rc<Source>> {
        self.cur_id.last_mut().map(|slot| {
            // A unique ID is available in this context. Retrieve it if cached, otherwise generate and cache it.
            &*slot.get_or_insert_with(|| {
                let new_id = self.next_id;
                self.next_id += 1;
                Rc::new(Source {
                    name: format_compact!("<unique ID>"),
                    contents: format_compact!("_u{new_id}").into(),
                })
            })
        })
    }

    pub fn enter_unique_ctx(&mut self) {
        self.cur_id.push(None);
    }
    pub fn exit_unique_ctx(&mut self) {
        let elem = self.cur_id.pop();
        debug_assert!(elem.is_some());
    }
    pub fn debug_check_empty(&self) {
        debug_assert!(self.cur_id.is_empty());
    }
}
