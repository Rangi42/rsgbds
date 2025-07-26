use std::rc::Rc;

use compact_str::{format_compact, CompactString};

use crate::sources::Source;

/// This should be created using the [`FromIterator`] trait.
#[derive(Debug)]
pub struct MacroArgs {
    args: Vec<Rc<Source>>,
    shift: usize,
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

impl FromIterator<CompactString> for MacroArgs {
    fn from_iter<T: IntoIterator<Item = CompactString>>(iter: T) -> Self {
        Self {
            args: iter
                .into_iter()
                .enumerate()
                .map(|(i, arg)| {
                    Rc::new(Source {
                        name: format_compact!("<macro argument #{}>", i + 1),
                        contents: arg.into(),
                    })
                })
                .collect(),
            shift: 0,
        }
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
