use std::rc::Rc;

use compact_str::{format_compact, CompactString};

use crate::sources::Source;

/// This should be created using the [`FromIterator`] trait.
#[derive(Debug)]
pub struct MacroArgs {
    args: Vec<Rc<Source>>,
    shift: usize,
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
