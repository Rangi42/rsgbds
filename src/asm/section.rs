use compact_str::CompactString;
use indexmap::IndexMap;
use rustc_hash::FxHasher;

#[derive(Debug)]
pub struct Sections<'ctx_stack> {
    sections: IndexMap<CompactString, Section<'ctx_stack>, FxHasher>,
    active_section_id: Option<usize>,
    section_stack: Vec<Option<usize>>,
}

#[derive(Debug)]
pub struct Section<'ctx_stack> {
    address: Option<u16>,
    bank: Option<u32>,
    _dummy: &'ctx_stack (),
}

impl<'ctx_stack> Sections<'ctx_stack> {
    pub fn new() -> Self {
        Self {
            sections: IndexMap::with_hasher(FxHasher::default()),
            active_section_id: None,
            section_stack: vec![],
        }
    }

    pub fn active_section_mut(&mut self) -> Option<&mut Section<'ctx_stack>> {
        self.active_section_id
            .map(|index| self.sections.get_index_mut(index).unwrap().1)
    }
}
