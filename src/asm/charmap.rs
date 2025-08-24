use std::{
    cell::Cell,
    collections::{hash_map::Entry, HashMap},
};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    common::Captures,
    diagnostics::{self, warning},
    sources::Span,
    Identifier, Identifiers, Options,
};

#[derive(Debug)]
pub struct Charmaps {
    // TODO(perf): consider using an `IndexMap` instead
    charmaps: Vec<Charmap>,
    active_charmap_id: usize,
    // TODO(perf): consider using `smallvec` or such, since this is hardly ever more than 1.
    stack: Vec<usize>,
}

#[derive(Debug)]
pub struct Charmap {
    def_span: Span,
    name: Identifier,
    nodes: Vec<Node>,
    root_node: Children,
}
type Children = FxHashMap<char, usize>;
#[derive(Debug, Clone)]
struct Node {
    children: Children,
    mapping: Vec<i32>,
}

const DEFAULT_CHARMAP_NAME: &str = "main";

impl Charmaps {
    pub fn new(identifiers: &mut Identifiers) -> Self {
        Self {
            charmaps: vec![Charmap::new(
                Span::Builtin,
                identifiers.get_or_intern_static(DEFAULT_CHARMAP_NAME),
            )],
            active_charmap_id: 0,
            stack: vec![],
        }
    }

    fn find_charmap(&self, name: Identifier) -> Option<usize> {
        self.charmaps
            .iter()
            .position(|charmap| name == charmap.name)
    }
    pub fn make_new(&mut self, name: Identifier, def_span: Span) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        self.charmaps.push(Charmap::new(def_span, name));
        self.active_charmap_id = self.charmaps.len() - 1;
        Ok(())
    }
    pub fn make_copy(
        &mut self,
        name: Identifier,
        def_span: Span,
        target_name: Identifier,
    ) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        let Some(i) = self.find_charmap(target_name) else {
            return Err(CharmapError::NoSuchCharmap(target_name, def_span));
        };
        self.charmaps.push(Charmap {
            def_span,
            name,
            nodes: vec![],
            root_node: self.charmaps[i].root_node.clone(),
        });
        self.active_charmap_id = self.charmaps.len() - 1;
        Ok(())
    }
    pub fn switch_to(&mut self, name: Identifier) -> Option<()> {
        // TODO: if the charmap doesn't exist, suggest closely-named ones
        self.active_charmap_id = self.find_charmap(name)?;
        Some(())
    }
    pub fn active_charmap(&self) -> &Charmap {
        &self.charmaps[self.active_charmap_id]
    }
    pub fn active_charmap_mut(&mut self) -> &mut Charmap {
        &mut self.charmaps[self.active_charmap_id]
    }
    pub fn is_main_charmap_active(&self) -> bool {
        self.active_charmap_id == 0
    }

    pub fn push_active_charmap(&mut self) {
        self.stack.push(self.active_charmap_id);
    }
    pub fn pop_active_charmap(&mut self) -> Option<()> {
        self.active_charmap_id = self.stack.pop()?;
        Some(())
    }

    pub fn warn_if_stack_not_empty(&self, nb_errors_left: &Cell<usize>, options: &Options) {
        if !self.stack.is_empty() {
            diagnostics::warn(
                warning!("unmatched-directive"),
                &Span::TopLevel,
                |warning| {
                    warning.set_message("`pushc` without corresponding `popc`");
                    if self.stack.len() != 1 {
                        warning.set_note(format!("{} unclosed `pushc`s", self.stack.len()));
                    }
                },
                nb_errors_left,
                options,
            )
        }
    }
}
impl Charmap {
    fn new(def_span: Span, name: Identifier) -> Self {
        Self {
            def_span,
            name,
            nodes: vec![],
            root_node: HashMap::with_capacity_and_hasher(128, FxBuildHasher),
        }
    }

    pub fn add_mapping(&mut self, string: &str, values: Vec<i32>) -> Option<()> {
        let mut chars = string.chars();
        let mut node_id = *self.root_node.entry(chars.next()?).or_insert_with(|| {
            self.nodes.push(Node::new());
            self.nodes.len() - 1
        });
        for c in chars {
            let next_node_id = self.nodes.len();
            node_id = match self.nodes[node_id].children.entry(c) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    entry.insert(next_node_id);
                    self.nodes.push(Node::new());
                    next_node_id
                }
            };
        }
        // TODO: warn if a mapping already exists
        self.nodes[node_id].mapping = values;
        Some(())
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    fn warn_on_passthrough(
        &self,
        c: char,
        is_main_charmap: bool,
        span: &Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        if !self.is_empty() {
            // Warn if this character is not mapped, but any others are.
            diagnostics::warn(
                diagnostics::warning!("unmapped-char=1"),
                span,
                |warning| {
                    warning.set_message(format!("'{}' is not mapped", c.escape_default()));
                    warning
                        .add_label(diagnostics::warning_label(span).with_message("in this string"));
                },
                nb_errors_left,
                options,
            );
        } else if !is_main_charmap {
            // Warn if this character is not mapped, except in the default charmap.
            diagnostics::warn(
                diagnostics::warning!("unmapped-char=2"),
                span,
                |warning| {
                    warning.set_message(format!(
                        "'{}' is not mapped in charmap \"{}\"",
                        c.escape_default(),
                        identifiers.resolve(self.name).unwrap(),
                    ));
                    warning
                        .add_label(diagnostics::warning_label(span).with_message("in this string"));
                },
                nb_errors_left,
                options,
            );
        }
    }

    pub fn encode<'a>(
        &self,
        (string, string_span): (&'a str, &'a Span),
        is_main_charmap: bool,
        identifiers: &'a Identifiers,
        nb_errors_left: &'a Cell<usize>,
        options: &'a Options,
    ) -> impl Iterator<Item = CharMapping<'_>> + Captures<&'a ()> {
        Encoder {
            charmap: self,
            string,
            string_span,
            is_main_charmap,
            identifiers,
            nb_errors_left,
            options,
        }
    }
}
#[derive(Debug)]
struct Encoder<'charmap, 'a> {
    charmap: &'charmap Charmap,
    string: &'a str,
    string_span: &'a Span,
    is_main_charmap: bool,
    identifiers: &'a Identifiers,
    nb_errors_left: &'a Cell<usize>,
    options: &'a Options,
}
impl<'charmap> Iterator for Encoder<'charmap, '_> {
    type Item = CharMapping<'charmap>;

    fn next(&mut self) -> Option<Self::Item> {
        // RGBASM matches charmaps using the common “leftmost-longest” scheme.
        let chars = self.string.char_indices();

        let mut children = &self.charmap.root_node;
        let mut target_node = None;
        // Read characters from the string, either until we reach a dead end, or exhaust the string.
        for (offset, c) in chars {
            let Some(&node_id) = children.get(&c) else {
                break;
            };
            let node = &self.charmap.nodes[node_id];
            if !node.mapping.is_empty() {
                target_node = Some((&node.mapping, offset + c.len_utf8()));
            }
            children = &node.children;
        }

        if let Some((mapping, nb_bytes)) = target_node {
            self.string = &self.string[nb_bytes..];
            Some(CharMapping::Mapped(mapping))
        } else {
            // Default identity mapping: just return one character.
            let c = self.string.chars().next()?;
            self.string = &self.string[c.len_utf8()..];
            self.charmap.warn_on_passthrough(
                c,
                self.is_main_charmap,
                self.string_span,
                self.identifiers,
                self.nb_errors_left,
                self.options,
            );
            Some(CharMapping::Passthrough(c))
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum CharMapping<'charmap> {
    Mapped(&'charmap [i32]),
    Passthrough(char),
}
impl<'charmap> IntoIterator for CharMapping<'charmap> {
    type IntoIter = CharValues<'charmap>;
    type Item = i32;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            CharMapping::Mapped(slice) => CharValues::Mapped(slice.iter()),
            CharMapping::Passthrough(ch) => CharValues::Passthrough(Some(ch)),
        }
    }
}
#[derive(Debug)]
pub enum CharValues<'charmap> {
    Mapped(std::slice::Iter<'charmap, i32>),
    Passthrough(Option<char>),
}
impl Iterator for CharValues<'_> {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Mapped(iter) => iter.next().copied(),
            Self::Passthrough(opt) => opt.take().map(|ch| ch as u32 as i32),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}
impl ExactSizeIterator for CharValues<'_> {
    fn len(&self) -> usize {
        match self {
            Self::Mapped(values) => values.len(),
            Self::Passthrough(_) => 1,
        }
    }
}

#[derive(Debug)]
pub enum CharmapError {
    Conflict(Identifier, Span, Span),
    NoSuchCharmap(Identifier, Span),
}
impl CharmapError {
    pub fn diag_span(&self) -> &Span {
        match self {
            CharmapError::Conflict(_, span, _) => span,
            CharmapError::NoSuchCharmap(_, span) => span,
        }
    }
    pub fn make_diag<'span>(
        &'span self,
        error: &mut crate::diagnostics::ReportBuilder<'span>,
        identifiers: &Identifiers,
    ) {
        match self {
            CharmapError::Conflict(name, this_def, existing) => {
                error.set_message(format!(
                    "a charmap called \"{}\" already exists",
                    identifiers.resolve(*name).unwrap()
                ));
                error.add_labels([
                    diagnostics::error_label(this_def)
                        .with_message("attempting to create it here..."),
                    diagnostics::error_label(existing)
                        .with_message("...but it was previously defined here"),
                ])
            }
            CharmapError::NoSuchCharmap(name, span) => {
                error.set_message(format!(
                    "no charmap called \"{}\" exists",
                    identifiers.resolve(*name).unwrap()
                ));
                error.add_label(diagnostics::error_label(span).with_message("requested here"));
            }
        }
    }
}

impl Node {
    fn new() -> Self {
        Self {
            children: Default::default(),
            mapping: vec![],
        }
    }
}
