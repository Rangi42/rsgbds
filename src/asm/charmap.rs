use std::{
    cell::Cell,
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap,
    },
    hash::BuildHasherDefault,
};

use compact_str::CompactString;

use crate::{common::Captures, diagnostics, sources::Span, Options};

#[derive(Debug)]
pub struct Charmaps {
    // TODO: consider using an `IndexMap` instead
    charmaps: Vec<Charmap>,
    active_charmap_id: usize,
    // TODO: consider using `smallvec` or such, since this is hardly ever more than 1.
    stack: Vec<usize>,
}

#[derive(Debug)]
pub struct Charmap {
    def_span: Span,
    name: CompactString,
    nodes: Vec<Node>,
    root_node: Children,
}
type Children = HashMap<char, usize, BuildHasherDefault<DefaultHasher>>;
#[derive(Debug, Clone)]
struct Node {
    children: Children,
    mapping: Vec<i32>,
}

const DEFAULT_CHARMAP_NAME: &str = "main";

impl Charmaps {
    pub fn new() -> Self {
        Self {
            charmaps: vec![Charmap::new(DEFAULT_CHARMAP_NAME.into(), Span::Builtin)],
            active_charmap_id: 0,
            stack: vec![],
        }
    }

    fn find_charmap(&self, name: &str) -> Option<usize> {
        self.charmaps
            .iter()
            .position(|charmap| name == charmap.name)
    }
    pub fn make_new(&mut self, name: CompactString, def_span: Span) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(&name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        self.charmaps.push(Charmap::new(name, def_span));
        self.active_charmap_id = self.charmaps.len() - 1;
        Ok(())
    }
    pub fn make_copy(
        &mut self,
        name: CompactString,
        def_span: Span,
        target_name: &str,
    ) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(&name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        let Some(i) = self.find_charmap(target_name) else {
            return Err(CharmapError::NoSuchCharmap(target_name.into(), def_span));
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
    pub fn switch_to(&mut self, name: &str) -> Option<()> {
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

    pub fn push_active_charmap(&mut self) {
        self.stack.push(self.active_charmap_id);
    }
    pub fn pop_active_charmap(&mut self) -> Option<()> {
        self.active_charmap_id = self.stack.pop()?;
        Some(())
    }
}
impl Charmap {
    fn new(name: CompactString, def_span: Span) -> Self {
        Self {
            def_span,
            name,
            nodes: vec![],
            root_node: HashMap::with_capacity_and_hasher(128, BuildHasherDefault::default()),
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

    pub fn warn_on_passthrough(
        &self,
        c: char,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        if !self.is_empty() {
            // Warn if this character is not mapped, but any others are.
            diagnostics::warn(
                diagnostics::warning!("unmapped-char=1"),
                span,
                |warning| {
                    warning.set_message(format!("'{}' is not mapped", c.escape_default(),));
                    warning
                        .add_label(diagnostics::warning_label(span).with_message("In this string"));
                },
                nb_errors_left,
                options,
            );
        } else if self.name != DEFAULT_CHARMAP_NAME {
            // Warn if this character is not mapped, except in the default charmap.
            diagnostics::warn(
                diagnostics::warning!("unmapped-char=2"),
                span,
                |warning| {
                    warning.set_message(format!(
                        "'{}' is not mapped in charmap \"{}\"",
                        c.escape_default(),
                        &self.name,
                    ));
                    warning
                        .add_label(diagnostics::warning_label(span).with_message("In this string"));
                },
                nb_errors_left,
                options,
            );
        }
    }

    pub fn encode<'string>(
        &self,
        string: &'string str,
    ) -> impl Iterator<Item = CharMapping<'_>> + Captures<&'string ()> {
        Encoder(self, string)
    }
}
#[derive(Debug)]
struct Encoder<'charmap, 'string>(&'charmap Charmap, &'string str);
impl<'charmap> Iterator for Encoder<'charmap, '_> {
    type Item = CharMapping<'charmap>;

    fn next(&mut self) -> Option<Self::Item> {
        // RGBASM matches charmaps using the common “leftmost-longest” scheme.
        let chars = self.1.char_indices();

        let mut children = &self.0.root_node;
        let mut target_node = None;
        // Read characters from the string, either until we reach a dead end, or exhaust the string.
        for (offset, c) in chars {
            let Some(&node_id) = children.get(&c) else {
                break;
            };
            let node = &self.0.nodes[node_id];
            if !node.mapping.is_empty() {
                target_node = Some((&node.mapping, offset + c.len_utf8()));
            }
            children = &node.children;
        }

        if let Some((mapping, nb_bytes)) = target_node {
            self.1 = &self.1[nb_bytes..];
            Some(CharMapping::Mapped(mapping))
        } else {
            // Default identity mapping: just return one character.
            let c = self.1.chars().next()?;
            self.1 = &self.1[c.len_utf8()..];
            Some(CharMapping::Passthrough(c))
        }
    }
}
pub enum CharMapping<'charmap> {
    Mapped(&'charmap [i32]),
    Passthrough(char),
}
impl CharMapping<'_> {
    pub fn len(&self) -> usize {
        match self {
            Self::Mapped(values) => values.len(),
            Self::Passthrough(_) => 1,
        }
    }
}

#[derive(Debug)]
pub enum CharmapError {
    Conflict(CompactString, Span, Span),
    NoSuchCharmap(CompactString, Span),
}
impl CharmapError {
    pub fn diag_span(&self) -> &Span {
        match self {
            CharmapError::Conflict(_, span, _) => span,
            CharmapError::NoSuchCharmap(_, span) => span,
        }
    }
    pub fn make_diag<'span>(&'span self, error: &mut crate::diagnostics::ReportBuilder<'span>) {
        match self {
            CharmapError::Conflict(name, this_def, existing) => {
                error.set_message(format!("A charmap called \"{name}\" already exists"));
                error.add_labels([
                    diagnostics::error_label(this_def)
                        .with_message("Attempting to create it here..."),
                    diagnostics::error_label(existing)
                        .with_message("...but it was previously defined here"),
                ])
            }
            CharmapError::NoSuchCharmap(name, span) => {
                error.set_message(format!("No charmap called \"{name}\" exists"));
                error.add_label(diagnostics::error_label(span).with_message("Requested here"));
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
