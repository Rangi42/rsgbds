use std::{
    cell::Cell,
    collections::{hash_map::DefaultHasher, HashMap},
    hash::BuildHasherDefault,
};

use compact_str::CompactString;

use crate::{
    common::Captures, context_stack::Span, diagnostics, source_store::SourceStore, Options,
};

#[derive(Debug)]
pub struct Charmaps<'ctx_stack> {
    charmaps: Vec<Charmap<'ctx_stack>>,
    active_charmap_id: usize,
    // TODO: consider using `smallvec` or such, since this is hardly ever more than 1.
    stack: Vec<usize>,
}

#[derive(Debug)]
pub struct Charmap<'ctx_stack> {
    def_span: Span<'ctx_stack>,
    name: CompactString,
    nodes: Vec<Node>,
    root_node: Children,
}
type Children = HashMap<char, usize, BuildHasherDefault<DefaultHasher>>;
#[derive(Debug, Clone)]
struct Node {
    children: Children,
    parent_id: usize,
    mapping: Vec<i32>,
}

const DEFAULT_CHARMAP_NAME: &str = "main";

impl<'ctx_stack> Charmaps<'ctx_stack> {
    pub fn new() -> Self {
        Self {
            charmaps: vec![Charmap::new(DEFAULT_CHARMAP_NAME.into(), Span::BUILTIN)],
            active_charmap_id: 0,
            stack: vec![],
        }
    }

    fn find_charmap(&self, name: &str) -> Option<usize> {
        self.charmaps
            .iter()
            .position(|charmap| name == charmap.name)
    }
    pub fn make_new(
        &mut self,
        name: CompactString,
        def_span: Span<'ctx_stack>,
    ) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(&name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        self.charmaps.push(Charmap::new(name, def_span));
        Ok(())
    }
    pub fn make_copy(
        &mut self,
        name: CompactString,
        def_span: Span<'ctx_stack>,
        target_name: CompactString,
    ) -> Result<(), CharmapError> {
        if let Some(i) = self.find_charmap(&name) {
            return Err(CharmapError::Conflict(
                name,
                def_span,
                self.charmaps[i].def_span.clone(),
            ));
        }
        let Some(i) = self.find_charmap(&target_name) else {
            return Err(CharmapError::NoSuchCharmap(target_name, def_span));
        };
        self.charmaps.push(Charmap {
            def_span,
            name,
            nodes: vec![],
            root_node: self.charmaps[i].root_node.clone(),
        });
        Ok(())
    }
    pub fn switch_to(&mut self, name: &str) -> Option<()> {
        // TODO: if the charmap doesn't exist, suggest closely-named ones
        self.active_charmap_id = self.find_charmap(name)?;
        Some(())
    }
    pub fn active_charmap(&self) -> &Charmap<'ctx_stack> {
        &self.charmaps[self.active_charmap_id]
    }
    pub fn active_charmap_mut(&mut self) -> &mut Charmap<'ctx_stack> {
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
impl<'ctx_stack> Charmap<'ctx_stack> {
    fn new(name: CompactString, def_span: Span<'ctx_stack>) -> Self {
        Self {
            def_span,
            name,
            nodes: vec![],
            root_node: HashMap::with_capacity_and_hasher(128, BuildHasherDefault::default()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn warn_on_passthrough(
        &self,
        c: char,
        span: &Span<'ctx_stack>,
        sources: &SourceStore,
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
                sources,
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
                sources,
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
struct Encoder<'ctx_stack, 'charmap, 'string>(&'charmap Charmap<'ctx_stack>, &'string str);
impl<'charmap> Iterator for Encoder<'_, 'charmap, '_> {
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
pub enum CharmapError<'ctx_stack> {
    Conflict(CompactString, Span<'ctx_stack>, Span<'ctx_stack>),
    NoSuchCharmap(CompactString, Span<'ctx_stack>),
}
impl<'ctx_stack> CharmapError<'ctx_stack> {
    pub fn make_diag(&self, error: &mut crate::source_store::ReportBuilder) {
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
