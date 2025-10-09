use std::{cell::Cell, collections::hash_map::Entry};

use chrono::prelude::*;
use compact_str::{format_compact, CompactString, ToCompactString};
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::Symbol;

use crate::{
    common::S,
    diagnostics::{self, warning},
    expr::Expr,
    format::{FormatError, FormatSpec},
    macro_args::MacroArgs,
    section::{ActiveSections, SectionId, Sections},
    sources::{NormalSpan, Span},
    Identifier, Identifiers, Options,
};

type SymMap = FxHashMap<Identifier, SymbolData>;

// TODO: consider using a `Vec<Option<SymbolData>>` instead of a hash map?
#[derive(Debug)]
pub struct Symbols {
    pub symbols: SymMap,
    anon_label_idx: u32,
}

#[derive(Debug)]
pub enum SymbolData {
    User {
        definition: Span,
        kind: SymbolKind,
        exported: bool,
    },

    /// Built-in symbols, but that don't have special behaviour.
    Builtin(SymbolKind),
    // These builtins *do* have special behaviour.
    Pc,
    Narg,
    Dot,
    DotDot,

    /// Placeholder left over after purging a symbol, to improve error messages.
    Deleted(Span),
    /// Placeholder to support exporting a symbol before it's actually defined.
    ExportPlaceholder(Span),
}

#[derive(Debug)]
pub enum SymbolKind {
    Numeric {
        value: i32,
        mutable: bool,
    },
    String(CompactString),
    Macro(NormalSpan),
    Label {
        section_id: SectionId,
        offset: usize,
    },
    Function {
        param_names: Vec<Identifier>,
        expr: Expr,
    },
}

impl Symbols {
    pub fn new(
        identifiers: &mut Identifiers,
        cli_defines: Vec<String>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Self {
        let mut this = Self {
            symbols: FxHashMap::with_hasher(FxBuildHasher),
            anon_label_idx: 0,
        };

        let mut def_builtin = |name, kind| {
            let name_sym = identifiers.get_or_intern_static(name);
            let res = this.symbols.insert(name_sym, kind);
            debug_assert!(res.is_none());
            name_sym
        };
        let numeric = |value, mutable| SymbolData::Builtin(SymbolKind::Numeric { value, mutable });
        let string = |string| SymbolData::Builtin(SymbolKind::String(string));

        let pc_sym = def_builtin("@", SymbolData::Pc);
        debug_assert_eq!(pc_sym, Self::pc_ident()); // Be careful, this identifier is special.
        let rs_sym = def_builtin("_RS", numeric(0, true));
        debug_assert_eq!(rs_sym, Self::rs_ident()); // And so is this one.
        def_builtin("_NARG", SymbolData::Narg);
        def_builtin(".", SymbolData::Dot);
        def_builtin("..", SymbolData::DotDot);

        def_builtin(
            "__RGBDS_VERSION__",
            string(CompactString::const_new(crate::common::build::PKG_VERSION)),
        );
        def_builtin(
            "__RGBDS_MAJOR__",
            numeric(
                crate::common::build::PKG_VERSION_MAJOR
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_MAJOR),
                false,
            ),
        );
        def_builtin(
            "__RGBDS_MINOR__",
            numeric(
                crate::common::build::PKG_VERSION_MINOR
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_MINOR),
                false,
            ),
        );
        def_builtin(
            "__RGBDS_PATCH__",
            numeric(
                crate::common::build::PKG_VERSION_PATCH
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_PATCH),
                false,
            ),
        );
        // This symbol is only defined for release candidates.
        if let Some(rc) = crate::common::build::PKG_VERSION_PRE.strip_prefix("-rc") {
            def_builtin(
                "__RGBDS_RC__",
                numeric(
                    rc.parse().expect(crate::common::build::PKG_VERSION_PRE),
                    false,
                ),
            );
        }

        // https://reproducible-builds.org/docs/source-date-epoch/
        let (now_utc, now) = match std::env::var("SOURCE_DATE_EPOCH") {
            Err(std::env::VarError::NotPresent) => Err(None),
            Ok(var) => match var.parse() {
                Ok(timestamp) => {
                    use chrono::LocalResult;
                    match Utc.timestamp_opt(timestamp, 0) {
                        LocalResult::Single(now_utc) => {
                            Ok((now_utc, now_utc.with_timezone(&Local)))
                        }
                        LocalResult::None => Err(Some("unrepresentable timestamp".into())),
                        // The API promises not to return this.
                        LocalResult::Ambiguous(_, _) => unreachable!("ambiguous timestamp!?"),
                    }
                }
                Err(err) => Err(Some(err.to_string())),
            },
            Err(err @ std::env::VarError::NotUnicode(_)) => Err(Some(err.to_string())),
        }
        .unwrap_or_else(|res| {
            if let Some(err) = res {
                diagnostics::error(
                    &Span::CommandLine,
                    |error| {
                        error.set_message(
                            "unable to parse `SOURCE_DATE_EPOCH` environment variable",
                        );
                        error.set_note(err);
                    },
                    nb_errors_left,
                    options,
                )
            }
            let now = chrono::Local::now();
            (now.with_timezone(&chrono::Utc), now)
        });
        def_builtin(
            "__ISO_8601_LOCAL__",
            string(now.format("%Y-%m-%dT%H:%M:%S%z").to_compact_string()),
        );
        def_builtin(
            "__ISO_8601_UTC__",
            string(now_utc.format("%Y-%m-%dT%H:%M:%SZ").to_compact_string()),
        );
        def_builtin("__UTC_YEAR__", numeric(now_utc.year(), false));
        def_builtin("__UTC_MONTH__", numeric(now_utc.month() as i32, false));
        def_builtin("__UTC_DAY__", numeric(now_utc.day() as i32, false));
        def_builtin("__UTC_HOUR__", numeric(now_utc.hour() as i32, false));
        def_builtin("__UTC_MINUTE__", numeric(now_utc.minute() as i32, false));
        def_builtin("__UTC_SECOND__", numeric(now_utc.second() as i32, false));

        for define in cli_defines {
            let (name, value) = define.split_once('=').unwrap_or((&define, "1"));
            let ident = identifiers.get_or_intern(name);
            this.define_string(
                ident,
                identifiers,
                Span::CommandLine,
                value.into(),
                false, // Not redefining.
                None,
                None,
                nb_errors_left,
                options,
            );
        }

        this
    }

    pub fn pc_ident() -> Identifier {
        Identifier::try_from_usize(0).unwrap()
    }

    fn rs_ident() -> Identifier {
        Identifier::try_from_usize(1).unwrap()
    }

    pub fn find(&self, name: &Identifier) -> Option<&SymbolData> {
        self.symbols.get(name)
    }

    pub fn find_macro(
        &self,
        name: &Identifier,
    ) -> Result<Result<&NormalSpan, &SymbolData>, Option<&Span>> {
        match self.find(name) {
            Some(SymbolData::User {
                kind: SymbolKind::Macro(slice),
                ..
            }) => Ok(Ok(slice)),
            None => Err(None),
            Some(SymbolData::Deleted(span)) => Err(Some(span)),
            Some(sym) => Ok(Err(sym)),
        }
    }

    pub fn format_as<'name, 'sym>(
        &'sym self,
        name: Option<Identifier>,
        name_str: &'name str,
        fmt: &FormatSpec,
        buf: &mut CompactString,
        macro_args: Option<&MacroArgs>,
        identifiers: &Identifiers,
        sections: &Sections,
    ) -> Result<(), SymbolError<'name, 'sym>> {
        let sym = name
            .and_then(|name| self.find(&name))
            .ok_or(SymbolError::NotFound(name_str))?;

        if let Some(value) = sym.get_number(macro_args, sections) {
            fmt.write_number(
                value?.ok_or(SymbolError::NotConst(name_str))? as u32,
                buf,
                sym.kind_name(),
            )?;
            Ok(())
        } else if let Some(res) = sym.get_string(sections.active_section.as_ref(), identifiers) {
            fmt.write_str(&res?, buf, sym.kind_name())?;
            Ok(())
        } else if let SymbolData::Deleted(span) = sym {
            Err(SymbolError::Deleted(name_str, span))
        } else {
            Err(SymbolError::FormatError(FormatError::BadKind {
                sym_kind: sym.kind_name(),
                fmt_kind: fmt.kind,
            }))
        }
    }

    fn try_define_symbol<'syms>(
        symbols: &'syms mut SymMap,
        name: Identifier,
        definition: Span,
        kind: SymbolKind,
        mut exported: bool,
        redef: bool,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), (&'syms mut SymbolData, Span)> {
        match symbols.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolData::User {
                    definition,
                    kind,
                    exported,
                });
                Ok(())
            }
            Entry::Occupied(entry) => {
                let existing = entry.into_mut();
                match existing {
                    // If the entry is merely occupied by a placeholder, just override it.
                    SymbolData::Deleted(..) => {
                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported,
                        };
                        Ok(())
                    }
                    SymbolData::ExportPlaceholder(export_span) => {
                        Self::export_user_symbol(
                            export_span,
                            &definition,
                            &mut exported,
                            &kind,
                            name,
                            identifiers,
                            nb_errors_left,
                            options,
                        );
                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported,
                        };
                        Ok(())
                    }
                    SymbolData::User {
                        definition: cur_definition,
                        kind: cur_kind,
                        exported: cur_exported,
                    } if redef && kind.is_same_kind(cur_kind) => {
                        *cur_definition = definition;
                        *cur_kind = kind;
                        *cur_exported |= exported;
                        Ok(())
                    }
                    SymbolData::User {
                        kind:
                            SymbolKind::Numeric {
                                value,
                                mutable: true,
                            },
                        ..
                    }
                    | SymbolData::Builtin(SymbolKind::Numeric {
                        value,
                        mutable: true,
                    }) => match kind {
                        SymbolKind::Numeric {
                            value: new_value,
                            mutable: true,
                        } => {
                            *value = new_value;
                            Ok(())
                        }
                        _ => Err((existing, definition)),
                    },
                    _ => Err((existing, definition)),
                }
            }
        }
    }

    fn export_user_symbol(
        span: &Span,
        definition: &Span,
        exported: &mut bool,
        kind: &SymbolKind,
        name: Identifier,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        match kind {
            // TODO: a warning if exporting a symbol twice
            SymbolKind::Label { .. } | SymbolKind::Numeric { .. } => *exported = true,
            _ => diagnostics::error(
                span,
                |error| {
                    error.set_message("cannot export non-numeric symbol");
                    error.add_labels([
                        diagnostics::error_label(span).with_message(format!(
                            "cannot export `{}`",
                            identifiers.resolve(name).unwrap()
                        )),
                        diagnostics::error_label(definition)
                            .with_message(format!("defined here as {}", kind.name())),
                    ]);
                },
                nb_errors_left,
                options,
            ),
        }
    }
    pub fn export(
        &mut self,
        name: Identifier,
        span: Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        match self.symbols.entry(name) {
            Entry::Vacant(entry) => {
                diagnostics::warn(
                    warning!("export-undefined"),
                    &span,
                    |warning| {
                        warning.set_message("exporting a symbol before it's defined");
                        warning.add_label(diagnostics::warning_label(&span).with_message(format!(
                            "`{}` is not defined at this point",
                            identifiers.resolve(name).unwrap(),
                        )));
                    },
                    nb_errors_left,
                    options,
                );
                entry.insert(SymbolData::ExportPlaceholder(span));
            }

            Entry::Occupied(mut entry) => match entry.get_mut() {
                data @ SymbolData::Deleted(..) => {
                    *data = SymbolData::ExportPlaceholder(span);
                }
                SymbolData::User {
                    exported,
                    kind,
                    definition,
                } => Self::export_user_symbol(
                    &span,
                    definition,
                    exported,
                    kind,
                    name,
                    identifiers,
                    nb_errors_left,
                    options,
                ),
                _ => diagnostics::error(
                    &span,
                    |error| {
                        error.set_message("cannot export built-in symbol");
                        error.add_label(diagnostics::error_label(&span).with_message(format!(
                            "cannot export `{}`",
                            identifiers.resolve(name).unwrap()
                        )));
                    },
                    nb_errors_left,
                    options,
                ),
            },
        }
    }

    fn define_symbol(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        payload: SymbolKind,
        exported: bool,
        redef: bool,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        if let Err((existing, definition)) = Self::try_define_symbol(
            &mut self.symbols,
            name,
            definition,
            payload,
            exported,
            redef,
            identifiers,
            nb_errors_left,
            options,
        ) {
            diagnostics::error(
                &definition,
                |error| {
                    error.set_message(format!(
                        "A symbol called \"{}\" already exists",
                        identifiers.resolve(name).unwrap()
                    ));
                    // TODO: this only really makes sense for user-defined symbols; print a better error message for builtins
                    error.add_labels([
                        diagnostics::note_label(existing.def_span()).with_message(format!(
                            "the name is {} here...",
                            if existing.exists(active_sections, macro_args) {
                                "defined"
                            } else {
                                "reserved"
                            }
                        )),
                        diagnostics::error_label(&definition)
                            .with_message("...so it's not available for this definition"),
                    ]);
                    if matches!(existing, SymbolData::User { .. }) {
                        // TODO: don't display this for labels
                        error.set_help("If this is intentional, consider using `PURGE` to delete the old definition first");
                    }
                },
                nb_errors_left,
                options,
            )
        }
    }

    pub fn define_string(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        string: CompactString,
        redef: bool,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::String(string),
            false, // Not exported.
            redef,
            active_sections,
            macro_args,
            nb_errors_left,
            options,
        );
    }

    pub fn define_label(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        (section_id, offset): (usize, usize),
        exported: bool,
        active_sections: &mut ActiveSections,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        active_sections.set_sym_scope(name, identifiers);

        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Label { section_id, offset },
            exported || options.export_all,
            false, // Not redefining.
            Some(active_sections),
            macro_args,
            nb_errors_left,
            options,
        )
    }

    pub fn define_constant(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        value: i32,
        mutable: bool,
        exported: bool,
        redef: bool,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Numeric { value, mutable },
            exported,
            redef,
            active_sections,
            macro_args,
            nb_errors_left,
            options,
        )
    }

    pub fn define_function(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        param_names: Vec<Identifier>,
        expr: Expr,
        redef: bool,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Function { param_names, expr },
            false, // Not exported.
            redef,
            active_sections,
            macro_args,
            nb_errors_left,
            options,
        )
    }

    pub fn define_macro(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        body: NormalSpan,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Macro(body),
            false, // Not exported.
            false, // Not redefining.
            active_sections,
            macro_args,
            nb_errors_left,
            options,
        )
    }

    pub fn rs(&mut self) -> &mut i32 {
        let Some(SymbolData::Builtin(SymbolKind::Numeric {
            value,
            mutable: true,
        })) = self.symbols.get_mut(&Self::rs_ident())
        else {
            unreachable!()
        };
        value
    }

    pub fn delete(
        &mut self,
        name: Identifier,
        deletion_span: Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        match self.symbols.entry(name) {
            Entry::Vacant(_) => diagnostics::error(
                &deletion_span,
                |error| {
                    error.set_message("cannot delete a symbol that doesn't exist");
                    error.add_label(diagnostics::error_label(&deletion_span).with_message(
                        format!(
                            "no symbol named `{}` exists at this point",
                            identifiers.resolve(name).unwrap(),
                        ),
                    ));
                },
                nb_errors_left,
                options,
            ),

            Entry::Occupied(mut entry) => {
                let sym = entry.get_mut();
                match sym {
                    SymbolData::User { exported, kind, .. } => {
                        if matches!(kind, SymbolKind::Label { .. }) {
                            diagnostics::error(
                                &deletion_span,
                                |warning| {
                                    warning.set_message("labels cannot be deleted");
                                    warning.add_label(
                                        diagnostics::warning_label(&deletion_span).with_message(
                                            format!(
                                                "attempting to delete `{}` here",
                                                identifiers.resolve(name).unwrap(),
                                            ),
                                        ),
                                    );
                                },
                                nb_errors_left,
                                options,
                            );
                        } else {
                            if *exported {
                                diagnostics::warn(
                                    warning!("purge"),
                                    &deletion_span,
                                    |warning| {
                                        warning.set_message("deleting an exported symbol");
                                        warning.add_label(
                                            diagnostics::warning_label(&deletion_span)
                                                .with_message(format!(
                                                    "deleting `{}` here",
                                                    identifiers.resolve(name).unwrap(),
                                                )),
                                        );
                                    },
                                    nb_errors_left,
                                    options,
                                );
                            }
                            *sym = SymbolData::Deleted(deletion_span);
                        }
                    }

                    SymbolData::Deleted(span) => diagnostics::error(
                        &deletion_span,
                        |error| {
                            error.set_message(format!(
                                "`{}` was already deleted",
                                identifiers.resolve(name).unwrap()
                            ));
                            error.add_labels([
                                diagnostics::error_label(&deletion_span)
                                    .with_message("cannot perform this deletion..."),
                                diagnostics::error_label(span)
                                    .with_message("...because of this one"),
                            ])
                        },
                        nb_errors_left,
                        options,
                    ),

                    _ => diagnostics::error(
                        &deletion_span,
                        |error| {
                            error.set_message(format!(
                                "cannot delete built-in symbol `{}`",
                                identifiers.resolve(name).unwrap(),
                            ));
                            error.add_label(
                                diagnostics::error_label(&deletion_span)
                                    .with_message("cannot perform this deletion"),
                            )
                        },
                        nb_errors_left,
                        options,
                    ),
                }
            }
        }
    }

    pub fn reject_placeholders(
        &self,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        for (name, sym) in &self.symbols {
            if let SymbolData::ExportPlaceholder(span) = sym {
                diagnostics::error(
                    span,
                    |error| {
                        error.set_message("cannot export undefined symbol");
                        error.add_label(diagnostics::error_label(span).with_message(format!(
                            "`{}` is not defined at or after this point",
                            identifiers.resolve(*name).unwrap(),
                        )));
                    },
                    nb_errors_left,
                    options,
                );
            }
        }
    }

    fn anon_label_ident(identifiers: &mut Identifiers, idx: u32) -> Identifier {
        let name = format_compact!("<anonymous label {idx}>");
        identifiers.get_or_intern(&name)
    }
    pub fn cur_anon_label(&self, identifiers: &mut Identifiers) -> Identifier {
        Self::anon_label_ident(identifiers, self.anon_label_idx)
    }
    pub fn defined_anon_label(&mut self) {
        self.anon_label_idx += 1;
    }
    pub fn anon_label_name(
        &self,
        offset: i32,
        identifiers: &mut Identifiers,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Option<Identifier> {
        let Some(idx) = self.anon_label_idx.checked_add_signed(offset) else {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("anonymous label offset out of range");
                    error.add_label(diagnostics::error_label(span).with_message(format!(
                        "{} anonymous label{} have been defined thus far",
                        self.anon_label_idx,
                        S::from(self.anon_label_idx),
                    )));
                },
                nb_errors_left,
                options,
            );
            return None;
        };
        Some(Self::anon_label_ident(identifiers, idx))
    }
}

#[derive(Debug, displaydoc::Display)]
pub enum SymbolError<'name, 'sym> {
    /// the symbol `{0}` doesn't exist
    NotFound(&'name str),
    /// the symbol `{0}` was deleted
    // TODO: highlight that span
    Deleted(&'name str, &'sym Span),
    /// {0}
    FormatError(FormatError),
    /// `{0}` is not constant
    NotConst(&'name str),
    /// `{0}` is not a numeric symbol
    NotNumeric(&'name str),
    /// `{0}` is not a string symbol
    NotString(&'name str),
    /// PC doesn't have a {0} outside of a section
    PcOutsideSect(&'static str),
    /// `_NARG` doesn't have a value outside of a macro
    NargOutsideMacro,
    /// `.` doesn't have a value outside of a label scope
    DotOutsideMacro,
    /// `..` doesn't have a value outside of a local label scope
    DotDotOutsideMacro,
}
impl From<FormatError> for SymbolError<'_, '_> {
    fn from(value: FormatError) -> Self {
        Self::FormatError(value)
    }
}

impl SymbolKind {
    fn is_same_kind(&self, other: &Self) -> bool {
        match (self, other) {
            (
                SymbolKind::Numeric { mutable, .. },
                SymbolKind::Numeric {
                    mutable: other_mutable,
                    ..
                },
            ) => *mutable == *other_mutable,
            (SymbolKind::String(_), SymbolKind::String(_))
            | (SymbolKind::Macro(_), SymbolKind::Macro(_))
            | (SymbolKind::Label { .. }, SymbolKind::Label { .. })
            | (SymbolKind::Function { .. }, SymbolKind::Function { .. }) => true,
            (_, _) => false,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Self::Numeric { mutable, .. } => {
                if *mutable {
                    "variable"
                } else {
                    "constant"
                }
            }
            Self::String(_) => "string symbol",
            Self::Macro(_) => "macro",
            Self::Label { .. } => "label",
            Self::Function { .. } => "function",
        }
    }
}

impl SymbolData {
    pub fn def_span(&self) -> &Span {
        match self {
            Self::User { definition, .. } => definition,
            Self::Deleted(span) | Self::ExportPlaceholder(span) => span,
            _ => &Span::Builtin,
        }
    }

    pub fn exists(
        &self,
        active_sections: Option<&ActiveSections>,
        macro_args: Option<&MacroArgs>,
    ) -> bool {
        match self {
            SymbolData::User { .. } | SymbolData::Builtin(..) => true,
            SymbolData::Deleted(..) | SymbolData::ExportPlaceholder(..) => false,
            SymbolData::Pc => active_sections.is_some(),
            SymbolData::Narg => macro_args.is_some(),
            SymbolData::Dot => active_sections
                .and_then(|active| active.sym_scope(0))
                .is_some(),
            Self::DotDot => active_sections
                .and_then(|active| active.sym_scope(1))
                .is_some(),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            SymbolData::User { kind, .. } | SymbolData::Builtin(kind) => kind.name(),
            SymbolData::Pc => "label",
            SymbolData::Narg => "constant",
            SymbolData::Dot | SymbolData::DotDot => "string symbol",
            SymbolData::Deleted(_) => "deleted",
            Self::ExportPlaceholder(..) => "undefined",
        }
    }

    pub fn get_string(
        &self,
        active_sections: Option<&ActiveSections>,
        identifiers: &Identifiers,
    ) -> Option<Result<CompactString, SymbolError<'static, 'static>>> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(string) => Some(Ok(string.clone())),
                SymbolKind::Macro(_) => None,
                SymbolKind::Label { .. } => None,
                SymbolKind::Function { .. } => None,
            },
            Self::Pc => None,
            Self::Narg => None,
            Self::Dot => Some(
                match active_sections.and_then(|active| active.sym_scope(0)) {
                    Some(ident) => Ok(identifiers.resolve(ident).unwrap().into()),
                    None => Err(SymbolError::DotOutsideMacro),
                },
            ),
            Self::DotDot => Some(
                match active_sections.and_then(|active| active.sym_scope(1)) {
                    Some(ident) => Ok(identifiers.resolve(ident).unwrap().into()),
                    None => Err(SymbolError::DotDotOutsideMacro),
                },
            ),
            Self::Deleted(..) => None,
            Self::ExportPlaceholder(..) => None,
        }
    }

    /// # Returns
    ///
    /// - `None` for non-numeric symbols;
    /// - `Some(Err())` if the symbol's value is an error (e.g. PC outside of a section);
    /// - `Some(Ok(None))` if the symbol doesn't have a constant value.
    pub fn get_number(
        &self,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Option<Result<Option<i32>, SymbolError<'static, 'static>>> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { value, .. } => Some(Ok(Some(*value))),
                SymbolKind::String(..) => None,
                SymbolKind::Macro(_) => None,
                SymbolKind::Label { section_id, offset } => Some(Ok(sections
                    .find(*section_id)
                    .address()
                    .map(|base_addr| base_addr as i32 + *offset as i32))),
                SymbolKind::Function { .. } => None,
            },
            Self::Pc => Some(match sections.active_section.as_ref() {
                Some(active) => Ok(sections.sections[active.sym_section.id]
                    .address()
                    .map(|addr| i32::from(addr) + active.sym_section.offset as i32)),
                None => Err(SymbolError::PcOutsideSect("value")),
            }),
            Self::Narg => Some(match macro_args {
                Some(args) => Ok(Some(args.max_valid() as i32)),
                None => Err(SymbolError::NargOutsideMacro),
            }),
            Self::Dot => None,
            Self::DotDot => None,
            Self::Deleted(..) => None,
            Self::ExportPlaceholder(..) => None,
        }
    }

    /// # Returns
    ///
    /// - `None` for non-label symbols;
    /// - `Some(Err())` if the symbol's bank is an error (e.g. PC outside of a section);
    pub fn get_section_and_offset(
        &self,
        sections: &Sections,
    ) -> Option<Result<(usize, usize), SymbolError<'static, 'static>>> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Label { section_id, offset } => Some(Ok((*section_id, *offset))),
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(_) => None,
                SymbolKind::Macro(_) => None,
                SymbolKind::Function { .. } => None,
            },
            Self::Pc => match sections.active_section.as_ref() {
                Some(active) => Some(Ok((active.sym_section.id, active.sym_section.offset))),
                None => Some(Err(SymbolError::PcOutsideSect("bank"))),
            },
            Self::Narg => None,
            Self::Dot => None,
            Self::DotDot => None,
            Self::Deleted(_) => None,
            Self::ExportPlaceholder(..) => None,
        }
    }
}
