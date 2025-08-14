use std::{cell::Cell, collections::hash_map::Entry};

use chrono::prelude::*;
use compact_str::CompactString;
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::Symbol;

use crate::{
    diagnostics::{self, warning},
    format::{FormatError, FormatSpec},
    macro_args::MacroArgs,
    section::{ActiveSection, Sections},
    sources::{NormalSpan, Span},
    Identifier, Identifiers, Options,
};

type SymMap = FxHashMap<Identifier, SymbolData>;

// TODO: consider using a `Vec<Option<SymbolData>>` instead of a hash map?
#[derive(Debug)]
pub struct Symbols {
    pub symbols: SymMap,
    pub scope: Option<Identifier>,
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
}

#[derive(Debug)]
pub enum SymbolKind {
    Numeric { value: i32, mutable: bool },
    String(CompactString),
    Macro(NormalSpan),
    Label { section_id: usize, offset: usize },
    Ref,
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
            scope: None,
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
            "__TIME__",
            string(now.format("\"%H:%M:%S\"").to_string().into()),
        );
        def_builtin(
            "__DATE__",
            string(now.format("\"%d %B %Y\"").to_string().into()),
        );
        def_builtin(
            "__ISO_8601_LOCAL__",
            string(
                now.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
                    .into(),
            ),
        );
        def_builtin(
            "__ISO_8601_UTC__",
            string(
                now_utc
                    .to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
                    .into(),
            ),
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
        } else if let Some(s) = sym.get_string() {
            fmt.write_str(&s, buf, sym.kind_name())?;
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

    fn try_define_symbol(
        symbols: &mut SymMap,
        name: Identifier,
        definition: Span,
        kind: SymbolKind,
        exported: bool,
        redef: bool,
    ) -> Result<(), (&mut SymbolData, Span)> {
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
                    // Numeric symbols override "references" (themselves essentially placeholders).
                    // References also do not conflict with themselves.
                    SymbolData::User {
                        kind: SymbolKind::Ref,
                        exported: previously_exported,
                        ..
                    } if matches!(
                        kind,
                        SymbolKind::Label { .. } | SymbolKind::Numeric { .. } | SymbolKind::Ref
                    ) =>
                    {
                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported: exported || *previously_exported,
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
                entry.insert(SymbolData::User {
                    definition: span,
                    kind: SymbolKind::Ref,
                    exported: true,
                });
            }

            Entry::Occupied(mut entry) => match entry.get_mut() {
                SymbolData::Deleted(..) => {
                    entry.insert(SymbolData::User {
                        definition: span,
                        kind: SymbolKind::Ref,
                        exported: true,
                    });
                }
                SymbolData::User {
                    exported,
                    kind: SymbolKind::Label { .. } | SymbolKind::Numeric { .. } | SymbolKind::Ref,
                    ..
                } => *exported = true,
                ref sym @ SymbolData::User { ref definition, .. } => diagnostics::error(
                    &span,
                    |error| {
                        error.set_message("cannot export non-numeric symbol");
                        error.add_labels([
                            diagnostics::error_label(&span).with_message(format!(
                                "cannot export `{}`",
                                identifiers.resolve(name).unwrap()
                            )),
                            diagnostics::error_label(definition)
                                .with_message(format!("defined here as {}", sym.kind_name())),
                        ]);
                    },
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
        active_section: Option<&ActiveSection>,
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
        ) {
            diagnostics::error(
                &definition,
                |error| {
                    error.set_message(format!(
                        "A symbol called \"{}\" already exists",
                        identifiers.resolve(name).unwrap()
                    ));
                    error.add_labels([
                        diagnostics::note_label(existing.def_span()).with_message(format!(
                            "the name is {} here...",
                            if existing.exists(self.scope.as_ref(), active_section, macro_args) {
                                "defined"
                            } else {
                                "reserved"
                            }
                        )),
                        diagnostics::error_label(&definition)
                            .with_message("...so it's not available for this definition"),
                    ]);
                    error.set_help("If this is intentional, consider using `PURGE` to delete the old definition first");
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
        active_section: Option<&ActiveSection>,
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
            active_section,
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
        active_section: Option<&ActiveSection>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        // TODO: update the scope

        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Label { section_id, offset },
            exported || options.export_all,
            false, // Not redefining.
            active_section,
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
        active_section: Option<&ActiveSection>,
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
            active_section,
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
        active_section: Option<&ActiveSection>,
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
            active_section,
            macro_args,
            nb_errors_left,
            options,
        )
    }

    pub fn create_ref(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        ref_span: Span,
        active_section: Option<&ActiveSection>,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            ref_span,
            SymbolKind::Ref,
            false, // Not exported.
            false, // Not redefining.
            active_section,
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
                    SymbolData::User {
                        definition,
                        kind: SymbolKind::Ref,
                        ..
                    } => diagnostics::error(
                        &deletion_span,
                        |error| {
                            error.set_message("cannot delete a symbol that doesn't exist");
                            error.add_labels([
                                diagnostics::error_label(&deletion_span).with_message(format!(
                                    "no symbol named `{}` exists at this point",
                                    identifiers.resolve(name).unwrap(),
                                )),
                                diagnostics::error_label(definition)
                                    .with_message("the name was previously referenced here"),
                            ]);
                        },
                        nb_errors_left,
                        options,
                    ),

                    SymbolData::User { exported, kind, .. } => {
                        if *exported {
                            diagnostics::warn(
                                warning!("purge=1"),
                                &deletion_span,
                                |warning| {
                                    warning.set_message("deleting an exported symbol");
                                    warning.add_label(
                                        diagnostics::warning_label(&deletion_span).with_message(
                                            format!(
                                                "deleting `{}` here",
                                                identifiers.resolve(name).unwrap(),
                                            ),
                                        ),
                                    );
                                },
                                nb_errors_left,
                                options,
                            );
                        } else if matches!(kind, SymbolKind::Label { .. }) {
                            diagnostics::warn(
                                warning!("purge=2"),
                                &deletion_span,
                                |warning| {
                                    warning.set_message("deleting a label");
                                    warning.add_label(
                                        diagnostics::warning_label(&deletion_span).with_message(
                                            format!(
                                                "deleting `{}` here",
                                                identifiers.resolve(name).unwrap(),
                                            ),
                                        ),
                                    );
                                },
                                nb_errors_left,
                                options,
                            );
                        }
                        *sym = SymbolData::Deleted(deletion_span)
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
}

#[derive(Debug, displaydoc::Display)]
pub enum SymbolError<'name, 'sym> {
    /// the symbol `{0}` doesn't exist
    NotFound(&'name str),
    /// the symbol `{0}` was deleted
    Deleted(&'name str, &'sym Span),
    /// {0}
    FormatError(FormatError),
    /// `{0}` is not constant
    NotConst(&'name str),
    /// `{0}` is not a numeric symbol
    NotNumeric(&'name str),
    /// PC doesn't have a {0} outside of a section
    PcOutsideSect(&'static str),
    /// _NARG doesn't have a value outside of a macro
    NargOutsideMacro,
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
            | (SymbolKind::Ref, SymbolKind::Ref) => true,
            (_, _) => false,
        }
    }
}

impl SymbolData {
    pub fn def_span(&self) -> &Span {
        match self {
            Self::User { definition, .. } => definition,
            _ => &Span::Builtin,
        }
    }

    pub fn exists(
        &self,
        scope: Option<&Identifier>,
        active_section: Option<&ActiveSection>,
        macro_args: Option<&MacroArgs>,
    ) -> bool {
        match self {
            SymbolData::User { .. } | SymbolData::Builtin(..) => true,
            SymbolData::Deleted(..) => false,
            SymbolData::Pc => active_section.is_some(),
            SymbolData::Narg => macro_args.is_some(),
            SymbolData::Dot | SymbolData::DotDot => scope.is_some(),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            SymbolData::User { kind, .. } | SymbolData::Builtin(kind) => match kind {
                SymbolKind::Numeric { mutable, .. } => {
                    if *mutable {
                        "variable"
                    } else {
                        "constant"
                    }
                }
                SymbolKind::String(_) => "string",
                SymbolKind::Macro(_) => "macro",
                SymbolKind::Label { .. } => "label",
                SymbolKind::Ref => "missing",
            },
            SymbolData::Pc => "label",
            SymbolData::Narg => "constant",
            SymbolData::Dot | SymbolData::DotDot => "string",
            SymbolData::Deleted(_) => "deleted",
        }
    }

    pub fn get_string(&self) -> Option<CompactString> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(string) => Some(string.clone()),
                SymbolKind::Macro(_) => None,
                SymbolKind::Label { .. } => None,
                SymbolKind::Ref => None,
            },
            Self::Pc => None,
            Self::Narg => None,
            Self::Dot => todo!(),
            Self::DotDot => todo!(),
            Self::Deleted(..) => None,
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
                SymbolKind::Ref => None,
            },
            Self::Pc => Some(match sections.active_section.as_ref() {
                Some((_data_sect, sym_sect)) => Ok(sections.sections[sym_sect.id]
                    .address()
                    .map(|addr| i32::from(addr) + sym_sect.offset as i32)),
                None => Err(SymbolError::PcOutsideSect("value")),
            }),
            Self::Narg => Some(match macro_args {
                Some(args) => Ok(Some(args.max_valid() as i32)),
                None => Err(SymbolError::NargOutsideMacro),
            }),
            Self::Dot => None,
            Self::DotDot => None,
            Self::Deleted(..) => None,
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
            SymbolData::User { kind, .. } | SymbolData::Builtin(kind) => match kind {
                SymbolKind::Label { section_id, offset } => Some(Ok((*section_id, *offset))),
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(_) => None,
                SymbolKind::Macro(_) => None,
                SymbolKind::Ref => None,
            },
            SymbolData::Pc => match sections.active_section.as_ref() {
                Some((_data_sect, sym_sect)) => Some(Ok((sym_sect.id, sym_sect.offset))),
                None => Some(Err(SymbolError::PcOutsideSect("bank"))),
            },
            SymbolData::Narg => None,
            SymbolData::Dot => None,
            SymbolData::DotDot => None,
            SymbolData::Deleted(_) => None,
        }
    }
}
