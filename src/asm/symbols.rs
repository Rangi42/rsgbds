use std::cell::Cell;

use chrono::prelude::*;
use compact_str::CompactString;
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::{backend::StringBackend, symbol::SymbolU32, StringInterner};

use crate::{
    diagnostics,
    format::FormatSpec,
    macro_args::MacroArgs,
    sources::{NormalSpan, Span},
    Identifier, Identifiers, Options,
};

type SymMap = FxHashMap<Identifier, SymbolData>;

// TODO: consider using a `Vec<Option<SymbolData>>` instead of a hash map?
#[derive(Debug)]
pub struct Symbols {
    symbols: SymMap,
    scope: Option<Identifier>,
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
    Label, // TODO
    Ref,
}

impl Symbols {
    pub fn new(identifiers: &mut Identifiers) -> Self {
        let mut this = Self {
            symbols: FxHashMap::with_hasher(FxBuildHasher),
            scope: None,
        };

        let mut def_builtin = |name, kind| {
            let name_sym = identifiers.get_or_intern_static(name);
            let res = this.symbols.insert(name_sym, kind);
            debug_assert!(res.is_none());
        };
        let numeric = |value, mutable| SymbolData::Builtin(SymbolKind::Numeric { value, mutable });
        let string = |string| SymbolData::Builtin(SymbolKind::String(string));

        def_builtin("@", SymbolData::Pc);
        def_builtin("_NARG", SymbolData::Narg);
        def_builtin("_RS", numeric(0, true));
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

        let now = chrono::Local::now();
        let now_utc = now.with_timezone(&chrono::Utc);
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

        this
    }

    pub fn find(&self, name: &Identifier) -> Option<&SymbolData> {
        self.symbols.get(name)
    }

    pub fn find_macro(&self, name: &Identifier) -> Option<Result<&NormalSpan, &SymbolData>> {
        match self.find(name) {
            Some(SymbolData::User {
                kind: SymbolKind::Macro(slice),
                ..
            }) => Some(Ok(slice)),
            None => None,
            Some(sym) => Some(Err(sym)),
        }
    }

    pub fn format_as<'sym>(
        &'sym self,
        name: Option<Identifier>,
        fmt: &FormatSpec,
        buf: &mut CompactString,
        macro_args: Option<&MacroArgs>,
    ) -> Result<(), FormatError<'sym>> {
        let Some(sym) = name.and_then(|name| self.find(&name)) else {
            return Err(FormatError::NotFound);
        };
        if let Some(value) = sym.get_number(macro_args) {
            todo!()
        } else if let Some(s) = sym.get_string() {
            fmt.write_str(&s, buf);
        } else if let SymbolData::Deleted(span) = sym {
            return Err(FormatError::Deleted(span));
        } else {
            return Err(FormatError::BadKind);
        };

        Ok(())
    }

    fn try_define_symbol<'map>(
        symbols: &'map mut SymMap,
        name: Identifier,
        definition: Span,
        kind: SymbolKind,
        exported: bool,
    ) -> Result<(), (&'map mut SymbolData, Span)> {
        use std::collections::hash_map::Entry;

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
                    SymbolData::User {
                        kind: SymbolKind::Ref,
                        exported: previously_exported,
                        ..
                    } if matches!(kind, SymbolKind::Label | SymbolKind::Numeric { .. }) => {
                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported: exported || *previously_exported,
                        };
                        Ok(())
                    }
                    _ => Err((existing, definition)),
                }
            }
        }
    }

    fn define_symbol(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        payload: SymbolKind,
        exported: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        if let Err((existing, definition)) =
            Self::try_define_symbol(&mut self.symbols, name, definition, payload, exported)
        {
            diagnostics::error(
                &definition,
                |error| {
                    error.set_message(format!(
                        "A symbol called \"{}\" already exists",
                        identifiers.resolve(name).unwrap()
                    ));
                    error.add_labels([
                        diagnostics::note_label(existing.def_span())
                            .with_message("The name is used here..."),
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
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::String(string),
            false,
            nb_errors_left,
            options,
        );
    }

    pub fn define_label(
        &mut self,
        name: Identifier,
        identifiers: &Identifiers,
        definition: Span,
        exported: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Label,
            exported,
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
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.define_symbol(
            name,
            identifiers,
            definition,
            SymbolKind::Macro(body),
            false,
            nb_errors_left,
            options,
        )
    }
}

pub enum FormatError<'sym> {
    NotFound,
    Deleted(&'sym Span),
    BadKind,
}

impl SymbolData {
    pub fn def_span(&self) -> &Span {
        match self {
            Self::User { definition, .. } => definition,
            _ => &Span::Builtin,
        }
    }

    pub fn get_number(&self, macro_args: Option<&MacroArgs>) -> Option<i32> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { value, .. } => Some(*value),
                SymbolKind::String(..) => None,
                SymbolKind::Macro(_) => None,
                SymbolKind::Label => todo!(),
                SymbolKind::Ref => None,
            },
            Self::Pc => todo!(),
            Self::Narg => todo!(),
            Self::Dot => None,
            Self::DotDot => None,
            Self::Deleted(..) => None,
        }
    }

    pub fn get_string(&self) -> Option<CompactString> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(string) => Some(string.clone()),
                SymbolKind::Macro(_) => None,
                SymbolKind::Label => None,
                SymbolKind::Ref => None,
            },
            Self::Pc => None,
            Self::Narg => None,
            Self::Dot => todo!(),
            Self::DotDot => todo!(),
            Self::Deleted(..) => None,
        }
    }
}
