use std::{
    cell::Cell,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use rustc_hash::FxBuildHasher;
use sysexits::ExitCode;

use crate::{
    common::section::MemRegion,
    diagnostics,
    expr::{BinOp, Expr, OpKind, UnOp},
    section::{
        AddrConstraint, AssertLevel, Contents, LinkTimeExpr, PatchKind, SectionKind, Sections,
    },
    sources::{FileNode, NormalSpan, Span, SpanKind},
    symbols::{SymbolData, SymbolKind, Symbols},
    Identifier, Identifiers, Options,
};

type IndexMap<K, V> = indexmap::IndexMap<K, V, FxBuildHasher>;
type IndexSet<V> = indexmap::IndexSet<V, FxBuildHasher>;

type FileNodeRegistry<'nodes> = IndexSet<&'nodes FileNode>;
type SymRegistry<'sym> = IndexMap<Identifier, Option<(&'sym NormalSpan, &'sym SymbolKind, bool)>>;

const VERSION_STRING: &[u8] = b"RGB9";
const REVISION: u32 = 13;

pub fn emit(
    path: &Path,
    identifiers: &Identifiers,
    sections: &Sections,
    symbols: &Symbols,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) -> Result<(), ExitCode> {
    // TODO: `dash_stdio`
    let file = File::create(path).map_err(|err| {
        diagnostics::error(
            &Span::TopLevel,
            |error| {
                error.set_message("unable to write object file");
                error.add_label(diagnostics::error_label(&Span::TopLevel).with_message(err));
            },
            nb_errors_left,
            options,
        );
        ExitCode::CantCreat
    })?;
    let registered_symbols = register_symbols(symbols, sections);
    let registered_file_nodes = register_file_nodes(sections, &registered_symbols);
    let mut ctx = WriteContext {
        identifiers,
        sections,
        symbols,
        nb_errors_left,
        options,

        file: BufWriter::new(file),
        registered_symbols: &registered_symbols,
        registered_file_nodes,
    };

    let io_err = |res: std::io::Result<()>| {
        res.map_err(|err| {
            diagnostics::error(
                &Span::TopLevel,
                |error| {
                    error.set_message("error writing object file");
                    error.add_label(diagnostics::error_label(&Span::TopLevel).with_message(err));
                },
                nb_errors_left,
                options,
            );
            ExitCode::IoErr
        })
    };
    let usize_to_u32 = |value: usize, what: &str| {
        u32::try_from(value).map_err(|err| {
            diagnostics::error(
                &Span::TopLevel,
                |error| {
                    error.set_message(format!("too many {what}"));
                    error.add_label(diagnostics::error_label(&Span::TopLevel).with_message(err));
                },
                nb_errors_left,
                options,
            );
            ExitCode::DataErr
        })
    };

    for (_name, section) in &sections.sections {
        // TODO: report the section for which there are too many patches
        let _ = usize_to_u32(section.patches.len(), "patches")?;
    }

    io_err(ctx.write_header(
        usize_to_u32(ctx.registered_symbols.len(), "symbols")?,
        usize_to_u32(ctx.sections.sections.len(), "sections")?,
        usize_to_u32(ctx.registered_file_nodes.len(), "file nodes")?,
    ))?;
    io_err(ctx.write_file_nodes())?;
    io_err(ctx.write_symbols())?;
    io_err(ctx.write_sections())?;
    io_err(ctx.write_assertions(usize_to_u32(ctx.sections.assertions.len(), "assertions")?))?;

    io_err(ctx.file.flush())?;
    Ok(())
}

fn register_file_nodes<'nodes>(
    sections: &'nodes Sections,
    registered_symbols: &'nodes SymRegistry,
) -> FileNodeRegistry<'nodes> {
    let mut registry = IndexSet::default();

    // Registers a span's node, as well as all of its parents, recursively.
    let mut register_span = |span: &'nodes NormalSpan| {
        let mut span = hard_span_of(span);
        // If the node is newly inserted, we need to register its parents as well.
        // If the node is already present in the registry, then we can assume its parent are as well.
        while registry.insert(&span.node) {
            let Some(parent) = span.node.parent.as_deref() else {
                break;
            };
            span = parent;
        }
    };

    for (_name, opt) in registered_symbols {
        if let Some((span, _kind, _exported)) = opt {
            register_span(span);
        }
    }
    for (_name, sect) in &sections.sections {
        let Span::Normal(span) = &sect.def_span else {
            unreachable!()
        };
        register_span(span);
        for patch in &sect.patches {
            let Span::Normal(span) = &patch.rest.span else {
                unreachable!()
            };
            register_span(span);
        }
    }
    for assertion in &sections.assertions {
        let Span::Normal(span) = &assertion.rest.span else {
            unreachable!()
        };
        register_span(span);
    }

    registry
}

fn register_symbols<'sym>(symbols: &'sym Symbols, sections: &Sections) -> SymRegistry<'sym> {
    let mut registry = IndexMap::default();

    // Emit all exported symbols.
    registry.extend(symbols.symbols.iter().filter_map(|(name, sym)| {
        if let SymbolData::User {
            definition: Span::Normal(span),
            kind,
            exported: true,
        } = sym
        {
            Some((*name, Some((span, kind, true))))
        } else {
            None
        }
    }));
    // Emit all symbols referenced by patches.
    registry.extend(
        sections
            .all_link_time_exprs()
            .flat_map(|expr| expr.expr.ops().filter_map(|op| op.get_symbol()))
            .flat_map(|ident| match symbols.symbols.get(&ident) {
                None => Some((ident, None)),
                // Treat placeholders the same way as if the symbol wasn't defined at all.
                Some(sym) if !sym.exists(None, None, None, None) => Some((ident, None)),
                Some(SymbolData::User {
                    definition: Span::Normal(span),
                    kind,
                    exported,
                }) => Some((ident, Some((span, kind, *exported)))),
                // Builtin symbol.
                Some(_) => None,
            }),
    );

    registry
}

struct WriteContext<'nodes, 'ident, 'sect: 'nodes, 'sym: 'nodes, 'nerr, 'opt> {
    identifiers: &'ident Identifiers,
    sections: &'sect Sections,
    symbols: &'sym Symbols,
    nb_errors_left: &'nerr Cell<usize>,
    options: &'opt Options,

    file: BufWriter<File>,
    registered_symbols: &'nodes SymRegistry<'sym>,
    registered_file_nodes: FileNodeRegistry<'nodes>,
}
impl WriteContext<'_, '_, '_, '_, '_, '_> {
    fn resolve_span(&self, span: &NormalSpan) -> (u32, u32) {
        let span = hard_span_of(span);

        let node_num = self
            .registered_file_nodes
            .get_index_of(&span.node)
            .expect("Unregistered file node!?");
        let line_no = span
            .node
            .src
            .contents
            .get_byte_line(span.bytes.start)
            .expect("Span start out of range!?")
            .line_idx
            + 1;
        // We know there are less than `u32::MAX` nodes, from the earlier size check.
        // The line number is unlikely to ever be more than u32::MAX, but even if it is, this'll just lead to incorrect error reporting.
        (node_num as u32, line_no as u32)
    }
}
fn hard_span_of(mut span: &NormalSpan) -> &NormalSpan {
    while span.node.kind.ends_implicitly() {
        span = span
            .node
            .parent
            .as_ref()
            .expect("Implicitly-ending context without a parent!?");
    }
    span
}

fn write_byte(mut output: impl Write, byte: u8) -> std::io::Result<()> {
    output.write_all(std::array::from_ref(&byte))
}
fn write_long(mut output: impl Write, long: u32) -> std::io::Result<()> {
    output.write_all(&long.to_le_bytes())
}
fn write_string(mut output: impl Write, string: &str) -> std::io::Result<()> {
    assert!(!string.as_bytes().contains(&0)); // TODO: be more graceful(?)
    output.write_all(string.as_bytes())?;
    output.write_all(&[0])
}
impl WriteContext<'_, '_, '_, '_, '_, '_> {
    fn write_byte(&mut self, byte: u8) -> std::io::Result<()> {
        write_byte(&mut self.file, byte)
    }
    fn write_long(&mut self, long: u32) -> std::io::Result<()> {
        write_long(&mut self.file, long)
    }
    fn write_string(&mut self, string: &str) -> std::io::Result<()> {
        write_string(&mut self.file, string)
    }

    fn write_header(
        &mut self,
        nb_symbols: u32,
        nb_sections: u32,
        nb_file_nodes: u32,
    ) -> std::io::Result<()> {
        self.file.write_all(VERSION_STRING)?;
        self.write_long(REVISION)?;
        self.write_long(nb_symbols)?;
        self.write_long(nb_sections)?;
        self.write_long(nb_file_nodes)?;

        Ok(())
    }

    fn write_file_nodes(&mut self) -> std::io::Result<()> {
        for node in self.registered_file_nodes.iter().rev() {
            let (parent_id, parent_line_no) = match node.parent.as_deref() {
                Some(parent) => self.resolve_span(parent),
                None => (u32::MAX, 0),
            };
            write_long(&mut self.file, parent_id)?;
            write_long(&mut self.file, parent_line_no)?;

            match node.kind {
                SpanKind::File => {
                    write_byte(&mut self.file, 1)?;
                    write_string(&mut self.file, &node.src.name)?;
                }
                SpanKind::Macro(ident) => {
                    write_byte(&mut self.file, 2)?;
                    let macro_name = self.identifiers.resolve(ident).unwrap();
                    write_string(&mut self.file, &format!("{}::{macro_name}", node.src.name))?;
                }
                SpanKind::Loop(_) => {
                    write_byte(&mut self.file, 0)?;
                    let depth = {
                        let mut iter = *node;
                        let mut depth = 0;
                        while matches!(iter.kind, SpanKind::Loop(..)) {
                            depth += 1;
                            iter = &iter.parent.as_ref().unwrap().node;
                        }
                        depth
                    };
                    write_long(&mut self.file, depth)?;
                    let mut iter = *node;
                    while let SpanKind::Loop(iteration_num) = &iter.kind {
                        write_long(&mut self.file, *iteration_num)?;
                        iter = &iter.parent.as_ref().unwrap().node;
                    }
                }
                _ => {
                    debug_assert!(node.kind.ends_implicitly());
                    unreachable!("Registered non-hard node!?")
                }
            };
        }
        Ok(())
    }

    fn write_symbols(&mut self) -> std::io::Result<()> {
        for (&ident, opt) in self.registered_symbols.as_slice() {
            self.write_string(self.identifiers.resolve(ident).unwrap())?;

            match opt {
                None => self.write_byte(1)?,

                Some((definition, kind, exported)) => {
                    self.write_byte(if *exported { 2 } else { 0 })?;

                    let (node_id, line_no) = self.resolve_span(definition);
                    self.write_long(node_id)?;
                    self.write_long(line_no)?;

                    let (value, section_id) = match kind {
                        SymbolKind::String(_)
                        | SymbolKind::Macro(_)
                        | SymbolKind::Function { .. } => unreachable!(),
                        SymbolKind::Numeric { value, .. } => (*value as u32, u32::MAX),
                        SymbolKind::Label { section_id, offset } => {
                            // The offset must be in u32 range, as section sizes are lower than u16 range.
                            // Section IDs are also checked for validity by the earlier check of the number of sections.
                            (*offset as u32, *section_id as u32)
                        }
                    };
                    self.write_long(section_id)?;
                    self.write_long(value)?;
                }
            }
        }
        Ok(())
    }

    fn write_sections(&mut self) -> std::io::Result<()> {
        for (name, section) in self.sections.sections.as_slice() {
            self.write_string(name)?;

            let Span::Normal(span) = &section.def_span else {
                unreachable!("section not defined normally!?")
            };
            let (node_id, line_no) = self.resolve_span(span);
            self.write_long(node_id)?;
            self.write_long(line_no)?;

            // Section sizes have been checked, and are known to be below u16::MAX.
            self.write_long(section.bytes.len() as u32)?;

            let flags = match section.attrs.kind {
                SectionKind::Normal => 0x00,
                SectionKind::Union => 0x80,
                SectionKind::Fragment => 0x40,
            };
            let mem_region = serialise_region(section.attrs.mem_region);
            self.write_byte(flags | mem_region)?;

            self.write_long(section.address().map_or(u32::MAX, Into::into))?;
            self.write_long(section.attrs.bank.unwrap_or(u32::MAX))?;

            match section.attrs.address {
                AddrConstraint::Align(alignment, ofs) => {
                    self.write_byte(alignment)?;
                    self.write_long(ofs.into())?;
                }
                AddrConstraint::None | AddrConstraint::Addr(_) => {
                    self.write_byte(0)?;
                    self.write_long(0)?;
                }
            }

            match &section.bytes {
                Contents::NoData(_) => {
                    debug_assert_eq!(section.patches.len(), 0, "data-less section has patches!?")
                }
                Contents::Data(data) => {
                    self.file.write_all(data)?;

                    // Section patch counts have been checked earlier.
                    self.write_long(section.patches.len() as u32)?;
                    for patch in &section.patches {
                        self.write_link_time_expr(
                            &patch.rest,
                            match patch.kind {
                                PatchKind::Byte => 0,
                                PatchKind::Word => 1,
                                PatchKind::Long => 2,
                                PatchKind::Jr => 3,
                            },
                        )?;
                    }
                }
            }
        }
        Ok(())
    }

    fn write_assertions(&mut self, nb_asserts: u32) -> std::io::Result<()> {
        self.write_long(nb_asserts)?;

        for assertion in &self.sections.assertions {
            self.write_link_time_expr(
                &assertion.rest,
                match assertion.level {
                    AssertLevel::Warn => 0,
                    AssertLevel::Error => 1,
                    AssertLevel::Fatal => 2,
                },
            )?;
            self.write_string(&assertion.message)?;
        }
        Ok(())
    }

    fn write_link_time_expr(
        &mut self,
        link_time_expr: &LinkTimeExpr,
        type_byte: u8,
    ) -> std::io::Result<()> {
        let Span::Normal(span) = &link_time_expr.span else {
            unreachable!("link-time expr not defined normally!?")
        };
        let (node_id, line_no) = self.resolve_span(span);
        self.write_long(node_id)?;
        self.write_long(line_no)?;

        self.write_long(link_time_expr.offset as u32)?;

        match link_time_expr.pc {
            Some((sect_id, offset)) => {
                self.write_long(sect_id as u32)?;
                self.write_long(offset as u32)?;
            }
            None => {
                self.write_long(u32::MAX)?;
                self.write_long(Default::default())?;
            }
        }

        self.write_byte(type_byte)?;

        self.write_expr(&link_time_expr.expr)
    }

    fn write_expr(&mut self, expr: &Expr) -> std::io::Result<()> {
        let len = expr.ops().fold(0, |len, op| {
            len + match &op.kind {
                OpKind::Number(_) => 5,
                OpKind::Symbol(_) => 5,
                &OpKind::BankOfSym(ident) => {
                    if ident == Symbols::pc_ident() {
                        1
                    } else {
                        5
                    }
                }
                OpKind::BankOfSect(name) | OpKind::StartOfSect(name) | OpKind::SizeOfSect(name) => {
                    2 + name.len()
                }
                OpKind::StartOfRegion(_) | OpKind::SizeOfRegion(_) => 2,
                OpKind::Binary(_) => 1,
                OpKind::Unary(operator) => {
                    if matches!(operator, UnOp::Identity) {
                        0
                    } else {
                        1
                    }
                }
                OpKind::Low => 1,
                OpKind::High => 1,
                OpKind::Bitwidth => 1,
                OpKind::Tzcount => 1,
                OpKind::Rst => 1,
                OpKind::Ldh => 1,
                OpKind::BitCheck(_) => 2,
                OpKind::Nothing => unreachable!("empty op at emission time!?"),
            }
        });
        self.write_long(len.try_into().expect("overly long RPN expr"))?;

        for op in expr.ops() {
            match &op.kind {
                &OpKind::Number(number) => {
                    self.write_byte(0x80)?;
                    self.write_long(number as u32)?;
                }
                &OpKind::Symbol(ident) => {
                    self.write_byte(0x81)?;
                    self.write_long(if ident == Symbols::pc_ident() {
                        u32::MAX
                    } else {
                        self.registered_symbols
                            .get_index_of(&ident)
                            .expect("non-registered symbol in expr!?")
                            as u32
                    })?;
                }
                &OpKind::BankOfSym(ident) => {
                    if ident == Symbols::pc_ident() {
                        self.write_byte(0x52)?;
                    } else {
                        self.write_byte(0x50)?;
                        self.write_long(
                            self.registered_symbols
                                .get_index_of(&ident)
                                .expect("non-registered symbol in bank expr!?")
                                as u32,
                        )?;
                    }
                }
                OpKind::BankOfSect(name) => {
                    self.write_byte(0x51)?;
                    self.write_string(name)?;
                }
                OpKind::SizeOfSect(name) => {
                    self.write_byte(0x53)?;
                    self.write_string(name)?;
                }
                OpKind::StartOfSect(name) => {
                    self.write_byte(0x54)?;
                    self.write_string(name)?;
                }
                &OpKind::SizeOfRegion(region) => {
                    self.write_byte(0x55)?;
                    self.write_byte(serialise_region(region))?;
                }
                &OpKind::StartOfRegion(region) => {
                    self.write_byte(0x56)?;
                    self.write_byte(serialise_region(region))?;
                }
                OpKind::Binary(operator) => self.write_byte(match operator {
                    BinOp::LogicalOr => 0x22,
                    BinOp::LogicalAnd => 0x21,
                    BinOp::NotEqual => 0x31,
                    BinOp::Equal => 0x30,
                    BinOp::LessEq => 0x35,
                    BinOp::Less => 0x33,
                    BinOp::GreaterEq => 0x34,
                    BinOp::Greater => 0x32,
                    BinOp::Add => 0x00,
                    BinOp::Subtract => 0x01,
                    BinOp::And => 0x11,
                    BinOp::Or => 0x10,
                    BinOp::Xor => 0x12,
                    BinOp::LeftShift => 0x40,
                    BinOp::RightShift => 0x41,
                    BinOp::UnsignedRightShift => 0x42,
                    BinOp::Multiply => 0x02,
                    BinOp::Divide => 0x03,
                    BinOp::Modulo => 0x04,
                    BinOp::Exponent => 0x06,
                })?,
                OpKind::Unary(operator) => match operator {
                    UnOp::Complement => self.write_byte(0x13)?,
                    UnOp::Identity => {}
                    UnOp::Negation => self.write_byte(0x05)?,
                    UnOp::Not => self.write_byte(0x23)?,
                },
                OpKind::High => self.write_byte(0x70)?,
                OpKind::Low => self.write_byte(0x71)?,
                OpKind::Bitwidth => self.write_byte(0x72)?,
                OpKind::Tzcount => self.write_byte(0x73)?,
                OpKind::Rst => self.write_byte(0x61)?,
                OpKind::Ldh => self.write_byte(0x60)?,
                &OpKind::BitCheck(mask) => {
                    self.write_byte(0x62)?;
                    self.write_byte(mask)?;
                }
                OpKind::Nothing => unreachable!("empty op when encoding RPN!?"),
            }
        }

        Ok(())
    }
}

fn serialise_region(mem_region: MemRegion) -> u8 {
    match mem_region {
        MemRegion::Wram0 => 0,
        MemRegion::Vram => 1,
        MemRegion::Romx => 2,
        MemRegion::Rom0 => 3,
        MemRegion::Hram => 4,
        MemRegion::Wramx => 5,
        MemRegion::Sram => 6,
        MemRegion::Oam => 7,
    }
}
