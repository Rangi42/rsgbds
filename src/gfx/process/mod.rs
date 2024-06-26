/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    cmp::Ordering, collections::HashSet, fmt::Display, io::Write, num::NonZeroU16, path::Path,
};

use plumers::{image::Frame, prelude::*};
use sysexits::ExitCode;

use crate::{
    color_set::ColorSet,
    common::{
        dash_stdio::{Input, Output},
        diagnostics::ContentlessReport,
    },
    palette::Palette,
    rgb::{Opacity, Rgb, Rgba},
    InputSlice, Options, PalSpec,
};

mod optimized;
mod tile_data;
pub use tile_data::{TileData, TileMatchKind};
mod unoptimized;

pub(crate) fn process(
    input_path: &Path,
    options: &Options,
    pal_spec: Option<PalSpec>,
) -> Result<(), ExitCode> {
    let mut file = Input::new(input_path).map_err(|err| {
        Input::error(input_path, format!("Failed to open input image: {err}"))
            .finish()
            .eprint_();
        ExitCode::NoInput
    })?;
    let image = DynImage32::load(
        plumers::image::Input(&mut file),
        LoadFlags {
            remove_alpha: false,
            palette_sort: Default::default(),
            sort_existing: false,
            reduce_palette: false,
        },
        AlphaMode::ZeroIsTransparent,
        false,
    )
    .map_err(|err| {
        file.error_in(format!("Couldn't load the input image: {err}"))
            .finish()
            .eprint_();
        ExitCode::DataErr
    })?;

    if image.nb_frames() != 1 {
        crate::build_warning()
            .with_message("The input image has multiple animation frames")
            .with_note("Only the first frame will be processed")
            .finish()
            .eprint_();
    }
    let frame = image.frame(0);
    let slice = match options.input_slice {
        Some(slice) => slice,
        None => {
            if image.width() % 8 != 0 || image.height() % 8 != 0 {
                crate::build_error().with_message(format!(
                    "The input image's dimensions ({}x{}) must each be a multiple of 8 pixels",
                    image.width(),
                    image.height(),
                )).with_note(
                    "If you want to only process a portion of it, try using the `-L` option instead",
                )
                .finish()
                .eprint_();
                return Err(ExitCode::DataErr);
            }

            // Panicking if the input image exceeds 524288 pixels in either dimension seems reasonable.
            let dimension_in_tiles = |pixels: usize, overflow_msg| {
                NonZeroU16::new((pixels / 8).try_into().expect(overflow_msg))
            };
            let (Some(width), Some(height)) = (
                dimension_in_tiles(image.width(), "Image width too large!"),
                dimension_in_tiles(image.height(), "Image height too large!"),
            ) else {
                crate::build_error()
                    .with_message("The input image cannot be empty")
                    .finish()
                    .eprint_();
                return Err(ExitCode::DataErr);
            };

            InputSlice {
                left: 0,
                top: 0,
                width,
                height,
            }
        }
    };

    let (image_colors, has_transparency) = collect_image_colors(&image, &slice, options)?;

    // This is done unconditionally because it validates the image (which we want to perform even
    // if no output is requested), and because it's necessary to generate any output (except an
    // un-dedup'd tilemap, but that's acceptable).
    let (color_sets, mut attrmap) = collect_color_sets(&frame, &slice, options, has_transparency)?;

    let (mappings, palettes) = match pal_spec {
        Some(PalSpec::Embedded) => {
            let pal = image.palette().ok_or_else(|| {
                crate::build_error()
                    .with_message(
                        "`-c embedded` is being used, but the image lacks an embedded palette",
                    )
                    .finish()
                    .eprint_();
                ExitCode::DataErr
            })?;
            make_palettes_as_specified(
                &[pal
                    .iter()
                    // Ignore any extraneous colors if they are unused.
                    // TODO: explicitly check that they are unused, to print a more friendly error if not
                    .take(options.colors_per_palette(has_transparency).into())
                    .copied()
                    .map(|color| Some(Rgba::from(color).cgb_color(options.use_color_curve)))
                    .collect()],
                &color_sets,
                has_transparency,
            )?
        }
        // TODO: this will generate duplicate colors... is that okay?
        Some(PalSpec::Explicit(spec)) => {
            make_palettes_as_specified(&spec, &color_sets, has_transparency)?
        }
        None => generate_palettes(&color_sets, options, has_transparency),
    };

    if let Some(path) = &options.palettes_path {
        output_palettes(&palettes, path, options)?;
    }

    // A lot of later operations depend on correct palette generation, so if the latter went wrong,
    // then they are likely to produce nonsensical results. So bail right now.
    if palettes.len() > options.nb_palettes.into() {
        crate::build_error()
            .with_message("Generated more palettes than the maximum")
            .with_note(format!(
                "Generated {} palettes, over the limit of {}",
                palettes.len(),
                options.nb_palettes
            ))
            .finish()
            .eprint_();
        return Err(ExitCode::DataErr);
    }

    // If there are more than 8 palettes, and attrmap is requested but not palmap, then warn.
    // Do not do this if there are too many palettes only because the limit was exceeded, though.
    if palettes.len() > 8 && options.attrmap_path.is_some() && options.palmap_path.is_none() {
        crate::build_warning()
            .with_message("More than 8 palettes were generated, but this cannot be reflected in the attribute map")
            .with_note(
                format!("{} palettes were generated", palettes.len())).with_help(
                "You can generate a palette map to get palette IDs up to 256")
                .finish()
                .eprint_();
    }

    if options.allow_dedup {
        // All of the "optimised" outputs require the deduplication process to be performed to be output.
        // (Except for the palette map, I guess.)
        let tile_data = optimized::generate_unique_tiles(
            &frame,
            &mut attrmap,
            &palettes,
            &mappings,
            &slice,
            options,
            has_transparency,
        );

        if let Some(path) = &options.output_path {
            optimized::output_tile_data(&tile_data, path, options)?;
        }

        if let Some(path) = &options.tilemap_path {
            optimized::output_tilemap(&attrmap, path)?;
        }

        if let Some(path) = &options.attrmap_path {
            optimized::output_attrmap(&attrmap, &mappings, path)?;
        }

        if let Some(path) = &options.palmap_path {
            optimized::output_palmap(&attrmap, &mappings, path)?;
        }
    } else {
        if let Some([bank0, bank1]) = &options.max_nb_tiles {
            let height_in_tiles = (image.height() / 8) as u32;
            let width_in_tiles = (image.width() / 8) as u32;
            let nb_tiles = height_in_tiles * width_in_tiles;

            // Check the tile count.
            if nb_tiles > (bank0 + bank1).into() {
                let nb_tiles_msg = format!("The image contains {nb_tiles} (unoptimized) tiles");
                crate::build_error()
                    .with_message("The image contains more tiles than the limit")
                    .with_note(nb_tiles_msg)
                    .finish()
                    .eprint_();
                return Err(ExitCode::DataErr);
            }
        }

        if let Some(path) = &options.output_path {
            unoptimized::output_tile_data(
                &frame,
                &attrmap,
                &palettes,
                &mappings,
                path,
                &slice,
                options,
                has_transparency,
            )?;
        }

        if options.tilemap_path.is_some()
            || options.attrmap_path.is_some()
            || options.palmap_path.is_some()
        {
            unoptimized::output_maps(&attrmap, &mappings, options)?;
        }
    }

    Ok(())
}

pub(crate) fn process_palettes_only(
    pal_specs: &[Vec<Option<Rgb16>>],
    path: &Path,
    options: &Options,
) -> Result<(), ExitCode> {
    let (_, palettes) = make_palettes_as_specified(pal_specs, &[], false)?;

    output_palettes(&palettes, path, options)?;

    if palettes.len() > options.nb_palettes.into() {
        crate::build_error()
            .with_message("Generated more palettes than the maximum")
            .with_note(format!(
                "Generated {} palettes, over the limit of {}",
                palettes.len(),
                options.nb_palettes
            ))
            .finish()
            .eprint_();
        return Err(ExitCode::DataErr);
    }

    Ok(())
}

/// Iterate through the image's pixels to check if any are transparent, and to build a table of
/// the image's colors.
fn collect_image_colors(
    image: &DynImage32,
    slice: &InputSlice,
    options: &Options,
) -> Result<(Vec<Rgb16>, bool), ExitCode> {
    let mut has_transparency = false;
    let mut cgb_colors: [_; 0x8000] = std::array::from_fn(|_i| None);
    let mut ambiguous_alpha_pos = Vec::new();
    let mut ambiguous_alpha = HashSet::new();

    for y in 0..usize::from(slice.height.get()) * 8 {
        for x in 0..usize::from(slice.width.get()) * 8 {
            let rgba = Rgba::from(image.pixel(0, x, y));
            match rgba.opacity() {
                None => {
                    ambiguous_alpha_pos.push((x, y));
                    ambiguous_alpha.insert(rgba);
                }

                Some(Opacity::Opaque) => {
                    let cgb_color = rgba.cgb_color(options.use_color_curve);
                    let rgb = Rgb::from(rgba);
                    // The array is correctly sized for all opaque colors, so the index is in bounds.
                    let slot = &mut cgb_colors[usize::from(cgb_color.0)];
                    match slot {
                        None => *slot = Some((rgb, Vec::new())),
                        Some((first, conflicting)) => {
                            // Remember and report different colors that map to the same GBC color.
                            // Perform a linear search, because `conflicting` is usually only a couple colors at worst.
                            if rgb != *first && conflicting.iter().all(|&member| rgb != member) {
                                conflicting.push(rgb);
                            }
                        }
                    }
                }

                Some(Opacity::Transparent) => has_transparency = true,
            }
        }
    }

    let mut ambiguous_colors = ambiguous_alpha.into_iter();
    if let Some(first) = ambiguous_colors.next() {
        let offending_colors = if ambiguous_colors.len() == 0 {
            format!("Offending color: {first}")
        } else {
            // TODO: generate an image showing where the offending pixels are located.

            let mut msg = format!("Offending colors: {first}");
            while let Some(color) = ambiguous_colors.next() {
                use std::fmt::Write;

                msg.push_str(if ambiguous_colors.len() == 0 {
                    ", and "
                } else {
                    ", "
                });
                write!(msg, "{color}").unwrap();
            }
            msg
        };
        let acceptable_alpha = format!(
            "Acceptable alpha values are between 0 and #{:02x} ({}) for transparency, or between #{:02x} ({}) and #FF (255) for opaque pixels.",
            Rgba::TRANSPARENCY_THRESHOLD,
            Rgba::TRANSPARENCY_THRESHOLD,
            Rgba::OPACITY_THRESHOLD,
            Rgba::OPACITY_THRESHOLD,
        );
        crate::build_error()
            .with_message("Some colors have ambiguous transparency")
            .with_note(offending_colors)
            .with_help(acceptable_alpha)
            .finish()
            .eprint_();
        return Err(ExitCode::DataErr);
    }

    let mut image_colors = match image.palette() {
        Some(pal) => Vec::with_capacity(pal.len()),
        None => Vec::with_capacity(
            usize::from(options.nb_colors_per_pal.get()) * usize::from(options.nb_palettes),
        ),
    };
    for (cgb_color, slot) in cgb_colors.iter().enumerate() {
        let Some((first, conflicting)) = slot else {
            continue;
        };

        if !conflicting.is_empty() {
            crate::build_warning()
                .with_message(format!(
                    "Several colors in the image map to GBC color ${cgb_color:04x}"
                ))
                .with_note(format!("The colors are: {}", ColorList(first, conflicting)))
                .finish()
                .eprint_();
        }

        image_colors.push(Rgb16(cgb_color as u16));
    }
    Ok((image_colors, has_transparency))
}

fn collect_color_sets(
    frame: &Frame<'_, Rgb32, DynImage32>,
    slice: &InputSlice,
    options: &Options,
    has_transparency: bool,
) -> Result<(Vec<ColorSet>, Vec<AttrmapEntry>), ExitCode> {
    let colors_per_palette = options.colors_per_palette(has_transparency);

    let mut color_sets = Vec::new();
    let mut attrmap =
        Vec::with_capacity((frame.image().height() / 8) * (frame.image().width() / 8));

    'tiles: for tile in slice.iter_tiles(frame, options.column_major) {
        attrmap.push(AttrmapEntry::default());
        let attrs = attrmap.last_mut().unwrap();

        // This is tracked separately from the set's len, because a tile may (erroneously) contain more than 4 colors.
        let mut nb_colors_in_tile = 0;
        let mut tile_colors = ColorSet::new();

        for y in 0..8 {
            for x in 0..8 {
                let color = Rgba::from(tile.pixel(x, y));
                // Do not count transparent colors for packing.
                if color.opacity() == Some(Opacity::Opaque) {
                    // Add the color to the set, and count it if it wasn't a duplicate.
                    if tile_colors.add(color.cgb_color(options.use_color_curve)) {
                        nb_colors_in_tile += 1;
                    }
                }
            }
        }

        // Empty color sets screw with the packing process, so discard them.
        if tile_colors.is_empty() {
            attrs.color_set_id = AttrmapEntry::TRANSPARENT;
            continue;
        }

        if nb_colors_in_tile > colors_per_palette {
            crate::build_error().with_message(format!(
                "The tile at (x: {}, y: {}) has more opaque colors than the maximum ({colors_per_palette})",
                tile.x, tile.y,
            ))
            .finish()
            .eprint_();
            return Err(ExitCode::DataErr);
        }

        // Register the color set, avoiding overlap with existing ones.
        for (i, set) in color_sets.iter_mut().enumerate() {
            match tile_colors.partial_cmp(set) {
                // If neither set contains the other, keep seeking.
                None => {}
                // If this set is contained within an existing one, reuse the latter.
                Some(Ordering::Less | Ordering::Equal) => {
                    attrs.color_set_id = i;
                    continue 'tiles;
                }
                // If this set fully contains an existing one, "override" it.
                Some(Ordering::Greater) => {
                    *set = tile_colors;
                    attrs.color_set_id = i;
                    // TODO: could iterate the rest of the sets, to find if this set also fully
                    //       contains any others; then they could be removed.
                    //       But that would require updating all of the mappings, is that worth it?
                    continue 'tiles;
                }
            }
        }
        // The set needs to be added to the list.

        debug_assert_ne!(color_sets.len(), AttrmapEntry::TRANSPARENT); // Ensure IDs don't conflict.
        attrs.color_set_id = color_sets.len();
        color_sets.push(tile_colors);
    }

    Ok((color_sets, attrmap))
}

fn generate_palettes(
    color_sets: &[ColorSet],
    options: &Options,
    has_transparency: bool,
) -> (Vec<usize>, Vec<Palette>) {
    let (mappings, nb_palettes) =
        crate::pal_packing::pack_palettes(color_sets, options, has_transparency);

    let mut palettes = vec![Palette::new(has_transparency); nb_palettes];
    // Generate the actual palettes from the mappings.
    debug_assert_eq!(mappings.len(), color_sets.len());
    for (&mapping, color_set) in std::iter::zip(&mappings, color_sets) {
        for &color in color_set.iter() {
            palettes[mapping].add_color(color);
        }
    }

    // Sort colors in the generated palettes.
    for palette in &mut palettes {
        fn luminance(color: &Rgb16) -> u32 {
            let (red, green, blue, _alpha) = color.decode();
            2126 * u32::from(red) + 7152 * u32::from(green) + 722 * u32::from(blue)
        }
        palette.colors.sort_unstable_by_key(luminance);
    }

    (mappings, palettes)
}

fn make_palettes_as_specified(
    pal_specs: &[Vec<Option<Rgb16>>],
    color_sets: &[ColorSet],
    has_transparency: bool,
) -> Result<(Vec<usize>, Vec<Palette>), ExitCode> {
    // Convert the palette spec to actual palettes.
    let palettes = Vec::from_iter(pal_specs.iter().map(|spec| {
        let mut palette = Palette::new(has_transparency);
        for color in spec {
            palette.add_color(color.unwrap_or(Rgba::TRANSPARENT));
        }
        palette
    }));

    // Iterate through color sets, and try mapping them to the specified palettes.
    let mut misfits = Vec::new();
    let mappings = color_sets
        .iter()
        .map(|set| {
            match palettes.iter().position(|palette| {
                set.iter()
                    .all(|&color| palette.index_of(color, has_transparency).is_some())
            }) {
                Some(idx) => idx,
                None => {
                    misfits.push(set);
                    0 // Bogus value, will not really be used.
                }
            }
        })
        .collect();

    if misfits.is_empty() {
        Ok((mappings, palettes))
    } else {
        fn format_palettes(palettes: &[Palette]) -> String {
            use std::fmt::Write;

            const PREFIX: &str = "The following palettes were specified:";
            let mut pals_str = String::with_capacity(PREFIX.len() + palettes.len() * (4 * 7 + 5));
            pals_str.push_str(PREFIX);
            for palette in palettes {
                write!(pals_str, "\n  {palette}").unwrap();
            }
            pals_str
        }
        let mut note = format_palettes(&palettes);
        for set in misfits {
            use std::fmt::Write;
            write!(note, "\nNo palette contains colors {set}").unwrap()
        }
        crate::build_error()
            .with_message("Some tiles cannot be displayed with the specified palettes")
            .with_note(note)
            .finish()
            .eprint_();
        Err(ExitCode::DataErr)
    }
}

fn output_palettes(palettes: &[Palette], path: &Path, options: &Options) -> Result<(), ExitCode> {
    let mut output = Output::new(path).map_err(|err| {
        Output::error(path, format!("Failed to create palette file: {err}"))
            .finish()
            .eprint_();
        ExitCode::CantCreat
    })?;

    for palette in palettes {
        for i in 0..usize::from(options.nb_colors_per_pal.get()) {
            let cgb_color = match palette.colors.get(i) {
                Some(color) => color.0,
                None => 0xFFFF,
            };
            output.write_all(&cgb_color.to_le_bytes()).map_err(|err| {
                output
                    .error_in(format!("Failed to write palettes: {err}"))
                    .finish()
                    .eprint_();
                ExitCode::IoErr
            })?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct AttrmapEntry {
    color_set_id: usize,
    tile_id: u8,
    bank: bool,
    vert_flip: bool,
    horiz_flip: bool,
}

impl AttrmapEntry {
    /// For `color_set_id`.
    const TRANSPARENT: usize = usize::MAX;

    fn get_pal_id(&self, mappings: &[usize]) -> usize {
        match self.color_set_id {
            Self::TRANSPARENT => 0,
            id => mappings[id],
        }
    }
}

struct ColorList<'a>(&'a Rgb, &'a Vec<Rgb>);

impl Display for ColorList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_assert_ne!(self.1.len(), 0); // This guarantees that the "and" is preceded by a color.

        for color in self.1 {
            write!(f, "{color}, ")?;
        }
        write!(f, "and {}", self.0)
    }
}

impl InputSlice {
    fn iter_tiles<'frame, 'img: 'frame>(
        &self,
        frame: &'frame Frame<'img, Rgb32, DynImage32>,
        column_major: bool,
    ) -> TileIter<'frame, 'img, '_> {
        TileIter::new(frame, self, column_major)
    }
}

#[derive(Debug, Clone)]
struct Tile<'frame, 'img: 'frame> {
    frame: &'frame Frame<'img, Rgb32, DynImage32>,
    x: u16,
    y: u16,
}

#[derive(Debug, Clone)]
struct TileIter<'frame, 'img: 'frame, 'slice> {
    frame: &'frame Frame<'img, Rgb32, DynImage32>,
    slice: &'slice InputSlice,
    dx: u16,
    dy: u16,
    column_major: bool,
}

impl<'img> Tile<'_, 'img> {
    fn pixel(&self, x: u8, y: u8) -> Rgb32 {
        self.frame.pixel(
            usize::from(x) + usize::from(self.x),
            usize::from(y) + usize::from(self.y),
        )
    }
}

impl<'frame, 'img: 'frame, 'slice> TileIter<'frame, 'img, 'slice> {
    fn new(
        frame: &'frame Frame<'img, Rgb32, DynImage32>,
        slice: &'slice InputSlice,
        column_major: bool,
    ) -> Self {
        Self {
            frame,
            slice,
            dx: 0,
            dy: 0,
            column_major,
        }
    }
}

impl<'frame, 'img: 'frame> Iterator for TileIter<'frame, 'img, '_> {
    type Item = Tile<'frame, 'img>;

    fn next(&mut self) -> Option<Self::Item> {
        let (width, height) = (self.slice.width.get(), self.slice.height.get());
        let tile = Tile {
            frame: self.frame,
            x: self.dx * 8,
            y: self.dy * 8,
        };

        let coords = if self.column_major {
            (&mut self.dy, height, &mut self.dx, width)
        } else {
            (&mut self.dx, width, &mut self.dy, height)
        };

        if *coords.2 == coords.3 {
            return None;
        }

        *coords.0 += 1;
        if *coords.0 == coords.1 {
            *coords.0 = 0;
            *coords.2 += 1;
        }
        Some(tile)
    }
}
