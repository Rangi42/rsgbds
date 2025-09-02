# rgbasm architecture

This document aims to describe the structure of rgbasm, and to help new contributors to the codebase.

## Overview

The “top-most” file is `main.rs`, but it may be tiresome to try and follow the logic from its `main` function.

In a nutshell:

1. command-line arguments are processed via the `cli` module;
2. files are processed by first being loaded into a `Source` (from `sources.rs`), which is then processed by the `syntax` module, which calls back into other top-level modules;
3. and finally, the data is written out via the `obj_file` module.

Calls are made throughout the process to the `diagnostics` module, which performs error reporting.

Top-level modules, except for those mentioned above, try to implement one “aspect” of rgbasm's processing.
(In particular, the `section` module is complex enough that it has its own sub-modules.)

### `syntax` module

Processing of the input files is split in two stages: a lexer and a parser.
(Essentially, a lexer is responsible for the equivalent of grouping characters into words, and a parser extracts meaning out of the stream of words.)

The lexer is written by hand, but the parser is generated using [LALRPOP].
The language's grammar thus obeys (roughly) [LR(1)] semantics and limitations.

Since the LALRPOP file is out of reach of most tooling (e.g. `rustfmt`, rust-analyzer...), and to avoid “diluting” the syntax definition proper, it is made to contain as little code as possible.
Instead, any “glue” code necessary to bridge parser results and top-level modules' APIs is written in the `semantics` sub-module, as methods on `ParseCtx`.
Notably, `ParseCtx` does not escape the `syntax` module.

To add new syntax to rgbasm, you will want to modify files in this directory; it contains a `CONTRIBUTING` document explaining some common workflows.

[LALRPOP]: https://lalrpop.github.io/lalrpop/
[LR(1)]: https://en.wikipedia.org/wiki/Canonical_LR_parser

## Salient points

- Traditionally, the whole file would be parsed in a single call to the parser; however, since rgbasm's lexer is context-sensitive, this would require some cooperation between the lexer and parser so that the mode is switched at the right time.
  Instead, this implementation chooses to lex an entire “logical” line, then parse it, which can modify the lexer's state for the following line.

  The use of the word “logical” is because such a line can span more than one “physical” line, for example in the presence of line continuations.

- The `warnings` directory contains files from which warning information is generated.
  Each file maps to a warning, and each sub-directory maps to a parametric warning; the names should be self-descriptive.
  `.mdoc` files are [mandoc]-formatted documentation of each warning (or warning level), with the first line being a mandoc comment that the build script processes into warning information, such as whether the warning is enabled by default.

[mandoc]: https://mandoc.bsd.lv
