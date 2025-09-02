# Changing rgbasm's grammar

This is the most common change that one will want to make to rgbasm.
But there are several flavours of it.

By the way, please remember to update the language documentation to describe your changes!

## Working with existing tokens

The file you will be most interested in is `parser.lalrpop`; consider reading [the LALRPOP documentation][LALRPOP] to get an idea of the syntax.

Please try to keep the Rust code in the LALRPOP file to one-liners, and moving more involved logic to the `semantics` module, so that it can be auto-formatted, is accessible to `rust-analyzer`, etc.

Some gotchas:

- LALRPOP doesn't allow productions (rules) that match no tokens, so when you want something to be optional, you have to mark it as optional (using `?`) in its caller, and handle the resulting `Option<T>`.
  Using `.unwrap_or_default()` is preferrable when possible.
- The set of infix operators used by numeric and string expressions must be disjoint.
  Otherwise, an ugly grammar conflict surfaces, because an identifier can be part of either[^ident_kind].
- Factoring out parts of the grammar can generate conflicts in certain circumstances; if there is no other way to solve them, consider using the `#[inline]` attribute (which I believe is currently not documented).

[^ident_kind]: The lexer could return a different token type if the identifier corresponds to a string symbol, but that would create strange syntax error messages if the user e.g. forgot to define a string symbol before its use. Overcoming this limitation would be a welcome improvement.

[LALRPOP]: https://lalrpop.github.io/lalrpop/

## Adding a new token

Suppose you want to introduce all-new syntax.
In addition to the above, you will also need to add a new token, which is done in `tokens.rs`.

### Keyword

Keep in mind that introducing new keywords can break existing code due to keywords sharing their namespace with identifiers, so try to use existing keywords if possible.

It is easy to add a new keyword, because they are automatically processed.
Simply follow the syntax for the existing ones.

### Other

This requires modifying the `next_token` function in `lexer.rs`.
In a nutshell, this function looks at the following character using `peek`, and chooses to make it part of the current token using `consume`.
(No, there is not more than 1 character of lookahead.)
