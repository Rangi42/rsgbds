# rsgbasm parser

This directory contains the grammar of `rgbasm`'s assembly language, written as a manual recursive-descent parser.

## Why not use a parser generator?

When RGBDS was written in C(++), its language grammar was written in Bison (YACC) with no conflicts, which should imply that the language is LR(1).

But, in fact, it never has been, for various reasons.

### Expansions

Expansions (which collectively refers to macro arguments `\1` and interpolations `{example}`) are performed at a textual level; this is required to implement their semantics, particularly the implicit token pasting.
(Trying to handle it manually within each token's lexing has proven extremely error-prone and impractical.)

This means that the lexer has to maintain awareness of the active macro arguments, and of the active symbols.
This could be solved using `RefCell`, but this also makes handling symbols and macro args impractical, due to all of the added `let symbols = symbols.borrow()`s.

(Note that the lexer would also need a `RefCell` for the identifier interner.)

### The lexer modes

RGBASM's lexer has five “modes”: normal, raw, and three “skip” modes.
They are necessary, because some parts of the language do not lex normally under the “normal” mode: macro arguments can be character soup (for example, because they will be expanded inside of a string, which has different lexing rules; or they can never be expanded at all, which causes them to be ignored entirely); and skipped conditional blocks are ignored as well.

The latter is maybe not so great in hindsight, but it is how the language was designed, and it would be painful for users if this decision was reversed, particularly macro args.

Thing is, switching lexer modes requires cooperation between the lexer and the parser to happen at the right time, on top of yet more state that must be encapsulated in interior mutability.

### Capture blocks

Simiarly to the lexer modes, some “blocks” need to be skipped over without any lexing whatsoever: macro definitions and loops.
The former because a macro that's never called can contain any syntax error (notably, because macro args don't respect tokenisation, it's not possible to lex a macro definition in advance and “fill in the blanks” when it's expanded), and the latter because the same is true of a loop that's never executed (e.g. `REPT 0`).

### Conclusion

The very design of `rgbasm`'s input language requires tight coupling of the lexer and parser, which no parser generator is, to my knowledge, amenable to.
Therefore, after at least two attempts using [LALRPOP] aborted, it was decided to implement the parser manually.

## Expression parser

The expression parser is implemented as a Pratt parser, based on [this article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).

[LALRPOP]: //lalrpop.github.io/lalrpop
