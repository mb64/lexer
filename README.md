# A simple lexer generator

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Generate DFA-driven lexers from a regex spec. Currently generates C, but the
goal is to also support Zig in the future (a more difficult task due to the
lack of `goto`s).

Example grammar:

```
// This is a comment
// Line syntax: TOKEN  regex
T_IF        if
T_FOR       for
T_WHILE     while
T_INTLIT    [0-9][0-9_]*

// Skip over some input using --
// These lex C-style comments
--          //[^\n]*\n
--          /\*([^\*]|\*+[^\*/])*\*+/
// Skip whitespace
// Regexes are trimmed of whitespace. If you need to start with a space, use [ ]
--          [ \t\r\n]

// Tokens are picked first by length of match, and second by their order in the
// file.  So even though this matches 'if', T_IF will be output preferentially.
// However, 'iff' will be T_IDENT since 'iff' is a longer match than 'if'.
T_IDENT     [a-zA-Z][a-zA-Z0-9_]*
```

Generate C with `cabal run -- lexer -c my_grammar.txt`.

Generate GraphViz for the DFA with `cabal run -- lexer -graphviz my_grammar.txt`.
