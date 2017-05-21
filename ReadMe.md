## SLR(1) Parser Generator
- `special-symbols.lisp`
- `regex.lisp`: a simple regex implementation.
- `director.list`: use to generate SLR(1) direct table.
- `lexer.lisp`: lexer part
- `parser.lisp`: parser part
- `slr1.ros`: a wrapper help to use this tool easily.

the grammar_file can be written like below. Add a `*` before the identifier to indicate that it should be ignored in result.
```
lex {
    *ignored_id: "[\t\n\r\b ]+";
    id0: "a regex string";
    id1: "another regex string";
    ...
}

grammar G {
    G ->  id0 *id1 ...
       |  G id1 ...
       |  ... ;
   ...
}
```