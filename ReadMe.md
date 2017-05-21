## SLR(1) Parser Generator
- `special-symbols.lisp`
- `regex.lisp`: a simple regex implementation.
- `director.list`: use to generate SLR(1) direct table.
- `lexer.lisp`: lexer part
- `parser.lisp`: parser part
- `main`: a wrapper help to use this tool easily.

grammar_file
```
lex {
    id: "";
    ...
}

grammar id {
    id -> id id ...
       |  id id ...
       |  ... ;
   ...
}
```