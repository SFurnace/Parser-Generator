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

## Example
### Files
grammar file: expression.txt
```
lex {
  *blank : "[\t\r\b ]+" ;
  split  : "\n" ;
  add    : "(?<=[0-9\)][\t\r\b ]*)\+" ;
  sub    : "(?<=[0-9\)][\t\r\b ]*)-" ;
  mul    : "\*" ;
  div    : "/" ;
  left   : "\(" ;
  right  : "\)" ;
  num    : "[\+-]?[0-9]+(\.[0-9]+)?" ;
}

grammar exps {
  exps    -> exp *split exps
          |  exp *split 
          |  exp ;
  exp     -> term add exp
          |  term sub exp
          |  term ;
  term    -> factor mul term
          |  factor div term
          |  factor ;
  factor  -> num
          |  left exp right ;
}
```

test file: e0.txt
```
11
-12
1+1
1+1*3/2
-1+1*(12)
(3+4)-5--5+3
```

### Usage
./slr1.ros expression.txt e0.txt
```
└── exps
    ├── exp
    │   └── term
    │       └── factor
    │           └── num
    └── exps
        ├── exp
        │   └── term
        │       └── factor
        │           └── num
        └── exps
            ├── exp
            │   ├── term
            │   │   └── factor
            │   │       └── num
            │   ├── add
            │   └── exp
            │       └── term
            │           └── factor
            │               └── num
            └── exps
                ├── exp
                │   ├── term
                │   │   └── factor
                │   │       └── num
                │   ├── add
                │   └── exp
                │       └── term
                │           ├── factor
                │           │   └── num
                │           ├── mul
                │           └── term
                │               ├── factor
                │               │   └── num
                │               ├── div
                │               └── term
                │                   └── factor
                │                       └── num
                └── exps
                    ├── exp
                    │   ├── term
                    │   │   └── factor
                    │   │       └── num
                    │   ├── add
                    │   └── exp
                    │       └── term
                    │           ├── factor
                    │           │   └── num
                    │           ├── mul
                    │           └── term
                    │               └── factor
                    │                   ├── left
                    │                   ├── exp
                    │                   │   └── term
                    │                   │       └── factor
                    │                   │           └── num
                    │                   └── right
                    └── exps
                        └── exp
                            ├── term
                            │   └── factor
                            │       ├── left
                            │       ├── exp
                            │       │   ├── term
                            │       │   │   └── factor
                            │       │   │       └── num
                            │       │   ├── add
                            │       │   └── exp
                            │       │       └── term
                            │       │           └── factor
                            │       │               └── num
                            │       └── right
                            ├── sub
                            └── exp
                                ├── term
                                │   └── factor
                                │       └── num
                                ├── sub
                                └── exp
                                    ├── term
                                    │   └── factor
                                    │       └── num
                                    ├── add
                                    └── exp
                                        └── term
                                            └── factor
                                                └── num
```
