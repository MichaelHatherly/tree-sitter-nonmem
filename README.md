# tree-sitter-nonmem

[![CI][ci]](https://github.com/MichaelHatherly/tree-sitter-nonmem/actions/workflows/ci.yml)

NONMEM control file grammar for [tree-sitter][].

Supports parsing of `.ctl` and `.mod` files used in pharmacometric modeling with NONMEM.

[tree-sitter]: https://github.com/tree-sitter/tree-sitter

## Features

- Full support for NONMEM control records (`$PROBLEM`, `$DATA`, `$INPUT`, `$PK`, `$ERROR`, etc.)
- Parameter blocks (`$THETA`, `$OMEGA`, `$SIGMA`)
- Estimation and covariance options
- Embedded Fortran code with control flow (`IF`/`THEN`/`ELSE`, `DO` loops)
- Expression parsing with proper operator precedence

## Development

### Building

```sh
tree-sitter generate
```

### Testing

```sh
tree-sitter test
```

### Parsing files

```sh
tree-sitter parse example.ctl
```

[ci]: https://img.shields.io/github/actions/workflow/status/MichaelHatherly/tree-sitter-nonmem/ci.yml?logo=github&label=CI
