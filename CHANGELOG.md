# Changelog

## [Unreleased]

### Added
- Initial grammar for NONMEM control files
- Support for all major record types ($PROBLEM, $DATA, $INPUT, $PK, $ERROR, $DES, $PRED, etc.)
- Parameter records ($THETA, $OMEGA, $SIGMA) with bounds and options
- Estimation and covariance records with full option support
- Expression parsing with Fortran operators
- Control flow (IF/THEN/ELSE, DO loops)
- Function calls and parameter references (THETA, ETA, EPS, ERR)
- Syntax highlighting queries
- Language bindings for Node.js, Python, Rust, Go, Swift, C
