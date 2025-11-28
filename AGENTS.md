# AGENTS.md

This file provides guidance to AI assistants when working with code in this repository.

## Project Overview

Tree-sitter grammar for NONMEM control files (.ctl, .mod). Enables syntax highlighting, code navigation, and structural analysis for NONMEM pharmacometric modeling files.

## Build Commands

```bash
# Generate parser from grammar
tree-sitter generate

# Run tests
tree-sitter test

# Run single test by name
tree-sitter test -i '<test-name>'

# Parse a file (for debugging)
tree-sitter parse <file.ctl>

# Interactive playground
tree-sitter playground
```

## Architecture

- `grammar.js` - Grammar definition (main development file)
- `src/` - Generated parser code (don't edit manually)
- `test/corpus/` - Test cases in tree-sitter's test format
- `queries/` - Syntax highlighting and other queries

## Tree-sitter Test Format

Tests in `test/corpus/*.txt` use this format:
```
==================
Test name
==================

input code here

---

(expected_tree)
```

## NONMEM Control File Structure

Key sections to parse: $PROBLEM, $INPUT, $DATA, $SUBROUTINES, $PK, $ERROR, $THETA, $OMEGA, $SIGMA, $ESTIMATION, $COVARIANCE, $TABLE

## Development Workflow

When fixing grammar issues or adding features:

1. **Identify the issue** - Parse example files to find errors
2. **Add test case first** - Add failing test to `test/corpus/` before fixing
3. **Fix grammar.js** - Make minimal changes to fix the issue
4. **Verify tests pass** - Run `tree-sitter generate && tree-sitter test`
5. **Check examples** - Verify all examples parse without errors
6. **Commit incrementally** - Commit related fixes together with tests

## Debugging Approaches

### Finding errors in examples
```bash
# Count total errors
tree-sitter parse examples/*.ctl 2>&1 | grep -c ERROR

# See specific error locations
tree-sitter parse <file> 2>&1 | grep "ERROR \["
```

### Investigating specific errors
```bash
# View line with error (tree-sitter uses 0-indexed lines)
sed -n '<line+1>p' <file>

# Check for special characters
sed -n '<line>p' <file> | od -c

# Test specific code snippet
echo '$PK
X = Y' | tree-sitter parse /dev/stdin
```

### Common fix patterns
- **Missing abbreviations**: Add `S?` for optional plural (`MAXEVALS?`)
- **Missing keywords**: Add to choice() or pattern alternatives
- **New operators**: Add to binary_expression with proper precedence
- **New record types**: Add to _record choice and define rule
- **Support multiple indices**: Use `repeat(seq(',', $._expression))`
