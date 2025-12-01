# Contributing

## Development Setup

1. Install tree-sitter CLI:
   ```sh
   npm install -g tree-sitter-cli
   ```

2. Clone the repository:
   ```sh
   git clone https://github.com/MichaelHatherly/tree-sitter-nonmem
   cd tree-sitter-nonmem
   ```

3. Generate the parser:
   ```sh
   tree-sitter generate
   ```

## Making Changes

1. Edit `grammar.js` to modify the grammar
2. Run `tree-sitter generate` to regenerate the parser
3. Add tests in `test/corpus/` for new features
4. Run `tree-sitter test` to verify all tests pass

## Test Format

Tests use tree-sitter's test format:

```
==================
Test name
==================

input code here

---

(expected_ast)
```

## Pull Requests

- Include tests for new features or bug fixes
- Ensure all tests pass (`tree-sitter test`)
- Keep commits focused and atomic
- Use clear commit messages

## Releasing

```sh
tree-sitter version X.Y.Z
git commit -am "Release X.Y.Z"
git tag vX.Y.Z
git push --tags origin main
```

The tag push triggers the publish workflow which creates a GitHub release.

## Reporting Issues

Please include:
- NONMEM code that fails to parse
- Expected behavior
- Actual behavior or error message
