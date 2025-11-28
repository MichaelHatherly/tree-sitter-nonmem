package tree_sitter_nonmem_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_nonmem "github.com/MichaelHatherly/tree-sitter-nonmem/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_nonmem.Language())
	if language == nil {
		t.Errorf("Error loading NONMEM grammar")
	}
}
