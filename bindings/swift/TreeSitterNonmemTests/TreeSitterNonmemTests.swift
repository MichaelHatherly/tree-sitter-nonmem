import XCTest
import SwiftTreeSitter
import TreeSitterNonmem

final class TreeSitterNonmemTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_nonmem())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading NONMEM grammar")
    }
}
