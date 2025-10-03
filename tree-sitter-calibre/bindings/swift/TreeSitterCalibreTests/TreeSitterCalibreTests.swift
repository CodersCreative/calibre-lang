import XCTest
import SwiftTreeSitter
import TreeSitterCalibre

final class TreeSitterCalibreTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_calibre())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Calibre grammar")
    }
}
