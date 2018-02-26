# Haskell WebAssembly Toolkit

## Goals
  * Provide a WebAssembly code generation tool for Haskell
  * Create an infrastructure for Cmm to WebAssembly code generator
  * Have Fun :)

## Status
  * [x] Lexer: supports nested block comments and all lexemes from the WebAssembly Spec
  * [x] Parser: parses all examples from WebAssembly Core TestsSuit (including folded instructions parsing)
  * [x] Renaming Phase: substitute identifiers with correct indexes, expand all implicit type declarations)
  * [x] Binary format parser/serializer
  * [x] Validation Phase: execute a verification procedure from the Spec

## Todo
  * [ ] Improve error messages for text representation parsing
  * [ ] Text Representation pretty-printer
  * [ ] Execution Phase: implement a simple interpreter for running WebAssembly Core TestsSuit assertions
