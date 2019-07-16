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
  * [x] Execution Phase: implement a simple interpreter
  * [x] Support extended Core Test Suit assertion grammar
  * [X] Compile Core Tests to Tasty test cases and pass all tests

## Todo
  * [ ] Improve error messages for text representation parsing
  * [ ] Text Representation pretty-printer
  * [ ] Command line tool for calling interpreter/compiler/validator
  * [ ] Codegen interface for type enforced generating valid WASM code

## Development
Clond sources to directory and use `stack` for running tests:
```
stack build && stack test
```
