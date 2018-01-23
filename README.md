# Haskell WebAssembly Toolkit

## Goals
  * Provide tool WebAssembly code generation for Haskell
  * Create infrastructure for Cmm to WebAssembly code generator
  * Have Fun :)

## Status
  * Lexer: supports nested block comments and all lexemes from Spec
  * Parser: parses all examples from WebAssembly Core TestsSuit

## ToDo
  * Improve error messages for text representation parsing
  * Renaming Phase: substitute identifiers with correct indexes, expand all implicit type declarations)
  * Verification Phase: execute verification procedure from Spec
  * Text Representation pretty-printer
  * Binary format parser/serializer
  * Execution Phase: implement simple interpreter for running WebAssembly Core TestsSuit assertions
