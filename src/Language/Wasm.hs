module Language.Wasm (
    Module,
    ValidModule,
    ValidationError(..),
    parse,
    validate,
    Language.Wasm.parseScript,
    encode,
    encodeLazy,
    decode,
    decodeLazy,
    Script,
    runScript
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Language.Wasm.Structure as Struct
import Language.Wasm.Script as Script
import Language.Wasm.Lexer as Lexer
import Language.Wasm.Parser as Parser
import Language.Wasm.Validate as Valid
import Language.Wasm.Binary as Binary

-- | Parse WebAssembly text representation to `Module`
parse :: LBS.ByteString -> Either String Module
parse content = Lexer.scanner content >>= Parser.parseModule

-- | Parse WebAssembly extended scipt grammar
parseScript :: LBS.ByteString -> Either String Script
parseScript content = Lexer.scanner content >>= Parser.parseScript

-- | Dump `Module` to binary representation
encode :: Module -> BS.ByteString
encode = dumpModule

-- | Dump `Module` to binary representation lazily
encodeLazy :: Module -> LBS.ByteString
encodeLazy = dumpModuleLazy

-- | Decode `Module` from binary representation
decode :: BS.ByteString -> Either String Module
decode = decodeModule

-- | Decode `Module` from binary representation lazily
decodeLazy :: LBS.ByteString -> Either String Module
decodeLazy = decodeModuleLazy
