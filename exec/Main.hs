{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Parser as Parser
import qualified Language.Wasm.Binary as Binary

import Options.Applicative
import Data.Semigroup ((<>))
import Wasm.WasmParser

import Data.Maybe (fromMaybe)

data OutputMode = WasmBinary | JSWrapper deriving (Show, Eq)

data WasmConfig = WasmConfig {
    inputFile :: String,
    outputFile :: String,
    outputMode :: OutputMode
} deriving (Show, Eq)

compile :: String -> String -> IO ()
compile input output = do
  content <- LBS.readFile input
  case Lexer.scanner content >>= Parser.parseModule of
    Right mod ->
      LBS.writeFile output $ Binary.dumpModuleLazy mod
    Left reason ->
      putStrLn $ "Cannot complie module: " ++ reason

main :: IO ()
main = do
     config <- execParser opts
     process config
   where
     opts = info (sample <**> helper)
       ( fullDesc
      <> progDesc "Haskell WebAssembly Toolkit"
      <> header "Compile WebAssembly code into binary" )

process :: WasmParser -> IO ()

process (WasmParser (Just output) input) =
  compile input output

process (WasmParser Nothing input) =
  compile input "out.wasm"

process _ = return ()