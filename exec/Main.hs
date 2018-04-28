{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Parser as Parser
import qualified Language.Wasm.Binary as Binary

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
    putStrLn "There is no WASM!"
