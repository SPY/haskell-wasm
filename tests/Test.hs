module Main (
  main
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Parser as Parser

main :: IO ()
main = do
  file <- LBS.readFile "tests/samples/switch.wast"
  print $ Parser.parseModule <$> Lexer.scanner file
  defaultMain $ testGroup "Test Suite" []
