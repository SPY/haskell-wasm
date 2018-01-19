module Main (
  main
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm.Lexer as Lexer

main :: IO ()
main = do
  file <- LBS.readFile "tests/samples/f64.wast"
  print $ map Lexer.tok <$> Lexer.scanner file
  defaultMain $ testGroup "Test Suite" []
