module Main (
  main
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Directory as Directory

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Parser as Parser
import qualified Language.Wasm.Structure as Structure

import qualified Debug.Trace as Debug

instance NFData Structure.Module

isRight :: (NFData b, Show b) => Either a b -> Bool
isRight (Right x) = x `seq` True
isRight _         = False

main :: IO ()
main = do
  files <- Directory.listDirectory "tests/samples"
  testCases <- (`mapM` files) $ \file -> do
    content <- LBS.readFile $ "tests/samples/" ++ file
    let result = Parser.parseModule <$> Lexer.scanner content
    return $ testCase ("Parse module from core Test Suit: " ++ file) $
      assertBool "Module parsed" $ isRight result
  defaultMain $ testGroup "Syntax parsing" testCases
