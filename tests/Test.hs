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
import qualified Language.Wasm.Binary as Binary

import qualified Debug.Trace as Debug

isRight :: (Show b) => Either a b -> Bool
isRight (Right x) = x `seq` True
isRight _         = False

compile :: String -> IO ()
compile file = do
  content <- LBS.readFile $ "tests/samples/" ++ file
  let Right mod = Parser.parseModule <$> Lexer.scanner content
  LBS.writeFile ("tests/runnable/" ++ file) $ Binary.dumpModuleLazy mod
  -- to run: python -m SimpleHTTPServer 8081 && open http://localhost:8081/tests/runnable

main :: IO ()
main = do
  files <- Directory.listDirectory "tests/samples"
  compile "fact.wast"
  syntaxTestCases <- (`mapM` files) $ \file -> do
    content <- LBS.readFile $ "tests/samples/" ++ file
    let result = Parser.parseModule <$> Lexer.scanner content
    return $ testCase ("Parse module from core Test Suit: " ++ file) $
      assertBool "Module parsed" $ isRight result
  binaryTestCases <- (`mapM` files) $ \file -> do
    content <- LBS.readFile $ "tests/samples/" ++ file
    let Right mod = Parser.parseModule <$> Lexer.scanner content
    let Right mod' = Binary.decodeModuleLazy $ Binary.dumpModuleLazy mod
    return $ testCase ("Dump module to binary and parse back: " ++ file) $
      assertBool "Module matched" $ mod == mod'
  defaultMain $ testGroup "tests" [
      testGroup "Syntax parsing" syntaxTestCases,
      testGroup "Binary format" binaryTestCases
    ]
