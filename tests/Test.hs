{-# LANGUAGE OverloadedStrings #-}
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
import qualified Language.Wasm.Validate as Validate
import qualified Language.Wasm.Interpreter as Interpreter

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
  -- let files = ["call_indirect.wast"]
  -- compile "fact.wast"
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
      assertEqual "Module matched" mod mod'
  validationTestCases <- (`mapM` files) $ \file -> do
    content <- LBS.readFile $ "tests/samples/" ++ file
    let Right mod = Parser.parseModule <$> Lexer.scanner content
    return $ testCase ("Validate module: " ++ file) $
      case file of
        "import.wast" ->
          assertEqual "Too many tables" Validate.MoreThanOneTable $ Validate.validate mod
        _ ->
          assertBool "Module matched" $ Validate.isValid $ Validate.validate mod
  interpretFact <- do
    content <- LBS.readFile "tests/samples/fact.wast"
    let Right mod = Parser.parseModule <$> Lexer.scanner content
    (modInst, store) <- Interpreter.instantiate Interpreter.emptyStore Interpreter.emptyImports mod
    let fac = \n -> Interpreter.invokeExport store modInst "fac-opt" [Interpreter.VI64 n]
    fac3 <- fac 3
    fac5 <- fac 5
    fac8 <- fac 8
    return $ testCase "Interprete factorial" $ do
      assertEqual "Fact 3! == 120" [Interpreter.VI64 6] fac3
      assertEqual "Fact 5! == 120" [Interpreter.VI64 120] fac5
      assertEqual "Fact 8! == 40320" [Interpreter.VI64 40320] fac8
  defaultMain $ testGroup "tests" [
      testGroup "Syntax parsing" syntaxTestCases,
      testGroup "Binary format" binaryTestCases,
      testGroup "Validation" validationTestCases,
      testGroup "Interpretation" [interpretFact]
    ]
