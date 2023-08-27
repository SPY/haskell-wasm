{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Directory as Directory

import qualified Data.ByteString.Lazy as LBS

import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Script as Script
import qualified Data.List as List

main :: IO ()
main = do
  files <-
    filter (not . List.isPrefixOf "simd") . filter (List.isSuffixOf ".wast")
      <$> Directory.listDirectory "tests/spec"
  -- let files = ["binary-leb128.wast"]
  scriptTestCases <- (`mapM` files) $ \file -> do
    test <- LBS.readFile ("tests/spec/" ++ file)
    return $ testCase file $ do
      case Wasm.parseScript test of
        Right script -> 
          Script.runScript (\msg assert -> assertFailure ("Failed assert: " ++ msg ++ ". Assert " ++ show assert)) script
        Left error ->
          assertFailure $ "Failed to parse with error: " ++ show error
  defaultMain $ testGroup "Wasm Core Test Suit" scriptTestCases
