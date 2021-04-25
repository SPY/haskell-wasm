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
  let groups = [
          ("Core Tests", "tests/spec"),
          ("Reference Types Proposal", "tests/spec/proposals/reference-types")
        ]
  testGroups <- (`mapM` groups) $ \(groupName, dir) -> do
    files <- filter (List.isSuffixOf ".wast") <$> Directory.listDirectory dir
    -- let files = ["const.wast"]
    scriptTestCases <- (`mapM` files) $ \file -> do
      test <- LBS.readFile (dir ++ "/" ++ file)
      return $ testCase file $ do
        case Wasm.parseScript test of
          Right script -> 
            Script.runScript (\msg assert -> assertFailure ("Failed assert: " ++ msg ++ ". Assert " ++ show assert)) script
          Left error ->
            assertFailure $ "Failed to parse with error: " ++ show error
    return $ testGroup groupName scriptTestCases
  defaultMain $ testGroup "Wasm Test Suit" testGroups
