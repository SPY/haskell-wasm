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

main :: IO ()
main = do
  files <- Directory.listDirectory "tests/samples"
  scriptTestCases <- (`mapM` files) $ \file -> do
    Right script <- Wasm.parseScript <$> LBS.readFile ("tests/samples/" ++ file)
    return $ testCase file $ do
      Script.runScript (\msg assert -> assertFailure ("Failed assert: " ++ msg ++ ". Assert " ++ show assert)) script
  defaultMain $ testGroup "Wasm Core Test Suit" scriptTestCases
