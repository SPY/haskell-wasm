module Main (
  main
) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Wasm (something)

main :: IO ()
main = defaultMain $ testGroup "Test Suite" []
