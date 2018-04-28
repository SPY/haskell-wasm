module Wasm.WasmParser where

import Options.Applicative
import Data.Semigroup ((<>))

data WasmParser = WasmParser
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser WasmParser
sample = WasmParser
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )