module Wasm.WasmParser where

import Options.Applicative
import Data.Semigroup ((<>))

data WasmFileFormat = WasmText | WasmBinary | WasmScript deriving (Read, Show)

instance Read WasmFileFormat where
    readsPrec _ ("WAT":xs) = [ (WasmText, xs) ]
    readsPrec _ ("WAST":xs) = [ (WasmScript, xs) ]
    readsPrec _ ("WASM":xs) = [ (WasmBinary, xs) ]

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