module Wasm.WasmParser where

import Options.Applicative
import Data.Semigroup ((<>))

--data WasmFileFormat = WasmText | WasmBinary | WasmScript deriving (Read, Show)
--
--instance Read WasmFileFormat where
--    readsPrec _ ("WAT":xs) = [ (WasmText, xs) ]
--    readsPrec _ ("WAST":xs) = [ (WasmScript, xs) ]
--    readsPrec _ ("WASM":xs) = [ (WasmBinary, xs) ]

data WasmParser = WasmParser
  { output     :: Maybe String
  , input      :: String }

sample :: Parser WasmParser
sample = WasmParser
      <$> option (maybeReader (Just . Just))
          ( long "output"
         <> short 'o'
         <> metavar "OUTPUT_FILE"
         <> help "Output filename"
         <> value Nothing )
      <*> argument str (metavar "INPUT_FILE")