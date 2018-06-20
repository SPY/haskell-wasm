{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Maybe (fromMaybe)

import qualified Language.Wasm as Wasm

import Options.Applicative
import Data.Semigroup ((<>))

{-
wasm compile INPUT -o output -f js -f binary
wasm exec --script INPUT
wasm validate INPUT -f binary -f text
wasm link 
-}

data CompileOutFormat = OutJS | OutBinary | OutHTML deriving (Eq)
data ExecMod = Plain | Script deriving (Show, Eq)
data Input = InpText | InpBinary deriving (Eq)

instance Read CompileOutFormat where
  readsPrec _ "js" = [(OutJS, "")]
  readsPrec _ "binary" = [(OutBinary, "")]
  readsPrec _ "html" = [(OutHTML, "")]
  readsPrec _ _ = error "Unknown compilation output foramt"

instance Show CompileOutFormat where
  show OutJS = "js"
  show OutBinary = "binary"
  show OutHTML = "html"

instance Read Input where
  readsPrec _ "text" = [(InpText, "")]
  readsPrec _ "binary" = [(InpBinary, "")]
  readsPrec _ _ = error "Unknown validation input foramt"

instance Show Input where
  show InpText = "text"
  show InpBinary = "binary"

data WasmCommand
  = Compile {
    input :: String,
    output :: String,
    outFormat :: CompileOutFormat
  }
  | Exec {
    input :: String,
    mode :: ExecMod,
    inpFormat :: Input
  }
  | Validate {
    input :: String,
    inpFormat :: Input
  }
  deriving (Show, Eq)

compileArgs = Compile
  <$> argument str (metavar "FILE")
  <*> strOption (
      long "out"
      <> short 'o'
      <> metavar "FILE"
      <> help "Distination for compilation"
    )
  <*> option auto (
      long "format"
      <> short 'f'
      <> help "Output file format"
      <> showDefault
      <> value OutBinary
    )

execArgs = Exec
  <$> argument str (metavar "FILE")
  <*> flag Plain Script (long "script" <> help "Execute file in script mode")
  <*> option auto (
    long "format"
    <> short 'f'
    <> help "Input file format"
    <> showDefault
    <> value InpText
  )

validateArgs = Validate
  <$> argument str (metavar "FILE")
  <*> option auto (
      long "format"
      <> short 'f'
      <> help "Input file format"
      <> showDefault
      <> value InpText
    )

config :: Parser WasmCommand
config = subparser (
    command "compile" (info compileArgs (progDesc "Compile WebAssembly file from text representation"))
    <> command "exec" (info execArgs (progDesc "Compile WebAssembly file if needed and execute"))
    <> command "validate" (info validateArgs (progDesc "Validate WebAssembly file"))
  )

toBinary :: LBS.ByteString -> Either String LBS.ByteString
toBinary = fmap Wasm.encodeLazy . Wasm.parse

compileAs :: (LBS.ByteString -> LBS.ByteString) -> String -> String -> IO ()
compileAs transform input output = do
  content <- LBS.readFile input
  case toBinary content of
    Right binary ->
      LBS.writeFile output $ transform binary
    Left reason ->
      putStrLn $ "Cannot complie module: " ++ reason

binary :: LBS.ByteString -> LBS.ByteString
binary = id

js :: LBS.ByteString -> LBS.ByteString
js binary =
    let asBase64 = Base64.encode binary in
    LBS.concat [
      "const bytes = Uint8Array.from(atob('" <> asBase64 <> "'), c => c.charCodeAt(0));\n",
      "WebAssembly.instantiate(bytes, {}).then(res => console.log(res.instance))\n"
    ]

html :: LBS.ByteString -> LBS.ByteString
html binary =
    LBS.concat [
      "<script>\n",
      js binary,
      "</script>\n"
    ]

exec :: WasmCommand -> IO ()
exec (Compile inp out OutBinary) = compileAs binary inp out
exec (Compile inp out OutJS) = compileAs js inp out
exec (Compile inp out OutHTML) = compileAs html inp out
exec command = putStrLn $ "command is not implemented yet: " ++ show command

main :: IO ()
main = execParser opts >>= exec
  where
    opts = info (config <**> helper)
      (fullDesc
        <> progDesc "WebAssembly Toolkit"
        <> header "This tool can compile text representation to binary format, validate module in text or binary representation and so on.")